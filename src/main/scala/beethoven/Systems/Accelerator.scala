package beethoven.Systems

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import beethoven.Generation.DotGen
import beethoven.TLManagement.TLRWFilter
import beethoven.common._
import beethoven._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._

import scala.language.postfixOps

class BeethovenAcc(implicit p: Parameters) extends LazyModule {
  val configs = p(AcceleratorSystems)
  val name2Id = scala.collection.immutable.Map.from(configs.zipWithIndex.map(a => (a._1.name, a._2)))
  val requireInternalCmdRouting = configs.map(_.canIssueCoreCommandsTo).foldLeft(false)(_ || _.nonEmpty)

  val sysCmddN = DotGen.addNode("SysCmd")
  val sysRespN = DotGen.addNode("SysResp")

  val system_tups = name2Id.keys.map { name =>
    val id = name2Id(name)
    val config = configs(id)
    val pWithMap = p.alterPartial({
      case SystemName2IdMapKey => name2Id
    })
    (LazyModule(new BeethovenSystem(config, id, configs.flatMap(_.canIssueCoreCommandsTo).contains(name), this)(pWithMap)), id, config)
  }.toSeq


  system_tups.foreach{ a =>
      a._1.suggestName(a._3.name)
  }

  // tie together intra-core memory ports in one system to core/channel sps individually in another system
  system_tups.foreach { case (sys, _, _) =>
    sys.intraCoreMemMasters.foreach { case (mpo, nodes_per_core, mpi) =>
      val target = mpo.toSystem
      val targetSys = system_tups(name2Id(target))
      val assoc_endpoints = targetSys._1.intraCoreMemSlaveNodes.filter(_._1 == mpo.name).map(_._2)
      //      val assoc_endpoints = system_tups.filter(_._3.name == pparams.toSystem)(0)._1.cores.map(_._2.intraCoreMemSlaveNodes.filter(_._1 == pparams.toMemoryPort)(0)).map(_._2)
      nodes_per_core foreach { clients =>
        require(clients.length == assoc_endpoints.flatten.length, "sanity - if this doesn't work, somethign bad has happened")
        clients zip assoc_endpoints.flatten foreach { case (c, e) =>
          val buffer = TLBuffer()
          e := buffer
          buffer := c
        }
      }
    }
  }
  val systems = system_tups.map(_._1)

  // for each system that can issue commands, connect them to the target systems in the accelerator
  // Seq of tuple of (target name, src xbar)
  val sysNCmdSourceLookup =
    Map.from(system_tups.map { case (targetSys, _, sys_param) =>
      targetSys.incomingInternalCommandXbar match {
        case Some(manager) =>
          val targetName = sys_param.name
          // find all systems that send commands here
          (sys_param.name, systems.map { srcSys =>
            if (srcSys.outgoingCmdXBars.isDefinedAt(targetName)) {
              val src = srcSys.outgoingCmdXBars(targetName)
              manager := src
              srcSys.incomingInternalResponseHandlers(targetName)._2 := targetSys.outgoingInternalResponseXbar.get
              srcSys.nCores + 1
            } else 1
          }.sum)
        case None => (sys_param.name, 1)
      }
    })

  val reads = systems.flatMap(_.r_nodes)
  val writes = systems.flatMap(_.w_nodes)

  lazy val module = new BeethovenAccModule(this)
}

class BeethovenAccModule(outer: BeethovenAcc)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new RoCCCommand))
    val resp = Decoupled(new AccelRoccResponse)
  })

  val cmd = Queue(io.cmd)

  // route cmds between:
  val accCmd = Wire(Decoupled(new AccelRoccCommand)) //CUSTOM3, used for everything else
  accCmd <> cmd.map(AccelRoccCommand.fromRoccCommand)
  val system_id = accCmd.bits.inst.system_id
  accCmd.ready := false.B //base case

  outer.system_tups.foreach { a =>
    a._1.module.cmd_origin_dN
  }
  val systemSoftwareResps = outer.system_tups.filter(_._3.canReceiveSoftwareCommands) map { sys_tup =>
    val (sys_queue, sys_queue_dN) = (Module(new Queue(new AccelRoccCommand, entries = 1)),
      DotGen.addNode(f"${sys_tup._3.name}.cmdQ"))
    DotGen.addEdge(sys_queue_dN, sys_tup._1.module.cmdArbiter_dN)
    DotGen.addEdge(outer.sysCmddN, sys_queue_dN)
    val params = sys_tup._3
    val sys_idx = outer.name2Id(params.name)
    sys_queue.suggestName(params.name + "_command_queue")

    // enqueue commands from software
    sys_queue.io.enq.valid := accCmd.valid && sys_idx.U === system_id
    sys_queue.io.enq.bits := accCmd.bits
    when(system_id === sys_idx.U) {
      accCmd.ready := sys_queue.io.enq.ready
    }

    // dequeue
    sys_tup._1.module.sw_io.get.cmd <> sys_queue.io.deq

    // while we're flushing don't let anything get in
    (sys_tup._1.module.sw_io.get.resp, sys_tup._1.module.respQ_dN)
  }

  if (systemSoftwareResps.isEmpty) {
    io.resp.valid := false.B
    io.resp.bits := DontCare
  } else {
    val (respArbiter, respArbiter_dN) = (Module(new RRArbiter[AccelRoccResponse](new AccelRoccResponse, systemSoftwareResps.size)),
      DotGen.addNode(f"SystemRespArbiter"))
    respArbiter.io.in.zip(systemSoftwareResps).foreach { case (arbio, (sysio, r_dN)) =>
      // these commands are being routed back to the sw so they need to be full Rocc
      arbio <> sysio
      DotGen.addEdge(r_dN, respArbiter_dN)
    }
    respArbiter.io.out <> io.resp
    DotGen.addEdge(respArbiter_dN, outer.sysRespN)
  }
}

class BeethovenAccSystem(hasTLDMA: Boolean)(implicit p: Parameters) extends LazyModule {
  val nMemChannels = platform.extMem.nMemoryChannels

  val acc = LazyModule(new BeethovenAcc())

  val tldma = if (hasTLDMA) Some((TLIdentityNode(), DotGen.addNode("TLDMA", imaginary = true))) else None

  val (reads, writes) = tldma match {
    case Some(tldmanode) =>
      val filter = LazyModule(new TLRWFilter(
        TLSlavePortParameters.v1(
          managers = Seq(TLSlaveParameters.v1(
            address = (0 until nMemChannels).map(BeethovenTop.getAddressSet(_)),
            supportsGet = TransferSizes(platform.frontBusBeatBytes),
            supportsPutFull = TransferSizes(platform.frontBusBeatBytes),
            supportsAcquireB = TransferSizes.none,
          )), platform.frontBusBeatBytes, 0
        ),
        TLMasterPortParameters.v1(
          clients = Seq(TLMasterParameters.v1(
            "Splitter",
            IdRange(0, 1))))))
      val filterdN = DotGen.addNode("AXISplitFilter")
      filter.in_node := (tldmanode._1)
      (acc.reads ++ Seq((filter.read_out, filterdN)), acc.writes ++ Seq((filter.write_out, filterdN)))
    case None =>
      (acc.reads, acc.writes)
  }

  val Seq(r_mem, w_mem) = Seq(reads, writes).map { mems =>
    if (mems.nonEmpty) {
      val mem_endpoints = Seq.fill(nMemChannels)((TLIdentityNode(), DotGen.addNode("MemEP", imaginary = true)))
      val crossbarModule = LazyModule(new TLXbar())
      val xbardN = DotGen.addNode("Xbar")
      val crossbar = crossbarModule.node
      mems foreach { m =>
        crossbar := m._1
        DotGen.addEdge(m._2, xbardN)
      }
      mem_endpoints.foreach { a =>
        a._1 := crossbar
        DotGen.addEdge(xbardN, a._2)
      }
      mem_endpoints
    } else Seq()
  }

  lazy val module = new BeethovenAccSystemModule(this)
}


class BeethovenAccSystemModule(outer: BeethovenAccSystem)(implicit p: Parameters) extends LazyModuleImp(outer) {
  //noinspection ScalaUnusedSymbol
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new RoCCCommand()))
    val resp = Decoupled(new AccelRoccResponse())
  })

  outer.acc.module.io.cmd <> io.cmd
  io.resp <> outer.acc.module.io.resp
}