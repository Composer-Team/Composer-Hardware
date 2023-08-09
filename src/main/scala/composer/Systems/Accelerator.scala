package composer.Systems

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.common._
import composer._
import composer.Platforms.HasDisjointMemoryControllers
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.ExtMem
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._

import scala.language.postfixOps

class ComposerAcc(implicit p: Parameters) extends LazyModule {
  val configs = p(AcceleratorSystems)
  val name2Id = scala.collection.immutable.Map.from(configs.zipWithIndex.map(a => (a._1.name, a._2)))
  val requireInternalCmdRouting = configs.map(_.canIssueCoreCommandsTo).foldLeft(false)(_ || _.nonEmpty)


  val system_tups = name2Id.keys.map { name =>
    val id = name2Id(name)
    val config = configs(id)
    val pWithMap = p.alterPartial({
      case SystemName2IdMapKey => name2Id
    })
    (LazyModule(new ComposerSystem(config, id, configs.flatMap(_.canIssueCoreCommandsTo).contains(name), this)(pWithMap)), id, config)
  }.toSeq


  system_tups.foreach(a => a._1.suggestName(a._3.name))

  // tie together intra-core memory ports in one system to core/channel sps individually in another system
  system_tups.foreach { case (sys, _, _) =>
    sys.cores.foreach { case (_, core) =>
      core.intraCoreMemMasters.foreach { case (pparams, clients, _) =>
        val assoc_endpoints = system_tups.filter(_._3.name == pparams.toSystem)(0)._1.cores.map(_._2.intraCoreMemSlaveNodes.filter(_._1 == pparams.toMemoryPort)(0)).map(_._2)
        require(clients.flatten.length == assoc_endpoints.flatten.length, "sanity - if this doesn't work, somethign bad has happened")
        clients.flatten zip assoc_endpoints.flatten foreach { case (c, e) =>
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

  val mems = systems.flatMap(_.memory_nodes)

  lazy val module = new ComposerAccModule(this)
}

class ComposerAccModule(outer: ComposerAcc)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new RoCCCommand))
    val resp = Decoupled(new AccelRoccResponse)
  })

  val cmd = Queue(io.cmd)

  val cmdArb = Module(new RRArbiter(new RoCCCommand, 1))
  cmdArb.io.in(0) <> cmd

  // route cmds between:
  //  custom0: control opcodes (used for WAIT commands)
  //  custom3: accelerator commands
  val cmdRouter = Module(new RoccCommandRouter(Seq(OpcodeSet.custom0, OpcodeSet.custom3)))
  cmdRouter.io.in <> cmdArb.io.out

  val accCmd = Wire(Decoupled(new ComposerRoccCommand)) //CUSTOM3, used for everything else
  accCmd.valid := cmdRouter.io.out(1).valid
  cmdRouter.io.out(1).ready := accCmd.ready
  accCmd <> cmdRouter.io.out(1).map(ComposerRoccCommand.fromRoccCommand)
  val system_id = accCmd.bits.inst.system_id
  accCmd.ready := false.B //base case

  val waitingToFlush = RegInit(false.B)
  cmdRouter.io.out(0).ready := !waitingToFlush
  when(cmdRouter.io.out(0).fire) {
    waitingToFlush := true.B
  }

  val systemSoftwareResps = outer.system_tups.filter(_._3.canReceiveSoftwareCommands) map { sys_tup =>
    val sys_queue = Module(new Queue(new ComposerRoccCommand, entries = 2))
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
    when(waitingToFlush) {
      sys_queue.io.deq.ready := false.B
      sys_tup._1.module.sw_io.get.cmd.valid := false.B
    }
    sys_tup._1.module.sw_io.get.resp
  }

  if (systemSoftwareResps.isEmpty) {
    io.resp.valid := false.B
    io.resp.bits := DontCare
  } else {
    val respArbiter = Module(new RRArbiter[AccelRoccResponse](new AccelRoccResponse, systemSoftwareResps.size))
    respArbiter.io.in.zip(systemSoftwareResps).foreach { case (arbio, sysio) =>
      // these commands are being routed back to the sw so they need to be full Rocc
      arbio <> sysio
    }
    respArbiter.io.out <> io.resp
  }
}

class ComposerAccSystem(implicit p: Parameters) extends LazyModule {
  val nMemChannels = p(ExtMem).get.nMemoryChannels

  val acc = LazyModule(new ComposerAcc())

  val mem = if (acc.mems.nonEmpty) {
    val nEndpoints = if (p(HasDisjointMemoryControllers)) Math.min(nMemChannels, acc.mems.length)
    else nMemChannels
    val mem = Seq.fill(nEndpoints)(TLIdentityNode())

    if (p(HasDisjointMemoryControllers)) {
      // disjoint memory controllers go to different addresses. The redirection to the correct controller
      // happens at the crossbar level, so we join all transactions at a single crossbar and then feed out to the
      // individual channels
      val crossbarModule = LazyModule(new TLXbar())
      val crossbar = crossbarModule.node
      acc.mems foreach (crossbar := _)
      mem.foreach(_ := crossbar)
    } else {
      // controllers can all access the same addresses! Split up end points to be able to access these addresses in
      // parallel
      val nondisjointXbars = Seq.fill(nMemChannels)(LazyModule(new TLXbar()))
      acc.mems.zipWithIndex foreach { case (m, idx) =>
        nondisjointXbars(idx % nMemChannels).node := m
      }
      mem zip nondisjointXbars foreach { case (m, xb) => m := xb.node }
    }
    mem
  } else Seq()

  lazy val module = new ComposerAccSystemModule(this)
}


class ComposerAccSystemModule(outer: ComposerAccSystem)(implicit p: Parameters) extends LazyModuleImp(outer) {
  //noinspection ScalaUnusedSymbol
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new RoCCCommand()))
    val resp = Decoupled(new AccelRoccResponse())
  })

  outer.acc.module.io.cmd <> io.cmd
  io.resp <> outer.acc.module.io.resp
}