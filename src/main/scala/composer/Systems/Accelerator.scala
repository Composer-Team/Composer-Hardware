package composer.Systems

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.ComposerParams.{CoreIDLengthKey, SystemIDLengthKey}
import composer.MemoryStreams.CIntraCoreMemoryPort
import composer.TLManagement.makeTLMultilayerXbar
import composer.common._
import composer._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.ExtMem
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._

import scala.language.postfixOps

class ComposerAcc(implicit p: Parameters) extends LazyModule {
  val configs = p(ComposerSystemsKey)
  val name2Id = scala.collection.immutable.Map.from(configs.zipWithIndex.map(a => (a._1.name, a._2)))
  val requireInternalCmdRouting = configs.map(_.canIssueCoreCommandsTo).foldLeft(false)(_ || _.nonEmpty)

  // preprocess intra-core memory IOs and pass through param object
  val largestMM = p(ComposerSystemsKey).flatMap(_.coreParams.memoryChannelParams).filter(_.isInstanceOf[CIntraCoreMemoryPort]).map(_.asInstanceOf[CIntraCoreMemoryPort]).map(a => a.dataWidthBits.longValue() / 8 * a.nDatas.intValue()).fold(0L)(_ + _)
  val mm_algsz = log2Up(largestMM)
  val mm_off = 1L << mm_algsz
  val system2ICMPOffset = {
    val mm_amask = mm_off - 1
    val systems_sorted_with_n_icmp = configs.filter(_.coreParams.memoryChannelParams.count(_.isInstanceOf[CIntraCoreMemoryPort]) > 0).sortBy(cs => name2Id(cs.name)) map (cs => (cs, 1 << log2Up(cs.coreParams.memoryChannelParams.count(_.isInstanceOf[CIntraCoreMemoryPort]))))
    Map.from(systems_sorted_with_n_icmp.map(_._1) zip systems_sorted_with_n_icmp.map(_._2).scan(0)(_ + _) map {
      case (sys, off_mul) =>
        (sys.name, AddressSet(off_mul * mm_off, mm_amask))
    })
  }


  val system_tups = name2Id.keys.map { name =>
    val id = name2Id(name)
    val config = configs(id)
    val pWithMap = p.alterPartial({
      case SystemName2IdMapKey => name2Id
      case SystemName2ICMPMapKey => system2ICMPOffset
      case BaseICMPSizeKey => mm_off
    })
    (LazyModule(new ComposerSystem(config, id, configs.flatMap(_.canIssueCoreCommandsTo).contains(name))(pWithMap)), id, config)
  }.toSeq


  system_tups.foreach(a => a._1.suggestName(a._3.name))

  if (requireInternalCmdRouting) {
    // for each system that can issue commands, connect them to the target systems in the accelerator
    // Seq of tuple of (target name, src xbar)
    val cmdSourceSystems = system_tups.filter(_._3.canIssueCoreCommandsTo.nonEmpty).flatMap(a => a._3.canIssueCoreCommandsTo.map(b => (b, a)))
    system_tups.filter(_._1.incomingInternalCommandXbar.isDefined).foreach { sys =>
      val origins = cmdSourceSystems.filter(_._1 == sys._1.name).map(_._2)
      makeTLMultilayerXbar(origins.map(_._1.outgoingCommandXbar.get), Seq(sys._1.incomingInternalCommandXbar.get))
    }

    cmdSourceSystems.foreach { sys =>
      val respSources = system_tups.filter(other_sys => sys._2._3.canIssueCoreCommandsTo.contains(other_sys._1.name))
      makeTLMultilayerXbar(respSources.map(_._1.outgoingInternalResponseXbar.get), Seq(sys._2._1.incomingInternalResponseXBar.get))
    }
  }

  val systems = system_tups.map(_._1)
  val mems = systems.flatMap(_.memory_nodes)

  lazy val module = new ComposerAccModule(this)
}

class ComposerAccModule(outer: ComposerAcc)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new RoCCCommand))
    val resp = Decoupled(new RoCCResponse)
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
  accCmd.bits.inst.core_id := Cat(cmdRouter.io.out(1).bits.inst.rs1, cmdRouter.io.out(1).bits.inst.rs2)
  accCmd.bits.inst.rd := cmdRouter.io.out(1).bits.inst.rd
  accCmd.bits.inst.xd := cmdRouter.io.out(1).bits.inst.xd
  accCmd.bits.inst.xs1 := cmdRouter.io.out(1).bits.inst.xs1
  accCmd.bits.inst.xs2 := cmdRouter.io.out(1).bits.inst.xs2
  accCmd.bits.inst.opcode := cmdRouter.io.out(1).bits.inst.opcode

  accCmd.bits.inst.funct := cmdRouter.io.out(1).bits.inst.funct(6 - SystemIDLengthKey, 0)
  accCmd.bits.inst.system_id := cmdRouter.io.out(1).bits.inst.funct(6, 6 - SystemIDLengthKey + 1)

  accCmd.bits.getCoreID := cmdRouter.io.out(1).bits.rs1(63, 64 - CoreIDLengthKey)
  accCmd.bits.payload1 := cmdRouter.io.out(1).bits.rs1(63 - CoreIDLengthKey, 0)
  accCmd.bits.payload2 := cmdRouter.io.out(1).bits.rs2

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
    val respArbiter = Module(new RRArbiter[RoCCResponse](new RoCCResponse, systemSoftwareResps.size))
    respArbiter.io.in.zip(systemSoftwareResps).foreach { case (arbio, sysio) =>
      // these commands are being routed back to the sw so they need to be full Rocc
      arbio.bits.data := sysio.bits.packRocc
      arbio.bits.rd := sysio.bits.rd.get
      arbio.valid := sysio.valid
      sysio.ready := arbio.ready
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
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new RoCCCommand()))
    val resp = Decoupled(new RoCCResponse())
  })

  outer.acc.module.io.cmd <> io.cmd
  io.resp <> outer.acc.module.io.resp
}