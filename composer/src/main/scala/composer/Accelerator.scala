package composer

import chisel3._
import chisel3.util._
import composer.TLManagement.makeTLMultilayerXbar
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.config.Parameters
import composer.common._
import freechips.rocketchip.subsystem.ExtMem

import scala.language.postfixOps

class ComposerAcc(implicit p: Parameters) extends LazyModule {
  val configs = p(ComposerSystemsKey)
  val name2Id = scala.collection.immutable.Map.from(configs.zipWithIndex.map(a => (a._1.name, a._2)))
  println(name2Id)
  configs.zipWithIndex.foreach { case (c, id) =>
    println(s"$id => $c")
  }
  println(configs.length)
  val requireInternalCmdRouting = configs.map(_.canIssueCoreCommands).foldLeft(false)(_ || _)

  val system_tups = name2Id.keys.map { name =>
    val id = name2Id(name)
    val config = configs(id)
    val pWithMap = p.alterPartial({
      case SystemName2IdMapKey => name2Id
      case RequireInternalCommandRouting => requireInternalCmdRouting
    })
    (LazyModule(new ComposerSystem(config, id)(pWithMap)), id, config)
  }.toSeq

  system_tups.foreach(a => a._1.suggestName(a._3.name))

  if (requireInternalCmdRouting) {
    // for each system that can issue commands, connect them to the other systems in the accelerator
    val cmdSourceSystems = system_tups.filter(_._3.canIssueCoreCommands).map(_._1.outgoingCommandXbar.get)
    val cmdManagerSystems = system_tups.map(_._1.incomingInternalCommandXbar.get)
    makeTLMultilayerXbar(cmdSourceSystems, cmdManagerSystems)

    val respSources = system_tups.map(_._1.outgoingInternalResponseXbar.get)
    val respManagers = system_tups.filter(_._3.canIssueCoreCommands).map(_._1.incomingInternalResponseXBar.get)
    makeTLMultilayerXbar(respSources, respManagers)
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
  accCmd.bits.inst.rs1 := cmdRouter.io.out(1).bits.inst.rs1
  accCmd.bits.inst.rs2 := cmdRouter.io.out(1).bits.inst.rs2
  accCmd.bits.inst.rd := cmdRouter.io.out(1).bits.inst.rd
  accCmd.bits.inst.xd := cmdRouter.io.out(1).bits.inst.xd
  accCmd.bits.inst.xs1 := cmdRouter.io.out(1).bits.inst.xs1
  accCmd.bits.inst.xs2 := cmdRouter.io.out(1).bits.inst.xs2
  accCmd.bits.inst.opcode := cmdRouter.io.out(1).bits.inst.opcode

  val nSystemIDBits = p(SystemIDLengthKey)
  require(nSystemIDBits <= 7)
  accCmd.bits.inst.funct := cmdRouter.io.out(1).bits.inst.funct(6-nSystemIDBits, 0)
  accCmd.bits.inst.system_id := cmdRouter.io.out(1).bits.inst.funct(6, 6-nSystemIDBits+1)

  val nCoreIDBits = p(CoreIDLengthKey)
  require(nCoreIDBits < 64)
  accCmd.bits.core_id := cmdRouter.io.out(1).bits.rs1(63, 64-nCoreIDBits)
  accCmd.bits.payload1 := cmdRouter.io.out(1).bits.rs1(63-nCoreIDBits, 0)
  accCmd.bits.payload2 := cmdRouter.io.out(1).bits.rs2

  val system_id = accCmd.bits.inst.system_id
  accCmd.ready := false.B //base case

  val waitingToFlush = RegInit(false.B)
  cmdRouter.io.out(0).ready := !waitingToFlush
  when (cmdRouter.io.out(0).fire) {
    waitingToFlush := true.B
  }

  val systemSoftwareResps = outer.system_tups.zipWithIndex.filter(_._1._3.canReceiveSoftwareCommands) map { case (sys_tup, sys_idx) =>
    val sys_queue = Module(new Queue(new ComposerRoccCommand, entries = 2))

    // enqueue commands from software
    sys_queue.io.enq.valid := accCmd.valid && sys_idx.U === system_id
    sys_queue.io.enq.bits := accCmd.bits
    when (system_id === sys_idx.U) {
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
      arbio.bits.data := sysio.bits.pack()
      arbio.bits.rd := sysio.bits.rd
      arbio.valid := sysio.valid
      sysio.ready := arbio.ready
    }
    respArbiter.io.out <> io.resp
  }
}

class ComposerAccSystem(implicit p: Parameters) extends LazyModule {
  val nMemChannels = p(ExtMem).get.nMemoryChannels

  val hostmem = TLIdentityNode()
  val mem = Seq.fill(nMemChannels) { TLIdentityNode() }

  val dummyTL = p.alterPartial({ case TileVisibilityNodeKey => mem.head})
  val acc = LazyModule(new ComposerAcc()(dummyTL))

  val crossbar = TLXbar()

  acc.mems foreach (crossbar := _)
  mem.foreach ( _ := crossbar)
  crossbar := hostmem

  lazy val module = new ComposerAccSystemModule(this)
}

class ComposerAccSystemModule(outer: ComposerAccSystem)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new RoCCCommand()(outer.dummyTL)))
    val resp = Decoupled(new RoCCResponse()(outer.dummyTL))
  })

  outer.acc.module.io.cmd <> io.cmd
  io.resp <> outer.acc.module.io.resp
}