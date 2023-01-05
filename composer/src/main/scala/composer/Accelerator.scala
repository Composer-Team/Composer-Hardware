package composer

import chisel3._
import chisel3.util._
import composer.CppGenerationUtils.genCPPHeader
import composer.RoccConstants.FUNC_START
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.config.Parameters
import composer.common._
import composer.common.Util._
import freechips.rocketchip.amba.axi4.{AXI4Buffer, AXI4IdentityNode, AXI4ToTL}
import freechips.rocketchip.subsystem.ExtMem

import java.io.FileWriter
import scala.annotation.tailrec
import scala.language.postfixOps

class ComposerAcc(implicit p: Parameters) extends LazyModule {

  val configs = p(ComposerSystemsKey)
  val system_tups = configs.zipWithIndex.map { case (c: ComposerSystemParams, id: Int) =>
    (LazyModule(new ComposerSystem(c, id)), id, c)
  }

  def sysLookup(opcode: Int): ComposerSystem = {
    system_tups(system_tups.indexWhere(_._2 == opcode))._1
  }

  val systems = system_tups.map(_._1)
  val mems = systems.flatMap(_.memory_nodes)

  lazy val module = new ComposerAccModule(this)
}

class ComposerAccModule(outer: ComposerAcc)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new RoCCCommand))
    val resp = Decoupled(new RoCCResponse)
    val busy = Output(Bool())
  })

  val cmd = Queue(io.cmd)

  val cmdArb = Module(new RRArbiter(new RoCCCommand, 1))
  cmdArb.io.in <> Seq(cmd)

  // route cmds between:
  //  custom0: control opcodes (used for WAIT commands)
  //  custom3: accelerator commands
  val cmdRouter = Module(new RoccCommandRouter(Seq(OpcodeSet.custom0, OpcodeSet.custom3)))
  cmdRouter.io.in <> cmdArb.io.out

  val optionCmd = cmdRouter.io.out(0) // CUSTOM0, used for WAIT
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
  // TODO UG: The top bits in funct refer to the system_id. Ensure that we extract the bits according to our param
  val nSystemIDBits = p(SystemIDLengthKey)
  accCmd.bits.inst.funct := cmdRouter.io.out(1).bits.inst.funct(2, 0)
  accCmd.bits.inst.system_id := cmdRouter.io.out(1).bits.inst.funct(6, 3)
  // TODO UG: same thing with Core ID. Get parameter and extract bits right for core_id and rs1
  accCmd.bits.core_id := cmdRouter.io.out(1).bits.rs1(63, 56)
  accCmd.bits.rs1 := cmdRouter.io.out(1).bits.rs1(55, 0)
  accCmd.bits.rs2 := cmdRouter.io.out(1).bits.rs2
  val system_id = accCmd.bits.inst.system_id
  accCmd.ready := false.B //base case

  val maxCore = outer.systems.map(_.nCores).max
  val hostRespFile = RegInit(VecInit(Seq.fill(outer.systems.length)(VecInit(Seq.fill(maxCore)(false.B)))))
  val monitorWaiting = RegInit(false.B)

  val systemQueues = outer.systems.map(_ => Module(new Queue(new ComposerRoccCommand, 2)))
  outer.system_tups zip systemQueues foreach { case (tup, q) =>
    q.io.enq.valid := (accCmd.valid && system_id === tup._2.U /* && !monitorWaiting*/)
    q.io.enq.bits := accCmd.bits
    when(system_id === tup._2.U) {
      accCmd.ready := q.io.enq.ready
    }
    tup._1.module.io.cmd <> q.io.deq
    when(q.io.deq.fire && isStart(q.io.deq.bits)) {
      hostRespFile(q.io.deq.bits.inst.system_id)(q.io.deq.bits.core_id) := q.io.deq.bits.inst.xd
    }
    when(monitorWaiting) {
      q.io.deq.ready := false.B
      tup._1.module.io.cmd.valid := false.B
    }
  }

  val anyBusy = outer.system_tups.map(_._1.module.io.busy).any
  when(monitorWaiting === true.B && !anyBusy) {
    monitorWaiting := false.B
  }
  when(optionCmd.fire) {
    monitorWaiting := true.B
  }
  optionCmd.ready := !monitorWaiting

  def isStart(x: ComposerRoccCommand): Bool = {
    x.inst.funct === FUNC_START.U
  }


  val respIF = outer.systems.map(_.module.io.resp)
  val respArb = Module(new RRArbiter(new ComposerRoccResponse(), respIF.size))
  when(respArb.io.out.fire) {
    hostRespFile(respArb.io.out.bits.system_id)(respArb.io.out.bits.core_id) := false.B
  }

  respArb.io.in <> respIF
  //io.resp <> respArb.io.out
  respArb.io.out.ready := io.resp.ready

  io.resp.bits.rd := respArb.io.out.bits.rd
  // TODO UG: fix this to extract the correct bits according to the parameter
  io.resp.bits.data := respArb.io.out.bits.packData()
  io.resp.valid := respArb.io.out.valid && hostRespFile(respArb.io.out.bits.system_id)(respArb.io.out.bits.core_id)
  // MONITOR CODE END

  io.busy := cmdArb.io.out.valid || accCmd.valid || outer.systems.map(_.module.io.busy).any || systemQueues.map(_.io.count =/= 0.U).any

}

class ComposerAccSystem(implicit p: Parameters) extends LazyModule {
  val dummyTL = p.alterPartial({ case TileVisibilityNodeKey => mem.head})

  val nMemChannels = p(ExtMem).get.nMemoryChannels

  val hostmem = TLIdentityNode()
  val mem = Seq.fill(nMemChannels) {
    TLIdentityNode()
  }
  lazy val acc = LazyModule(new ComposerAcc()(dummyTL))

  lazy val crossbar = LazyModule(new TLXbar)
  acc.mems.foreach (crossbar.node := _)
  // what happens if we get rid of this?
  crossbar.node := hostmem

  mem.foreach ( _ := crossbar.node)

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