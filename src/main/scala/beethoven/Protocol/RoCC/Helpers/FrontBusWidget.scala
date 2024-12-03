package beethoven.Protocol.RoCC.Helpers

import beethoven.Generation.CppGeneration
import beethoven.Platforms.PlatformKey
import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4.AXI4IdentityNode
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

class FrontBusWidget(implicit p: Parameters) extends LazyModule {
  val node = AXI4IdentityNode()
  val crFile = LazyModule(new MCRFileAXI(16))
  crFile.node := node
  override lazy val module = new AXILWidgetModule(this)
}

class AXILWidgetModule(outer: FrontBusWidget) extends LazyModuleImp(outer) {

  val io = IO(new Bundle {
    val cmds = Decoupled(UInt(32.W))
    val resp = Flipped(Decoupled(UInt(32.W)))
  })


  var allocated = 0

  def genPulsedBoolWO(name: String, drive: DecoupledIO[Bool], init: Bool): Bool = {
    drive.ready := true.B
    val state = Reg(Bool())
    state.suggestName(f"PulsedState$name")
    val addr = allocated << log2Up(p(PlatformKey).frontBusBeatBytes)
    CppGeneration.addPreprocessorDefinition(name, addr)
    allocated += 1
    state := init
    when (drive.fire) {
      state := drive.bits
    }
    state
  }

  def genRO[T <: Data](name: String, src: T, dst: DecoupledIO[T]): Unit = {
    val addr = allocated << log2Up(p(PlatformKey).frontBusBeatBytes)
    CppGeneration.addPreprocessorDefinition(name, addr)
    allocated += 1

    dst.bits := src
    dst.valid := true.B
  }

  def genWO[T <: Data](name: String, dst: DecoupledIO[T], init: T): T = {
    val addr = allocated << log2Up(p(PlatformKey).frontBusBeatBytes)
    CppGeneration.addPreprocessorDefinition(name, addr)
    allocated += 1

    val state = RegInit(init)
    state.suggestName(f"State${name}")
    dst.ready := true.B
    when (dst.fire) {
      state := dst.bits
    }

    state
  }

  val roccCmdFifo = Module(new Queue(UInt(32.W), 16))
  val roccRespFifo = Module(new Queue(UInt(32.W), 16))

  val mcrio = outer.crFile.module.io.mcr

  roccCmdFifo.io.enq.valid := genPulsedBoolWO("CMD_VALID", mcrio.write(0).map(_(0).asBool), false.B)
  roccCmdFifo.io.enq.bits := genWO("CMD_BITS", mcrio.write(1), 0.U)
  genRO("CMD_READY", roccCmdFifo.io.enq.ready, mcrio.read(2))

  genRO("RESP_VALID", roccRespFifo.io.deq.valid, mcrio.read(3))
  genRO("RESP_BITS", roccRespFifo.io.deq.bits, mcrio.read(4))
  roccRespFifo.io.deq.ready := genPulsedBoolWO("RESP_READY", mcrio.write(5).map(a => a(0).asBool), false.B)

  genRO("AXIL_DEBUG", 0xDEADCAFEL.U(32.W), mcrio.read(6))

  io.cmds <> roccCmdFifo.io.deq
  roccRespFifo.io.enq <> io.resp


}

