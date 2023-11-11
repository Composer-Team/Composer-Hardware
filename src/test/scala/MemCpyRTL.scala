//import chisel3._
//import chisel3.util._
//import composer.Protocol.AXI4Compat
//import freechips.rocketchip.subsystem.MasterPortParams
//class MemCpyRTL extends Module {
//  val mem = IO(new AXI4Compat(param = MasterPortParams(
//    base=0,
//    size=1 << 24,
//    beatBytes = 64,
//    idBits = 6)))
//
//  val io = IO(Flipped(new AXI4Compat(param = MasterPortParams(
//    base = 0,
//    size = 1 << 8,
//    beatBytes = 4,
//    idBits = 6))))
//
//  val s_idle :: s_respond ::  s_emit :: s_run_read :: s_run_write :: s_run_cpy :: s_finish :: Nil = Enum(5)
//  val state = RegInit(s_idle)
//  io.initLow()
//  mem.initLow()
//  mem.arburst := 1.U
//  mem.awburst := 1.U
//  val axiTxN = 16 // arbitrary
//  val axiTxWidth = 64 // bytes
//  val axiTxLen = 1024 * 4 / axiTxWidth
//
//  mem.arlen := (axiTxLen - 1).U
//  mem.awlen := (axiTxLen - 1).U
//
//  val txCounterR, txCounterW = Reg(UInt((1 + nTxsNeeded).W))
//
//  val cycleCounter = Reg(UInt(32.W))
//  val prog = Reg(UInt(2.W))
//  when (state === s_idle) {
//    io.arready := mem.arready && mem.awready
//    when (io.arvalid && io.arready) {
//      when (io.araddr === 0.U) {
//        state := s_run_read
//        mem.arvalid := true.B
//      }.elsewhen(io.araddr === 4.U) {
//        state := s_run_write
//      }.otherwise {
//        state := s_run_cpy
//      }
//    }
//  }.elsewhen (state === s_emit) {
//    mem.
//  }
//}