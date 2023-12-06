import chisel3._
import chisel3.util._
import composer.Protocol.AXI4Compat
import freechips.rocketchip.subsystem.MasterPortParams

class MemReadRTL(memBusWidth: Int, hostBusWidth: Int, txLen: Int) extends Module {
  val mem = IO(new AXI4Compat(param = MasterPortParams(
    base = 0,
    size = 1 << 24,
    beatBytes = memBusWidth / 8,
    idBits = 6)))

  val io = IO(Flipped(new AXI4Compat(param = MasterPortParams(
    base = 0,
    size = 1 << 8,
    beatBytes = hostBusWidth / 8,
    idBits = 6))))

  val s_idle :: s_collect :: s_finish_read :: s_emit :: s_wait_last :: Nil = Enum(5)
  val state = RegInit(s_idle)
  io.initFromSlaveLow()
  mem.initFromMasterLow()
  val addr = Reg(UInt(32.W))
  mem.arburst := 1.U
  mem.awburst := 1.U
  mem.araddr := addr

  mem.arlen := (txLen - 1).U
  mem.awlen := (txLen - 1).U


  val counts = Reg(UInt(16.W))
  val cycleCounter, cycleHold = Reg(UInt(hostBusWidth.W))
  cycleCounter := cycleCounter + 1.U
  when(state === s_idle) {
    io.awready := mem.arready
    io.arready := true.B
    when (io.awready && io.awvalid) {
      state := s_collect
    }.elsewhen(io.arready && io.arvalid) {
      state := s_finish_read
    }
  }.elsewhen(state === s_collect) {
    io.wready := true.B
    when (io.wready && io.wvalid) {
      counts := io.wdata
      state := s_emit
      cycleCounter := 0.U
    }
  }.elsewhen(state === s_emit) {
    mem.arvalid := true.B
    when (mem.arready) {
      state := s_wait_last
    }
  }.elsewhen(state === s_wait_last) {
    mem.rready := true.B
    when (mem.rvalid && mem.rlast) {
      counts := counts - 1.U
      when (counts === 0.U) {
        state := s_idle
        cycleHold := cycleCounter
      }.otherwise {
        state := s_emit
      }
    }
  }.elsewhen(state === s_finish_read) {
    io.rdata := cycleHold
    io.rvalid := true.B
    io.rlast := true.B
    when (io.rready) {
      state := s_idle
    }
  }
}