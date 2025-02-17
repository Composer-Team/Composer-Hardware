package beethoven.Floorplanning

import chisel3._
import beethoven.common.ShiftReg
import chipsalliance.rocketchip.config.Parameters

object ResetBridge {
  def apply[T <: Reset](dut: T, bridgeDelay: Int)(implicit p: Parameters): T = {
    val bridge = Module(new ResetBridge(dut, bridgeDelay))
    bridge.io.reset <> dut
    bridge.io.dut_reset
  }
  def apply[T <: Reset](dut: T, clock: Clock, bridgeDelay: Int)(implicit p: Parameters): T = {
    val bridge = Module(new ResetBridge(dut, bridgeDelay))
    bridge.io.reset <> dut
    bridge.io.clock := clock
    bridge.io.dut_reset
  }
}

class ResetBridge[T <: Reset](dut: T, bridgeDelay: Int)(implicit p: Parameters) extends RawModule {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(dut.cloneType)
    val dut_reset = Output(dut.cloneType)
  })
  withClockAndReset(io.clock, false.B.asAsyncReset) {
    io.dut_reset := ShiftReg(io.reset.asBool, bridgeDelay, io.clock)
  }
}
