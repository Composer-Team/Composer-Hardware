package beethoven.Platforms.FPGA.Xilinx.F2

import chisel3._
import chisel3.util._
import beethoven._
import beethoven.common._
import chipsalliance.rocketchip.config.Parameters

class MemsetHelper(dataWidthBytes: Int)(implicit p: Parameters) extends AcceleratorCore {
  require(isPow2(dataWidthBytes), "data payload must be power two of bytes")
  val bitWidth = 8 * dataWidthBytes
  val io = BeethovenIO(new AccelCommand("memset") {
    val address = Address()
    val num_eles = UInt(32.W)
    val payload = UInt(bitWidth.W)
    val a = "a"
  }, EmptyAccelResponse())

  val write = getWriterModule("write")

  val s_idle :: s_wait :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val payload = Reg(UInt(bitWidth.W))
  when (io.req.fire) {
    payload := io.req.bits.payload
    state := s_wait
  }

  io.req.ready := state === s_idle && write.requestChannel.ready
  write.requestChannel.bits.addr := io.req.bits.address
  write.requestChannel.bits.len := common.Misc.multByIntPow2(io.req.bits.num_eles, dataWidthBytes)
  write.requestChannel.valid := io.req.valid && state === s_idle

  write.dataChannel.data.bits := payload
  write.dataChannel.data.valid := state === s_wait

  when (state === s_wait && write.requestChannel.ready) {
    state := s_finish
  }

  io.resp.valid := state === s_finish
  when (io.resp.fire) {
    state === s_idle
  }
}


class MemsetHelperConfig(payloadWidthBytes: Int) extends AcceleratorSystemConfig(
  nCores = 1,
  name = "MemsetHelper",
  moduleConstructor = ModuleBuilder(p=> new MemsetHelper(payloadWidthBytes)(p)),
  memoryChannelConfig = List(
    WriteChannelConfig("write", payloadWidthBytes))
)
