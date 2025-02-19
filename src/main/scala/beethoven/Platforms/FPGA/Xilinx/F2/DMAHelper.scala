package beethoven.Platforms.FPGA.Xilinx.F2

import chisel3._
import chisel3.util._
import beethoven._
import beethoven.common._
import chipsalliance.rocketchip.config.Parameters

class DMAHelper()(implicit p: Parameters) extends AcceleratorCore {
  val io = BeethovenIO(new AccelCommand("memcmd") {
    val write = Bool()
    val address = Address()
    val payload = UInt(32.W)
  }, new AccelResponse("my_response") {
    val payload = UInt(32.W)
  })

  val s_idle :: s_write :: s_wait :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val reader = getReaderModule("read")
  val writer = getWriterModule("write")
  reader.requestChannel.valid := io.req.valid && !io.req.bits.write && writer.requestChannel.ready && state === s_idle
  writer.requestChannel.valid := io.req.valid && io.req.bits.write && reader.requestChannel.ready && state === s_idle
  io.req.ready := state === s_idle && reader.requestChannel.ready && writer.requestChannel.ready
  reader.requestChannel.bits.len := 4.U // 4 bytes
  writer.requestChannel.bits.len := 4.U
  dontTouch(writer.requestChannel.bits.len)
  reader.requestChannel.bits.addr := io.req.bits.address
  writer.requestChannel.bits.addr := io.req.bits.address
  val payload_hold = Reg(UInt(32.W))
  val is_write = Reg(Bool())
  when(io.req.fire) {
    is_write := io.req.bits.write
    state := Mux(io.req.bits.write, s_write, s_wait)
    payload_hold := io.req.bits.payload
  }

  when (io.resp.fire) {
    state := s_idle
  }

  // write data channel will be ready 1 cycle after request fire
  writer.dataChannel.data.valid := state === s_write
  writer.dataChannel.data.bits := payload_hold
  when (writer.dataChannel.data.fire) {
    state := s_wait
  }
  reader.dataChannel.data.ready := state === s_wait && !is_write

  io.resp.bits.payload := Mux(is_write, DontCare, reader.dataChannel.data.bits)
  io.resp.valid := state === s_wait && Mux(is_write,
    writer.dataChannel.isFlushed && writer.requestChannel.ready,
    reader.dataChannel.data.valid)
}

class DMAHelperConfig extends AcceleratorSystemConfig(
  nCores = 1,
  name = "DMAHelper",
  moduleConstructor = ModuleBuilder(p=> new DMAHelper()(p)),
  memoryChannelConfig = List(
    ReadChannelConfig("read", 4),
    WriteChannelConfig("write", 4))
)
