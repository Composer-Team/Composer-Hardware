package basic

import beethoven._
import beethoven.common._
import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._

class MyAccelerator(implicit p: Parameters) extends AcceleratorCore {
  val io = BeethovenIO(new AccelCommand("my_accel") {
    val addend = UInt(32.W)
    val vec_addr = Address()
    val n_eles = UInt(20.W)
  }, EmptyAccelResponse())
  val addendReg = Reg(UInt(32.W))
  val activeCmd = RegInit(false.B)
  when (io.req.fire) {
    addendReg := io.req.bits.addend
    activeCmd := true.B
  }

  val write_len_bytes = Cat(io.req.bits.n_eles, 0.U(2.W))
  val ReaderModuleChannel(vec_in_request, vec_in_data) = getReaderModule("vec_in")
  vec_in_request.valid := io.req.valid
  vec_in_request.bits.addr := io.req.bits.vec_addr
  vec_in_request.bits.len := write_len_bytes
  val WriterModuleChannel(vec_out_request, vec_out_data) = getWriterModule("vec_out")
  vec_out_request.valid := io.req.valid
  vec_out_request.bits.addr := io.req.bits.vec_addr
  vec_out_request.bits.len := write_len_bytes
  // split vector into 32b chunks and add addend to it
  vec_out_data.data <> vec_in_data.data.map(bitVec =>
    applyToChunks(bitVec, 32, _ + addendReg))
  io.req.ready := vec_in_request.ready && vec_out_request.ready && !activeCmd
  io.resp.valid := vec_in_request.ready && vec_out_request.ready && activeCmd
  when (io.resp.fire) {
    activeCmd := false.B
  }
}