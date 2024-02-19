package composer.TLManagement

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import chisel3._
import chisel3.util.Queue

class TLRWFilter(spp: TLSlavePortParameters, mpp: TLMasterPortParameters)(implicit p: Parameters) extends LazyModule{

  val in_node = TLManagerNode(Seq(spp))
  val read_out, write_out = TLClientNode(Seq(mpp))

  lazy val module = new TLRWFilterImp(this)
}

class TLRWFilterImp(outer: TLRWFilter) extends LazyModuleImp(outer) {
  val in = outer.in_node.in.head._1
  val out_read = outer.read_out.out.head._1
  val out_write = outer.write_out.out.head._1

  val rqueue = Queue(out_read.d, entries=4)
  val wqueue = Queue(out_write.d, entries=4)
  val aqueue = Queue(in.a, entries=4)


  val is_read = TLMessages.Get === aqueue.bits.opcode
  out_read.a.valid := aqueue.valid && is_read
  out_write.a.valid := aqueue.valid && !is_read
  in.a.ready := Mux(is_read, out_read.a.ready, out_write.a.ready)



  rqueue.ready := false.B
  wqueue.ready := false.B
  aqueue.ready := false.B

  when (rqueue.valid) {
    in.d <> rqueue
  }.otherwise {
    in.d <> wqueue
  }

}
