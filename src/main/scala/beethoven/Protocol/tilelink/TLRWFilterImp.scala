package beethoven.Protocol.tilelink

import chisel3.util._
import chisel3._
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.tilelink.{TLBundleA, TLMessages}

class TLRWFilterImp(outer: TLRWFilter) extends LazyModuleImp(outer) {
  val in = outer.in_node.in.head._1
  val out_read = outer.read_out.out.head._1
  val out_write = outer.write_out.out.head._1

  val rqueue = Queue(out_read.d, entries = 4)
  val wqueue = Queue(out_write.d, entries = 4)
  val aqueue = Module(new Queue(new TLBundleA(in.a.bits.params), entries = 4))

  aqueue.io.enq.valid := in.a.valid
  aqueue.io.enq.bits := in.a.bits
  in.a.ready := aqueue.io.enq.ready

  val is_read = TLMessages.Get === aqueue.io.deq.bits.opcode

  aqueue.io.deq.ready := Mux(is_read, out_read.a.ready, out_write.a.ready)
  out_read.a.valid := aqueue.io.deq.valid && is_read
  out_read.a.bits := aqueue.io.deq.bits
  out_write.a.valid := aqueue.io.deq.valid && !is_read
  out_write.a.bits := aqueue.io.deq.bits

  rqueue.ready := false.B
  wqueue.ready := false.B

  when(rqueue.valid) {
    in.d <> rqueue
  }.otherwise {
    in.d <> wqueue
  }
}

