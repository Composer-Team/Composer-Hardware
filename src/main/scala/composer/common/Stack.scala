package composer.common

import chisel3._
import chisel3.util._

class Stack[T <: Data](gen: => T,
                       depth: Int) extends Module {
  val io = IO(new QueueIO[T](gen, depth, hasFlush = false))

  val mem = Reg(Vec(depth, gen))
  val ptr = RegInit(0.U(log2Up(depth+1).W))
  io.count := ptr

  io.deq.valid := ptr =/= 0.U
  io.enq.ready := false.B
  io.deq.bits := DontCare

  when(io.deq.fire) {
    io.deq.bits := mem(ptr - 1.U)
    ptr := ptr - 1.U
  } otherwise {
    io.enq.ready := ptr =/= depth.U
    when(io.enq.fire) {
      ptr := ptr + 1.U
      mem(ptr) := io.enq.bits
    }
  }
}
