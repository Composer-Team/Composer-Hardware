package composer.common

import chisel3._

import scala.annotation.tailrec
object ShiftReg {
  @tailrec
  def apply[T <: Data](t: T, latency: Int): T = {
    if (latency == 0) t
    else ShiftReg(RegNext(t), latency - 1)
  }
}

object ShiftRegWithReset {
  def apply[T <: Data](t: T, latency: Int, resetVal: T): T = {
    if (latency == 0) t
    else ShiftReg(RegNext(t, resetVal), latency - 1)
  }
}
