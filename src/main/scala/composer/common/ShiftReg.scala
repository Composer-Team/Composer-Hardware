package composer.common

import chisel3._
import chisel3.util.RegEnable

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

object ShiftRegEnable {
  @tailrec
  def apply[T <: Data](t: T, depth: Int, enable: Bool): T = {
    if (depth == 0) t
    else ShiftRegEnable(RegEnable(t, enable), depth - 1, enable)
  }
}