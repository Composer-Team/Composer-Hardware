package beethoven.common

import chisel3._
import chisel3.util.RegEnable
import freechips.rocketchip.diplomacy.ValName

import scala.annotation.tailrec

/**
 * I've run into trouble in the past where Chisel3 shiftregisters give me
 * unexpected behavior, so I have these instead.
 */

class ShiftReg[T <: Data](n: Int, gen: T) extends Module {
  val io = IO(new Bundle {
    val in = Input(gen)
    val out = Output(gen)
  })

  @tailrec
  private def recurse(t: T, latency: Int): T = {
    if (latency == 0) t
    else recurse(RegNext(t), latency - 1)
  }

  io.out := recurse(io.in, n)
}

object ShiftReg {
  def apply[T <: Data](t: T, latency: Int)(implicit valName: ValName): T = {
    val sr = Module(new ShiftReg[T](latency, t.cloneType))
    sr.suggestName("shiftReg" + valName.name)
    sr.io.in := t
    sr.io.out
  }
}

object ShiftRegWithReset {
  def apply[T <: Data](t: T, latency: Int, resetVal: T): T = {
    if (latency == 0) t
    else ShiftRegWithReset(RegNext(t, resetVal), latency - 1, resetVal)
  }
}

object ShiftRegEnable {
  @tailrec
  def apply[T <: Data](t: T, depth: Int, enable: Bool): T = {
    if (depth == 0) t
    else ShiftRegEnable(RegEnable(t, enable), depth - 1, enable)
  }
}