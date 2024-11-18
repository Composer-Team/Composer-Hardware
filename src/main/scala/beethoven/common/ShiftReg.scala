package beethoven.common

import beethoven.MemoryStreams.Memory
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
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
  def apply[T <: Data](t: T, latency: Int, useMemory: Boolean = false, withWidth: Option[Int] = None)(implicit valName: ValName, p: Parameters): T = {
    if (useMemory) {
      val mem = Memory(2, withWidth.getOrElse(t.getWidth), latency+1, 1, 1, 0, allowFallbackToRegister = false)
      mem.initLow(clock = chisel3.Module.clock)
      val read = mem.getReadPortIdx(0)
      val write = mem.getWritePortIdx(0)
      val read_ptr = Reg(UInt(log2Up(latency+1).W))
      when (chisel3.Module.reset.asBool) {
        read_ptr := 0.U
      }
      when (read_ptr === latency.U) {
        read_ptr := 0.U
      }.otherwise {
        read_ptr := read_ptr + 1.U
      }
      val write_ptr = RegNext(read_ptr)
      mem.addr(read) := read_ptr
      mem.write_enable(read) := false.B
      mem.read_enable(read) := true.B
      mem.chip_select(read) := true.B

      mem.addr(write) := write_ptr
      mem.write_enable(write) := true.B
      mem.read_enable(write) := true.B
      mem.chip_select(write) := true.B
      mem.data_in(write) := t.asUInt

      mem.data_out(read).asTypeOf(t)
    } else {
      val sr = Module(new ShiftReg[T](latency, t.cloneType))
      sr.suggestName("shiftReg" + valName.name)
      sr.io.in := t
      sr.io.out
    }
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