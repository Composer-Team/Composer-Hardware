package beethoven.Platforms.ASIC.memoryCompiler
import chisel3._

class ScanShifter(l: Int, w: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(w.W))
    val out = Output(Vec(l, UInt(w.W)))
  })
  val acc_wires = Wire(Vec(l, UInt(w.W)))
  acc_wires(0) := io.in
  (1 until l) foreach {i =>
    acc_wires(i) := RegNext(acc_wires(i-1))
  }
  io.out := acc_wires
}

object ScanShifter {
  def apply(a: => UInt, l: Int): Vec[UInt] = {
    val ss = Module(new ScanShifter(l, a.getWidth))
    ss.io.in := a
    ss.io.out
  }
}