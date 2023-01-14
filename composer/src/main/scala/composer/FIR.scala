package composer

import chisel3._

class FIR(bitWidth: Int, coeffs: Seq[Int]) extends Module {
  val io = IO(new Bundle() {
    val in = Input(UInt(bitWidth.W))
    val out = Output(UInt(bitWidth.W))
  })

  // make a list of everything in my FIR filter
  // first value is the input, the rest are registers storing previous inputs
  val my_vals = Seq(io.in) ++ (coeffs.tail map (_ => Reg(UInt(bitWidth.W))))
  // chain the registers to eachother in FIFO fashion
  (1 until my_vals.length) foreach { idx => my_vals(idx) := my_vals(idx - 1) }

  // compute the moving average using provided coefficients
  io.out := my_vals.zip(coeffs).map({case (reg, c) => reg * c.U}).reduce(_ + _)
}




