package beethoven.common
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable

/**
  * Generate circuit to map numbers to other numbers. Any non-specified pairs in the truth_table are considered DNCs.
  * Unoptimized, this is pretty expensive but is VERY optmizable in the average case (see Karnaugh maps)
  * @param inWidth input width
  * @param outWidth output width
  * @param truth_table list of every desired input/output pair
  */
class HardwareMap(inWidth: Int, outWidth: Int, truth_table: Seq[(Int, Int)]) extends Module {
  val io = IO(new Bundle() {
    val in = Input(Bits(inWidth.W))
    val out = Output(Bits(outWidth.W))
  })
  val obits = (0 until outWidth) map { idx =>
    // look at one bit of the output at a time
    val tt_chop = truth_table map { case (in: Int, out: Int) => (in, out & (1 << idx))}
    val neg = tt_chop.filter(_._2 == 0)
    val pos = tt_chop.filter(_._2 != 0)
    def toAnds(a: Int): Bool = {
      (0 until inWidth).map{inBit: Int =>
        if ((a & (1 << inWidth)) == 1) io.in(inBit)
        else !io.in(inBit)
      }.reduce(_ && _)
    }

    val is_pos = pos.map(a => toAnds(a._1)).reduce(_ || _)
    val is_neg = neg.map(a => toAnds(a._1)).reduce(_ || _)
    is_pos && !is_neg
  }
  io.out := Cat(obits)
}
