package fpnewWrapper.fpnew

import chisel3._
import chisel3.util.HasBlackBoxResource

object FPNewFType extends Enumeration {
  type FPNewFType = Value
  val B16, HalfPrecision, FullPrecision, DoublePrecision = Value

  def toWidth(ftype: FPNewFType): Int = {
    ftype match {
      case FPNewFType.B16 => 16
      case FPNewFType.HalfPrecision => 16
      case FPNewFType.FullPrecision => 32
      case FPNewFType.DoublePrecision => 64
    }
  }
}

class FPNBlackboxIO(fLen: Int, tagWidth: Int) extends Bundle {
    val clk_i = Input(Clock())
    val rst_ni = Input(Bool())
    val operands_i = Input(UInt((fLen * 3).W))
    val rnd_mode_i = Input(UInt(3.W))
    val op_i = Input(UInt(4.W))
    val op_mod_i = Input(Bool())
    val src_fmt_i = Input(UInt(3.W))
    val dst_fmt_i = Input(UInt(3.W))
    val int_fmt_i = Input(UInt(2.W))
    val vectorial_op_i = Input(Bool())
    val tag_i = Input(UInt(tagWidth.W))
    val in_valid_i = Input(Bool())
    val in_ready_o = Output(Bool())
    val flush_i = Input(Bool())
    val result_o = Output(UInt(fLen.W))
    val status_o = Output(UInt(5.W))
    val tag_o = Output(UInt(tagWidth.W))
    val out_valid_o = Output(Bool())
    val out_ready_i = Input(Bool())
    val busy_o = Output(Bool())
}

class FPNewBlackbox(ftype: FPNewFType.FPNewFType,
                    lanes: Int,
                    stages: Int,
                    tagWidth: Int,
                   ) extends BlackBox(Map()) with HasBlackBoxResource {
  val fLen = FPNewFType.toWidth(ftype) * lanes

  val modulePrefix = ftype match {
    case FPNewFType.B16 => "B"
    case FPNewFType.HalfPrecision => "H"
    case FPNewFType.FullPrecision => "S"
    case FPNewFType.DoublePrecision => "D"
  }
  val io = IO(new FPNBlackboxIO(fLen, tagWidth)).suggestName("io")

  // addResource(s"/fpnew/FPNewBlackbox_${floatType.kind().toString()}${lanes}l${stages}s.synth.v")
  // addResource(s"/fpnew/FPNewBlackbox_B1l0s.synth.v")
  val resourceModule = s"FPNewBlackbox_$modulePrefix${lanes}l${stages}s_synth"
  val resourceName = s"${resourceModule}.v"

  override def parentModName: String = resourceModule

  override val desiredName = resourceModule
  addResource(s"/fpnew_scripts/$resourceName") //Manually type B,H,S,D

}
