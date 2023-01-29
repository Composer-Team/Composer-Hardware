package fpnewWrapper.fpnew

import chisel3.experimental.ChiselEnum

//noinspection ScalaUnusedSymbol
object FPFloatFormat extends ChiselEnum {
  val Fp32, Fp64, Fp16, Fp8, Fp16Alt = Value
}

//noinspection ScalaUnusedSymbol
object FPIntFormat extends ChiselEnum {
  val Int8, Int16, Int32, Int64 = Value
}

//noinspection ScalaUnusedSymbol
object FPOperation extends ChiselEnum {
  val FMADD, FNMSUB, ADD, MUL, DIV, SQRT, SGNJ, MINMAX, CMP, CLASSIFY, F2F, F2I,
  I2F, CPKAB, CPKCD = Value
}

//noinspection ScalaUnusedSymbol
object FPRoundingMode extends ChiselEnum {
  val RNE, RTZ, RDN, RUP, RMM, DYN = Value
}

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
