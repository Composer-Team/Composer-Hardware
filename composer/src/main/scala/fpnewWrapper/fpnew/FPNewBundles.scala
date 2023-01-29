package fpnewWrapper.fpnew

import chisel3._
import chisel3.util._

class FPRequest(val fLen: Int, val tagBits: Int) extends Bundle {
  val operands = Vec(3, UInt(fLen.W))
  val roundingMode = FPRoundingMode()
  val op = FPOperation()
  val opModifier = Bool()
  val srcFormat = FPFloatFormat()
  val dstFormat = FPFloatFormat()
  val intFormat = FPIntFormat()
  val tag = UInt(tagBits.W)
}

//noinspection ScalaUnusedSymbol
class FPStatus extends Bundle {
  val NV = Bool() // Invalid
  val DZ = Bool() // Divide by zero
  val OF = Bool() // Overflow
  val UF = Bool() // Underflow
  val NX = Bool() // Inexact
}

class FPResponse(val fLen: Int, val tagWidth: Int) extends Bundle {
  val result = UInt(fLen.W)
  val status = new FPStatus()
  val tag = UInt(tagWidth.W)
}

class FPIO(val fLen: Int, val tagBits: Int) extends Bundle {
  val req = Flipped(Decoupled(new FPRequest(fLen, tagBits)))
  val resp = Decoupled(new FPResponse(fLen, tagBits))
  val flush = Input(Bool())
  val busy = Output(Bool())
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

