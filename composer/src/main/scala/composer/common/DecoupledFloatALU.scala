package composer.common

import composer.common.Float2ALUOp.Float2ALUOp
import composer.common.Float3ALUOp.Float3ALUOp
import chisel3._
import chisel3.util.{Decoupled, Queue, log2Up}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.{FPUFMAPipe, FType}
import hardfloat.RecFNToRecFN

import scala.language.implicitConversions

class Float2Bundle(gen: FType, recoded: Boolean) extends Bundle {
  val a = UInt((if (recoded) gen.recodedWidth else gen.ieeeWidth).W)
  val b = UInt((if (recoded) gen.recodedWidth else gen.ieeeWidth).W)
}

class Float3Bundle(gen: FType, recoded: Boolean) extends Bundle {
  val a = UInt((if (recoded) gen.recodedWidth else gen.ieeeWidth).W)
  val b = UInt((if (recoded) gen.recodedWidth else gen.ieeeWidth).W)
  val c = UInt((if (recoded) gen.recodedWidth else gen.ieeeWidth).W)
}

class DecoupledFloat2ALUIO(gen: FType, recoded: Boolean) extends Bundle {
  val in = Flipped(Decoupled(new Float2Bundle(gen, recoded)))
  val out = Decoupled(UInt((if (recoded) gen.recodedWidth else gen.ieeeWidth).W))
  val isEmpty = Output(Bool())
}

class DecoupledFloat3ALUIO(gen: FType, recoded: Boolean) extends Bundle {
  val in = Flipped(Decoupled(new Float3Bundle(gen, recoded)))
  val out = Decoupled(UInt((if (recoded) gen.recodedWidth else gen.ieeeWidth).W))
  val isEmpty = Output(Bool())
}

object Float2ALUOp extends Enumeration {

  protected case class Val(swap23: Boolean, fmaCmd: Int) extends super.Val

  type Float2ALUOp = Value

  implicit def scalaMagicValueVal(x: Value): Val = x.asInstanceOf[Val]

  val ADD = Val(swap23 = true, 0)
  val SUB = Val(swap23 = true, 1)
  val MUL = Val(swap23 = false, 0)
}

object Float3ALUOp extends Enumeration {

  protected case class Val(fmaCmd: Int) extends super.Val

  type Float3ALUOp = Value

  implicit def scalaMagicValueVal(x: Value): Val = x.asInstanceOf[Val]

  val FMADD = Val(0)
  val FNMADD = Val(3)
  val FMSUB = Val(1)
  val FNMSUB = Val(2)
}


class DecoupledFloat2ALU(gen: FType, recoded: Boolean, operation: Float2ALUOp)(implicit p: Parameters) extends Module {
  val io = IO(new DecoupledFloat2ALUIO(gen, recoded))

  val size_q = 4

  val sub = Module(new FPUFMAPipe(latency = 3, gen))
  val sub_inflight = RegInit(UInt(log2Up(size_q + 1).W), init = 0.U)
  //noinspection DuplicatedCode
  when(io.in.fire && sub.io.out.fire) {
    //nothing
  }.elsewhen(io.in.fire) {
    sub_inflight := sub_inflight + 1.U
  }.elsewhen(sub.io.out.fire) {
    sub_inflight := sub_inflight - 1.U
  }
  val q = Module(new Queue(UInt(gen.recodedWidth.W), size_q, flow = false)) // 3 elements b/c latency = 2
  val sub_ready = q.io.count + sub_inflight < size_q.U

  sub.io.in.valid := io.in.fire
  io.in.ready := sub_ready

  sub.io.in.bits.in1 := (if (recoded) io.in.bits.a else gen.recode(io.in.bits.a))
  sub.io.in.bits.in2 := (if (recoded) io.in.bits.b else gen.recode(io.in.bits.b))
  sub.io.in.bits.in3 := (if (recoded) io.in.bits.b else gen.recode(io.in.bits.b))

  q.io.enq.valid := sub.io.out.valid
  q.io.enq.bits := sub.io.out.bits.data
  //reqdy handled elsewhere

  val out = q.io.deq.bits
  io.out.bits := (if (recoded) out else gen.ieee(out))
  io.out.valid := q.io.deq.valid
  q.io.deq.ready := io.out.ready

  io.isEmpty := q.io.count === 0.U && sub_inflight === 0.U && !sub.io.out.valid


  sub.io.in.bits.rm := hardfloat.consts.round_near_even
  sub.io.in.bits.fmaCmd := operation.fmaCmd.U // I think correct
  //noinspection DuplicatedCode
  sub.io.in.bits.typ := (if (gen == FType.D) 1.U else if (gen == FType.S) 0.U else 2.U) // TODO: make work with non single, double

  sub.io.in.bits.ldst := false.B
  sub.io.in.bits.wen := true.B
  sub.io.in.bits.ren1 := true.B
  sub.io.in.bits.ren2 := true.B
  sub.io.in.bits.ren3 := false.B
  sub.io.in.bits.swap12 := false.B
  sub.io.in.bits.swap23 := operation.swap23.B
  // these used to be singlein/out but they got changed and I can't figure out with these typetags are
  sub.io.in.bits.typeTagIn := (gen == FType.S).B
  sub.io.in.bits.typeTagOut := (gen == FType.S).B
  sub.io.in.bits.fromint := false.B
  sub.io.in.bits.toint := false.B
  sub.io.in.bits.fastpipe := false.B // what is this
  sub.io.in.bits.fma := true.B
  sub.io.in.bits.div := false.B
  sub.io.in.bits.sqrt := false.B
  sub.io.in.bits.wflags := true.B // what is this
}


class DecoupledFloat3ALU(gen: FType, recoded: Boolean, operation: Float3ALUOp)(implicit p: Parameters) extends Module {
  val io = IO(new DecoupledFloat3ALUIO(gen, recoded))

  val size_q = 4

  val sub = Module(new FPUFMAPipe(latency = 3, gen))
  val sub_inflight = RegInit(UInt(log2Up(size_q + 1).W), init = 0.U)

  //noinspection DuplicatedCode
  when(io.in.fire && sub.io.out.fire) {
    //nothing
  }.elsewhen(io.in.fire) {
    sub_inflight := sub_inflight + 1.U
  }.elsewhen(sub.io.out.fire) {
    sub_inflight := sub_inflight - 1.U
  }
  val q = Module(new Queue(UInt(gen.recodedWidth.W), size_q, flow = true)) // 3 elements b/c latency = 2
  val sub_ready = q.io.count + sub_inflight < size_q.U

  sub.io.in.valid := io.in.fire
  io.in.ready := sub_ready

  sub.io.in.bits.in1 := (if (recoded) io.in.bits.a else gen.recode(io.in.bits.a)) //gen.recode(x)
  sub.io.in.bits.in2 := (if (recoded) io.in.bits.b else gen.recode(io.in.bits.b))
  sub.io.in.bits.in3 := (if (recoded) io.in.bits.c else gen.recode(io.in.bits.c))

  q.io.enq.valid := sub.io.out.valid
  q.io.enq.bits := sub.io.out.bits.data
  //reqdy handled elsewhere

  val out = q.io.deq.bits
  io.out.bits := (if (recoded) out else gen.ieee(out))
  io.out.valid := q.io.deq.valid
  q.io.deq.ready := io.out.ready

  io.isEmpty := q.io.count === 0.U && sub_inflight === 0.U && !sub.io.out.valid

  sub.io.in.bits.rm := hardfloat.consts.round_near_even
  sub.io.in.bits.fmaCmd := operation.fmaCmd.U // I think correct
  //noinspection DuplicatedCode
  sub.io.in.bits.typ := (if (gen == FType.D) 1.U else if (gen == FType.S) 0.U else 2.U) // TODO: make work with non single, double

  sub.io.in.bits.ldst := false.B
  sub.io.in.bits.wen := true.B
  sub.io.in.bits.ren1 := true.B
  sub.io.in.bits.ren2 := true.B
  sub.io.in.bits.ren3 := true.B
  sub.io.in.bits.swap12 := false.B
  sub.io.in.bits.swap23 := false.B
  sub.io.in.bits.typeTagIn := (gen == FType.S).B
  sub.io.in.bits.typeTagOut := (gen == FType.S).B
  sub.io.in.bits.fromint := false.B
  sub.io.in.bits.toint := false.B
  sub.io.in.bits.fastpipe := false.B // what is this
  sub.io.in.bits.fma := true.B
  sub.io.in.bits.div := false.B
  sub.io.in.bits.sqrt := false.B
  sub.io.in.bits.wflags := true.B // what is this
}

class DecoupledFloatDivider(gen: FType, recoded: Boolean)(implicit p: Parameters) extends Module {
  val io = IO(new DecoupledFloat2ALUIO(gen, recoded))

  val size_q = 4
  val dividerFtype = FType.D

  val div = Module(new hardfloat.DivSqrtRecF64())
  val div_inflight = RegInit(UInt(log2Up(size_q + 1).W), init = 0.U)
  when(io.in.fire && div.io.outValid_div) {
    //nothing
  }.elsewhen(io.in.fire) {
    div_inflight := div_inflight + 1.U
  }.elsewhen(div.io.outValid_div) {
    div_inflight := div_inflight - 1.U
  }
  val q = Module(new Queue(UInt(dividerFtype.recodedWidth.W), size_q, flow = false))
  val div_ready = q.io.count + div_inflight < size_q.U && div.io.inReady_div

  div.io.inValid := io.in.fire
  io.in.ready := div_ready

  val a_converter = Module(new RecFNToRecFN(gen.exp, gen.sig, dividerFtype.exp, dividerFtype.sig))
  a_converter.io.in := (if (recoded) io.in.bits.a else gen.recode(io.in.bits.a))
  val double_a = a_converter.io.out

  val b_converter = Module(new RecFNToRecFN(gen.exp, gen.sig, dividerFtype.exp, dividerFtype.sig))
  b_converter.io.in := (if (recoded) io.in.bits.b else gen.recode(io.in.bits.b))
  val double_b = b_converter.io.out

  div.io.a := double_a
  div.io.b := double_b

  q.io.enq.valid := div.io.outValid_div
  q.io.enq.bits := div.io.out
  //reqdy handled elsewhere

  val out = q.io.deq.bits
  val out_converter = Module(new RecFNToRecFN(dividerFtype.exp, dividerFtype.sig, gen.exp, gen.sig))
  out_converter.io.in := out
  io.out.bits := (if (recoded) out_converter.io.out else gen.ieee(out_converter.io.out))
  io.out.valid := q.io.deq.valid
  q.io.deq.ready := io.out.ready

  io.isEmpty := q.io.count === 0.U && div_inflight === 0.U && !div.io.outValid_div


  div.io.detectTininess := hardfloat.consts.tininess_afterRounding
  div.io.sqrtOp := false.B
  div.io.roundingMode := hardfloat.consts.round_near_even

  a_converter.io.detectTininess := hardfloat.consts.tininess_afterRounding
  a_converter.io.roundingMode := hardfloat.consts.round_near_even
  b_converter.io.detectTininess := hardfloat.consts.tininess_afterRounding
  b_converter.io.roundingMode := hardfloat.consts.round_near_even
  out_converter.io.detectTininess := hardfloat.consts.tininess_afterRounding
  out_converter.io.roundingMode := hardfloat.consts.round_near_even
}

