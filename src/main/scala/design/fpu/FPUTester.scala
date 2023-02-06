package design.fpu

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import composer._
import design.Composer
import fpnewWrapper._
import fpnewWrapper._

case class fpuParams(ftype: FPFloatFormat.Type)

class FPUTester(cp: ComposerConstructor, fparams: fpuParams)(implicit p: Parameters) extends ComposerCore(cp) {
  val numLanes = 1
  val numStages = 2

  val fpu = Module(new FPUNew(fparams.ftype,
    lanes = numLanes,
    stages = numStages,
    supportedOps = Seq(FPNewOpClass.ADDMUL), tagWidth = 1))

  val s_idle :: s_wait_for_input_fire :: s_wait_for_output_fire :: s_finishing :: Nil = Enum(4)
  val state = RegInit(s_idle)

  // default tie-offs
  io.req.ready := state === s_idle
  io.resp.valid := state === s_finishing
  io.resp.bits := DontCare

  // a, b, and c operands
  val fwidth = FPFloatFormat.toWidth(fparams.ftype)
  val operands = Seq.fill(3)(Seq.fill(numLanes)(Reg(UInt(fwidth.W))))
  fpu.io.req.bits.operands(0) := operands(0)(0)
  fpu.io.req.bits.operands(1) := operands(1)(0)
  fpu.io.req.bits.operands(2) := operands(2)(0)
  fpu.io.req.bits.tag := 0.U
  println(fwidth)
  val opTy = Reg(new FPOperation.Type)
  val opMod = Reg(Bool())
  val srcFormat = Reg(new FPFloatFormat.Type)
  val dstFormat = Reg(new FPFloatFormat.Type)
  val intFormat = Reg(new FPIntFormat.Type)
  val roundingMode = Reg(new FPRoundingMode.Type)

  object FPStartCommands extends ChiselEnum {
    val OpA, OpB, OpC, OpTy, OpMod, srcFormat, dstFormat, intFormat, roundingMode, GO = Value
  }

  // you can add any other user definitions you'd like to use in C++ here
  CppGeneration.exportChiselEnum(FPStartCommands)
  CppGeneration.exportChiselEnum(FPFloatFormat)
  CppGeneration.exportChiselEnum(FPIntFormat)
  CppGeneration.exportChiselEnum(FPRoundingMode)
  CppGeneration.exportChiselEnum(FPOperation)

  fpu.io.req.bits.op := opTy
  fpu.io.req.bits.opModifier := opMod
  fpu.io.req.bits.srcFormat := srcFormat
  fpu.io.req.bits.dstFormat := dstFormat
  fpu.io.req.bits.intFormat := intFormat
  fpu.io.req.bits.roundingMode := roundingMode

  fpu.io.req.valid := false.B
  fpu.io.resp.ready := false.B
  fpu.io.flush := false.B


  val results = Seq.fill(numLanes)(Reg(UInt(fwidth.W)))

  switch(state) {
    is(s_idle) {
      when(io.req.fire) {
        val whichLanes = io.req.bits.inst.rs2
        // when rs1 is [0, 3)
        operands.zipWithIndex.foreach { case (ops, op_idx) => // 0 is a, 1 is b, 2 is c
          when(io.req.bits.inst.rs1 === op_idx.U) {
            val laneChoose = VecInit(ops)
            laneChoose(whichLanes) := io.req.bits.payload2(fwidth - 1, 0)
          }
        }
        // when rs1 is 3, start
        switch(FPStartCommands(io.req.bits.inst.rs1(FPStartCommands.getWidth - 1, 0))) {
          is(FPStartCommands.OpA) {
            operands(0)(0) := io.req.bits.payload2(fwidth - 1, 0)
          }
          is(FPStartCommands.OpB) {
            operands(1)(0) := io.req.bits.payload2(fwidth - 1, 0)
          }
          is(FPStartCommands.OpC) {
            operands(2)(0) := io.req.bits.payload2(fwidth - 1, 0)
          }
          is(FPStartCommands.OpTy) {
            opTy := FPOperation(io.req.bits.payload2(opTy.getWidth - 1, 0))
          }
          is(FPStartCommands.OpMod) {
            opMod := io.req.bits.payload2(opMod.getWidth - 1, 0)
          }
          is(FPStartCommands.srcFormat) {
            srcFormat := FPFloatFormat(io.req.bits.payload2(srcFormat.getWidth - 1, 0))
          }
          is(FPStartCommands.dstFormat) {
            dstFormat := FPFloatFormat(io.req.bits.payload2(dstFormat.getWidth - 1, 0))
          }
          is(FPStartCommands.intFormat) {
            intFormat := FPIntFormat(io.req.bits.payload2(intFormat.getWidth - 1, 0))
          }
          is(FPStartCommands.roundingMode) {
            roundingMode := FPRoundingMode(io.req.bits.payload2(roundingMode.getWidth - 1, 0))
          }
          is(FPStartCommands.GO) {
            state := s_wait_for_input_fire
          }
        }
      }
    }

    is(s_wait_for_input_fire) {
      fpu.io.req.valid := true.B
      when(fpu.io.req.fire) {
        state := s_wait_for_output_fire
      }
    }

    is(s_wait_for_output_fire) {
      fpu.io.resp.ready := true.B
      when(fpu.io.resp.fire) {
        results.zipWithIndex.foreach { case (res, idx) =>
          res := fpu.io.resp.bits.result((idx + 1) * fwidth - 1, idx * fwidth)
        }
        state := s_finishing
      }
    }
    is(s_finishing) {
      io.resp.bits.data := Cat(results)
      require(Cat(results).getWidth <= io.resp.bits.data.getWidth, "Your response is too big to fit in response data field")
      when(io.resp.fire) {
        state := s_idle
      }
    }
  }
}

class withFPU extends Config((site, _, up) => {
  case ComposerSystemsKey =>
    List(ComposerSystemParams(nCores = 1,
      name = "FPUNew",
      buildCore = {
        case (constructor, params) =>
          new FPUTester(constructor, fpuParams(FPFloatFormat.Fp32))(params)
      }))
})

class MyFPUConfig extends Config(new withFPU ++ new WithComposer ++ new WithNoMem)

object FPUApp extends App {
  Composer.buildConfig(new MyFPUConfig)
}
