package design

import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chisel3._
import chisel3.util._
import composer.{ComposerConstructor, ComposerCore, ComposerCoreParams, ComposerSystemParams, ComposerSystemsKey, ComposerUncachedChannelParams, WithAWSMem, WithComposer}
import composer.ComposerCoreIO

//TODO: Will we ever need to take more inputs in the middle of a calculation? Or only at the beginning?


// * * * * * * * * * * * * * * * * * * * * * * * * * * Simple ALU Implementation * * * * * * * * * * * * * * * * * * *

class SimpleInput extends Bundle {
  val op = UInt(2.W)
  val a = UInt(48.W)
  val b = UInt(48.W)
}
class SimpleOutput extends Bundle {
  val data = UInt(52.W) // TODO: Check size
}

class SimpleCore()(implicit p: Parameters, composerCoreParams: ComposerConstructor) extends ComposerCore(composerCoreParams) {

  val cio = exposeIOModule(new SimpleInput, new SimpleOutput)

  val s_idle :: s_working :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val op = RegInit(0.U(cio.req.bits.op.getWidth.W))
  val a = RegInit(0.U(cio.req.bits.a.getWidth.W))
  val b = RegInit(0.U(cio.req.bits.b.getWidth.W))
  val result = RegInit(0.U(cio.resp.bits.data.getWidth.W))

  cio.req.ready := false.B
  cio.resp.valid := false.B
  cio.resp.bits.data := 0.U
  cio.busy := true.B

  when(state === s_idle) {
    cio.busy := false.B
    cio.req.ready := true.B
    when(cio.req.fire) {
      state := s_working
      op := cio.req.bits.op
      a := cio.req.bits.a
      b := cio.req.bits.b
    }
  }.elsewhen(state === s_working) {
    switch(op) {
      is(0.U) {
        result := a +& b
      }
      is(1.U) {
        result := a -& b
      }
      is(2.U) {
        result := a * b
      }
    }
    state := s_finish
  }.elsewhen(state === s_finish) {
    cio.resp.bits.data := result
    cio.resp.valid := true.B
    when(cio.resp.fire) {
      state := s_idle
    }
  }
}

class WithSimple(withNCores: Int) extends Config((site, here, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
    coreParams = ComposerCoreParams(
      readChannelParams = Seq(),
      writeChannelParams = Seq()
    ),
    nCores = withNCores,
    name = "SimpleSystem",
    buildCore = {
      case (coreParams: ComposerConstructor, parameters: Parameters) =>
        new SimpleCore()(parameters, coreParams)
    }))
})

class SimpleConfig extends Config (
  new WithSimple(1) ++ new WithComposer() ++ new WithAWSMem(1)
)
