import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.util._
import composer._
import composer.MemoryStreams._

//TODO: Will we ever need to take more inputs in the middle of a calculation? Or only at the beginning?


// * * * * * * * * * * * * * * * * * * * * * * * * * * Simple ALU Implementation * * * * * * * * * * * * * * * * * * *

class SimpleInput extends Bundle {
//  val op = UInt(2.W)
//  val a = UInt(48.W)
//  val b = UInt(48.W)
  val op = UInt(8.W)
  val a = UInt(120.W)
  val b = UInt(120.W)
}
class SimpleOutput extends Bundle {
  val result = UInt(52.W) // TODO: Check size
}

class SimpleCore(composerCoreParams: ComposerConstructor)(implicit p: Parameters) extends ComposerCore(composerCoreParams) {

  val io = getIO(new SimpleInput, new SimpleOutput)

  val s_idle :: s_working :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val op = RegInit(0.U(io.req.bits.op.getWidth.W))
  val a = RegInit(0.U(io.req.bits.a.getWidth.W))
  val b = RegInit(0.U(io.req.bits.b.getWidth.W))
  val result = RegInit(0.U(io.resp.bits.result.getWidth.W))

  io.req.ready := false.B
  io.resp.valid := false.B
  io.resp.bits.result := 0.U
  io.busy := true.B

  when(state === s_idle) {
    io.busy := false.B
    io.req.ready := true.B
    when(io.req.fire) {
      state := s_working
      op := io.req.bits.op
      a := io.req.bits.a
      b := io.req.bits.b
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
    io.resp.bits.result := result
    io.resp.valid := true.B
    when(io.resp.fire) {
      state := s_idle
    }
  }
}

class WithSimple(withNCores: Int) extends Config((site, here, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
    coreParams = ComposerCoreParams(
      memoryChannelParams = List(
        CReadChannelParams("ReadChannel", 1),
        CWriteChannelParams("WriteChannel", 1))
    ),
    nCores = withNCores,
    name = "SimpleSystem",
    buildCore = {
      case (coreParams: ComposerConstructor, parameters: Parameters) =>
        new SimpleCore(coreParams)(parameters)
    }))
})

class SimpleConfig extends Config (
  new WithSimple(1) ++ new WithComposer() ++ new WithAWSPlatform(1)
)

object TestMyCore extends App {
  TestDriver.buildConfig(new SimpleConfig)
}