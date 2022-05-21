package design

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import composer.{ComposerCore, ComposerCoreParams}

import scala.util.Random

case class LFSRConfig(length: Int, taps: Seq[Int])

// galois lfsr
class LFSRCore(composerCoreParams: ComposerCoreParams)(implicit p: Parameters) extends ComposerCore(composerCoreParams) {
  val s_idle :: s_working :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val conf = p(LFSRConfigKey)
  val taps = conf.taps map (_ - 1)
  val rand_gen = new Random()

  val lfsr = Seq.fill(conf.length)(RegInit(rand_gen.nextBoolean().B))
  val outputbit = lfsr(0)
  val outputCache = Seq.fill(conf.length)(RegInit(false.B))
  val count = RegInit(0.U(log2Up(conf.length).W))

  io.req.ready := false.B
  io.resp.valid := false.B
  io.resp.bits.data := 0.U
  io.resp.bits.rd := 0.U
  io.busy := true.B

  when (state === s_idle) {
    io.busy := false.B
    io.req.ready := true.B
    when (io.req.fire) {
      state := s_working
      count := (conf.length-1).U
    }
  }.elsewhen(state === s_working) {
    (0 until conf.length-1).foreach{i =>
      lfsr(i) := lfsr(i+1)
      outputCache(i) := outputCache(i+1)
    }
    outputCache(conf.length-1) := outputbit
    taps foreach {tap =>
      if (tap == (conf.length-1)) {
        lfsr(tap) := outputbit
      } else {
        lfsr(tap) := lfsr(tap+1) ^ outputbit
      }
    }
    when(count === 0.U) {
      state := s_finish
    }
  }.elsewhen(state === s_finish) {
    io.resp.bits.data := VecInit(outputCache).asUInt
    io.resp.valid := true.B
    when(io.resp.fire) {
      state := s_idle
    }
  }
}
