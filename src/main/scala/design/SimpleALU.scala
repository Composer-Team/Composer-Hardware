package design

import chipsalliance.rocketchip.config._
import composer._
import chisel3._
import chisel3.util._


class SimpleALU(composerCoreParams: ComposerCoreParams)(implicit p: Parameters) extends ComposerCore(composerCoreParams){
  val s_idle :: s_working :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val op = RegInit(0.U(io.req.bits.inst.funct.getWidth.W))
  val a = RegInit(0.U(io.req.bits.rs1.getWidth.W))
  val b = RegInit(0.U(io.req.bits.rs2.getWidth.W))
  val result = RegInit(0.U(io.resp.bits.data.getWidth.W))

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
      op := io.req.bits.inst.rs1
      a := io.req.bits.rs1
      b := io.req.bits.rs2
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
    io.resp.bits.data := result
    io.resp.valid := true.B
    when(io.resp.fire) {
      state := s_idle
    }
  }
}
