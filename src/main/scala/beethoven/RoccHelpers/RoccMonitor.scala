package beethoven.RoccHelpers

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._

import freechips.rocketchip.tile.{RoCCCommand, RoCCResponse}

class RoccMonitor(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new RoCCCommand))
    val resp = Decoupled(new RoCCResponse)
    val busy = Input(Bool())
  })

  val s_idle :: s_wait :: s_resp :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val rd = Reg(UInt(5.W))

  io.cmd.ready := state === s_idle
  io.resp.valid := state === s_resp
  io.resp.bits.rd := rd
  io.resp.bits.data := 0.U

  when(io.cmd.fire) {
    rd := io.cmd.bits.inst.rd
    state := s_wait
  }

  when(state === s_wait && !io.busy) {
    state := s_resp
  }

  when(io.resp.fire) {
    state := s_idle
  }
}
