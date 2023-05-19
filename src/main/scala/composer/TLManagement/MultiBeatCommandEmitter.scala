package composer.TLManagement

import chisel3._
import chisel3.util._
import composer.ComposerParams.{CoreIDLengthKey, SystemIDLengthKey}
import composer.RoccHelpers.ComposerConsts
import composer.RoccHelpers.ComposerOpcode.ACCEL
import composer.common.{ComposerCommand, ComposerRoccCommand}
import freechips.rocketchip.tilelink.TLBundle

class MultiBeatCommandEmitter[T <: ComposerCommand](gen: T) extends Module {
  val in: DecoupledIO[T] = IO(Flipped(Decoupled(gen)))
  val out = IO(Decoupled(new ComposerRoccCommand))
  if (gen.isInstanceOf[ComposerRoccCommand]) {
    in <> out
  } else {
    val command = Reg(gen)
    val s_idle :: s_send :: Nil = Enum(2)
    val state = RegInit(s_idle)
    in.ready := state === s_idle
    out.bits := DontCare
    out.valid := state === s_send

    val nBeats = gen.getNBeats
    val beatCount = Reg(UInt(log2Up(nBeats).W))

    when(state === s_idle) {
      when (in.fire) {
        command := in.bits
        beatCount := 0.U
        state := s_send
      }
    }.elsewhen(state === s_send) {
      val beats = command.getRoccPayload(beatCount)
      out.bits.payload1 := beats._1
      out.bits.payload2 := beats._2
      out.bits.inst.xd := beatCount === (nBeats-1).U
      out.bits.inst.opcode := ACCEL
      out.bits.inst.funct := 0.U
      out.bits.inst.core_id := in.bits.getCoreID
      out.bits.inst.system_id := in.bits.getSystemID
      out.valid := true.B
      when (out.fire) {
        beatCount := beatCount + 1.U
        when (beatCount === (nBeats-1).U) {
          state := s_idle
        }
      }
    }
  }
}
