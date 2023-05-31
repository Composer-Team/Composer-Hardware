package composer.TLManagement

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.RoccHelpers.ComposerOpcode.ACCEL
import composer.Systems.{CustomIO, CustomIOWithRouting}
import composer.common._

class MultiBeatCommandEmitter[T <: ComposerCommand](gen: T)(implicit
    p: Parameters
) extends Module {
  val in: DecoupledIOWithCRouting[T] = IO(Flipped(DecoupledIOWithCRouting(gen)))
  val out = IO(Decoupled(new ComposerRoccCommand))
  if (gen.isInstanceOf[ComposerRoccCommand]) {
    in <> out
  } else {
    val command = Reg(gen)
    val expect_response = Reg(Bool())
    val s_idle :: s_send :: Nil = Enum(2)
    val state = RegInit(s_idle)
    in.ready := state === s_idle
    out.bits := DontCare
    out.valid := state === s_send

    val nBeats = gen.getNBeats
    val beatCount = Reg(UInt(log2Up(nBeats).W))

    when(state === s_idle) {
      when(in.fire) {
        command := in.bits
        beatCount := 0.U
        state := s_send
        expect_response := in.expectResponse
      }
    }.elsewhen(state === s_send) {
      val beats = command.getRoccPayload(beatCount)
      out.bits.payload1 := beats._1
      out.bits.payload2 := beats._2
      out.bits.inst.xd := Mux(
        expect_response,
        beatCount === (nBeats - 1).U,
        0.U
      )
      out.bits.inst.opcode := ACCEL
      out.bits.inst.funct := 0.U
      out.bits.inst.core_id := command.getCoreID
      out.bits.inst.system_id := command.getSystemID
      out.valid := true.B
      when(out.fire) {
        beatCount := beatCount + 1.U
        when(beatCount === (nBeats - 1).U) {
          state := s_idle
        }
      }
    }
  }
}

class ComposerIntraCoreIOModule[
    Tcmd <: ComposerCommand,
    Tresp <: ComposerUserResponse
](target: String, genCmd: Tcmd, genResp: Tresp)(implicit p: Parameters)
    extends Module {
  override val desiredName = s"Composer${target}CmdRespHandler"
  private val cmdModule = Module(new MultiBeatCommandEmitter(genCmd))
  private val respModule = Module(
    new ComposerRespConverter[Tresp, ComposerRoccResponse](
      genResp,
      new ComposerRoccResponse
    )
  )

  val out = IO(Flipped(new CustomIOWithRouting[Tcmd, Tresp](genCmd, genResp)))

  val cmdIO = IO(Decoupled(new ComposerRoccCommand()))
  val respIO = IO(Flipped(Decoupled(new ComposerRoccResponse)))

  cmdIO <> cmdModule.out
  respModule.in <> respIO

  out.req <> cmdModule.in
  out.resp <> respModule.out
}
