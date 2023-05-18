package composer.TLManagement

import chisel3._
import chisel3.util._
import composer.common.{ComposerRoccResponse, ComposerUserResponse, hasAccessibleUserSubRegions}

class ComposerRespConverter[T <: ComposerUserResponse](gen: T) extends Module {
  val in = IO(Input(Flipped(Decoupled(new ComposerRoccResponse))))
  val out = IO(Output(Decoupled(gen)))

  in.ready := out.ready
  out.valid := in.valid

  val w = hasAccessibleUserSubRegions.apply[T](in.bits.getDataField, gen)
  out.bits := w
}
