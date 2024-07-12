package beethoven.Protocol.tilelink
import beethoven.Protocol.tilelink.TLSlave
import chisel3._
import chisel3.util._
import beethoven.common.{AccelResponse, AccelRoccResponse, hasAccessibleUserSubRegions, hasDataField}

class RespConverter[
    outT <: AccelResponse,
    inT <: Bundle with hasDataField
](genOut: outT, genIn: inT)
    extends Module {
  val in = IO(Flipped(Decoupled(genIn)))
  val out = IO(Decoupled(genOut))

  in.ready := out.ready
  out.valid := in.valid

  val w = hasAccessibleUserSubRegions.apply[outT](in.bits.getDataField, genOut)
  w.rd := 0.U
  out.bits := w
}
