package composer.Systems

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.{FixedPoint, Interval}
import chisel3.util._
import composer.Generation.CppGeneration
import composer.common._

class ComposerCommandBundler[T1 <: ComposerCommand, T2 <: ComposerUserResponse](bundleIn: T1, bundleOut: T2, composerCoreWrapper: ComposerCoreWrapper)(implicit p: Parameters) extends Module {
  if (composerCoreWrapper.composerSystemParams.canReceiveSoftwareCommands)
    CppGeneration.addUserCppFunctionDefinition(composerCoreWrapper.composerSystemParams.name, bundleIn, bundleOut)

  val cio = IO(Flipped(new ComposerCoreIO))
  val io = IO(new CustomIO[T1, T2](bundleIn.cloneType, bundleOut.cloneType))

  io.req.bits.elements.foreach { case (_, data) => data := DontCare }
  io.req.bits.__system_id := composerCoreWrapper.system_id.U
  io.req.bits.__core_id := composerCoreWrapper.core_id.U

  cio.resp.valid := io.resp.valid
  cio.resp.bits.rd := 0.U
  io.resp.ready := cio.resp.ready
  cio.resp.bits.getDataField := io.resp.bits.getDataField


  val s_req_idle :: s_done :: Nil = Enum(2)
  val req_state = RegInit(s_req_idle)
  var reqCounter = RegInit(0.U(log2Up(bundleIn.getNBeats + 1).W))

  val nReqBeatsRequired = bundleIn.getNBeats
  val reqPayload = Reg(Vec(nReqBeatsRequired, UInt(128.W)))

  io.req.valid := req_state === s_done
  cio.req.ready := req_state =/= s_done && io.req.ready
  when(req_state === s_req_idle) {
    when(cio.req.fire) {
      reqCounter := reqCounter + 1.U
      reqPayload(reqCounter) := Cat(cio.req.bits.payload1, cio.req.bits.payload2)
      when(reqCounter === (nReqBeatsRequired - 1).U) {
        req_state := s_done
      }
    }
  }.elsewhen(req_state === s_done) {
    io.req.valid := true.B
    when(io.req.fire) {
      reqCounter := 0.U
      req_state := s_req_idle
    }
  }
  val whole = Cat(reqPayload.reverse)
  io.req.bits.fieldSubranges foreach { sr =>
    val range = sr._2
    val flat = whole(range._1, range._2)
    val field = io.req.bits.elements(sr._1)
    field := typedFlat(field, flat)
  }

  def typedFlat(field: Data, flat: UInt): Data = {
    field match {
      case vector: Vec[Data] =>
        val subField = vector.getElements.head
        val divs = vector.length
        val divSize = flat.getWidth / divs
        VecInit(Seq.tabulate(divs) { idx => flat((idx+1) * divSize - 1, idx * divSize)}.map(subFlat => typedFlat(subField, subFlat)))
      case _: Bool =>
        flat.asBool
      case _: UInt =>
        flat.asUInt
      case _: SInt =>
        flat.asSInt
      case fixedPoint: FixedPoint =>
        flat.asFixedPoint(fixedPoint.binaryPoint)
      case interval: Interval =>
        flat.asInterval(interval.range)
      case _ =>
        flat
    }
  }
}
