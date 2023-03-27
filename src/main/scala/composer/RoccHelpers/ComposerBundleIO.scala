package composer.RoccHelpers

import scala.math.ceil
import Chisel.Cat
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import composer.{ComposerCoreIO, ComposerCoreParams, CustomIO}

class ComposerBundleIO[T1 <: Bundle, T2 <: Bundle](bundleIn: T1, bundleOut: T2, composerCoreParams: ComposerCoreParams)(implicit p: Parameters) extends Module {

  val cio = IO(new ComposerCoreIO)
  val io = IO(new CustomIO(bundleIn.cloneType, bundleOut.cloneType))

  //zero initialize values
  io.req.bits.elements.foreach { case (_, data) => data := 0.U }
  cio.resp.bits.elements.foreach { case (_, data) => data := 0.U }

  cio.busy := io.busy

  cio.resp.valid := io.resp.valid
  io.resp.ready := cio.resp.ready

  when(io.resp.fire) {
    val payload = io.resp.bits.asUInt
    loadOutputPayload(payload, cio.resp.bits.data)
  }

  val deliveringReqPayload = RegInit(false.B)
  val packagingPayload = RegInit(false.B)

  val reqPayloadWidth = Cat(cio.req.bits.payload1, cio.req.bits.payload2).getWidth
  val reqInputWidth = io.req.bits.getWidth
  val extendedPayloadWidth = reqPayloadWidth*ceil(reqInputWidth.toDouble/reqPayloadWidth).toInt

  var reqPayloadCounter = RegInit(0.U(64.W)) //TODO: Pick Width
  val reqPayload = RegInit(VecInit(Seq.fill(extendedPayloadWidth)(0.B)))
  val reqPayloadChunk = VecInit(Seq.fill(reqPayloadWidth)(0.B))

  io.req.valid := cio.req.valid
  cio.req.ready := io.req.ready && !packagingPayload
  when(cio.req.fire) {
    deliveringReqPayload := true.B
    io.req.valid := false.B

    reqPayloadCounter := reqPayloadCounter + reqPayloadWidth.U
    reqPayloadChunk := Cat(cio.req.bits.payload1, cio.req.bits.payload2).asBools
    for (i <- 0 until reqPayloadWidth) {
      reqPayload(reqPayloadCounter + i.U) := reqPayloadChunk(0 + i)
    }
  }

  when(deliveringReqPayload) {
    packagingPayload := (reqPayloadCounter >= reqInputWidth.U)
    io.req.valid := false.B
  }

  when(packagingPayload) {
    reqPayloadCounter := 0.U
    io.req.bits := reqPayload.asTypeOf(io.req.bits)
    io.req.valid := true.B
    deliveringReqPayload := false.B
    packagingPayload := false.B
  }

  def loadOutputPayload(payload: UInt, output: UInt): Unit = {
    val payloadLen = payload.getWidth
    val availableLen = output.getWidth
    if(availableLen < payloadLen)
      throw new Exception(s"Not enough payload! Available: $availableLen. Required: $payloadLen")

    output := (if (payloadLen == availableLen) payload else Cat(0.U((availableLen - payloadLen).W), payload))
  }
}
