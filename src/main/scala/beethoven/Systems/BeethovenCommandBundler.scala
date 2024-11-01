package beethoven.Systems

import beethoven.CustomIO
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.{FixedPoint, Interval}
import chisel3.util._
import beethoven.Generation.CppGeneration
import beethoven.Protocol.RoCC.RoccExchange
import beethoven.common._

class BeethovenCommandBundler[T1 <: AccelCommand, T2 <: AccelResponse](bundleIn: T1,
                                                                       bundleOut: T2,
                                                                       outer: AcceleratorSystem,
                                                                       expectResponse: Boolean,
                                                                       opCode: Int)(implicit p: Parameters) extends Module {
  if (outer.systemParams.canReceiveSoftwareCommands)
    CppGeneration.addUserCppFunctionDefinition(outer.systemParams.name, bundleIn, bundleOut, opCode)

  val cio = IO(Flipped(new RoccExchange))
  val io = IO(new CustomIO[T1, T2](bundleIn.cloneType, bundleOut.cloneType, !bundleOut.isInstanceOf[InvalidAccelResponse]))
  val io_am_active = IO(Bool())

  if (expectResponse) {
    val am_active = RegInit(false.B)
    when(io.req.fire) {
      am_active := true.B
    }
    when(cio.resp.fire) {
      am_active := false.B
    }
    io_am_active := am_active
  } else {
    io_am_active := false.B
  }
  io.req.bits.elements.foreach { case (_, data) => data := DontCare }
  io.req.bits.getSystemID := outer.system_id.U
  io.req.bits.getCoreID := DontCare

  io._resp match {
    case Some(resp_io) =>
      cio.resp.valid := resp_io.valid
      cio.resp.bits.rd := 0.U
      cio.resp.bits.core_id := DontCare
      cio.resp.bits.system_id := DontCare
      resp_io.ready := cio.resp.ready
      cio.resp.bits.getDataField := resp_io.bits.getDataField
    case None =>
      cio.resp.bits := DontCare
      cio.resp.valid := false.B
  }


  val s_req_idle :: s_done :: Nil = Enum(2)
  val req_state = RegInit(s_req_idle)
  var reqCounter = Reg(UInt(log2Up(bundleIn.getNBeats + 1).W))
  when(reset.asBool) {
    reqCounter := 0.U
  }

  val nReqBeatsRequired = bundleIn.getNBeats

  if (nReqBeatsRequired > 0) {
    val reqPayload = Reg(Vec(nReqBeatsRequired, UInt(128.W)))

    io.req.valid := req_state === s_done
    cio.req.ready := req_state =/= s_done
    when(req_state === s_req_idle) {
      when(cio.req.fire) {
        val reqsRecieved = reqCounter
        reqCounter := reqCounter + 1.U
        reqPayload(reqsRecieved) := Cat(cio.req.bits.payload1, cio.req.bits.payload2)
        when(reqsRecieved === (nReqBeatsRequired - 1).U) {
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
      field match {
        case address: Address =>
          address.address := typedFlat(field, flat)
        case _ =>
          field := typedFlat(field, flat)
      }
    }
  } else {
    io.req.valid := cio.req.valid
    io.req.bits := DontCare
    cio.req.ready := io.req.ready
  }

  def typedFlat(field: Data, flat: UInt): Data = {
    field match {
      case vector: Vec[_] =>
        val subField = vector.getElements.head
        val divs = vector.length
        val divSize = flat.getWidth / divs
        VecInit(Seq.tabulate(divs) { idx => flat((idx + 1) * divSize - 1, idx * divSize) }.map(subFlat => typedFlat(subField, subFlat)))
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
