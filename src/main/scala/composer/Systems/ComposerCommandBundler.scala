package composer.Systems

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.{FixedPoint, Interval}
import chisel3.util._
import composer.Generation.CppGeneration
import composer.Systems.AcceleratorCore.Address
import composer.common._

class ComposerCommandBundler[T1 <: AccelCommand, T2 <: AccelResponse](bundleIn: T1, bundleOut: T2, outer: ComposerSystem, nSources: Int, opCode: Int)(implicit p: Parameters) extends Module {
  if (outer.systemParams.canReceiveSoftwareCommands)
    CppGeneration.addUserCppFunctionDefinition(outer.systemParams.name, bundleIn, bundleOut, opCode)

  val cio = IO(new Bundle() {
    val cmd = Flipped(new ComposerCoreIO)
    val cmd_in_source = Input(UInt(log2Up(nSources).W))
  })
  val io = IO(new CustomIO[T1, T2](bundleIn.cloneType, bundleOut.cloneType, opCode))

  io.req.bits.elements.foreach { case (_, data) => data := DontCare }
  io.req.bits.getSystemID := outer.system_id.U
  io.req.bits.getCoreID := DontCare

  cio.cmd.resp.valid := io.resp.valid
  cio.cmd.resp.bits.rd := 0.U
  io.resp.ready := cio.cmd.resp.ready
  cio.cmd.resp.bits.getDataField := io.resp.bits.getDataField


  val s_req_idle :: s_done :: Nil = Enum(2)
  val req_state = RegInit(s_req_idle)
  var reqCounter = Reg(Vec(nSources, UInt(log2Up(bundleIn.getNBeats + 1).W)))
  when (reset.asBool) {
    reqCounter.foreach(_ := 0.U)
  }

  val nReqBeatsRequired = bundleIn.getNBeats

  val reqPayload = Reg(Vec(nSources, Vec(nReqBeatsRequired, UInt(128.W))))

  io.req.valid := req_state === s_done
  cio.cmd.req.ready := req_state =/= s_done
  val succeedingSource = Reg(UInt(log2Up(nSources).W))
  when(req_state === s_req_idle) {
    val source = cio.cmd_in_source
    when(cio.cmd.req.fire) {
      val reqsRecieved = reqCounter(source)
      reqCounter(source) := reqCounter(source) + 1.U
      reqPayload(source)(reqsRecieved) := Cat(cio.cmd.req.bits.payload1, cio.cmd.req.bits.payload2)
      when(reqsRecieved === (nReqBeatsRequired - 1).U) {
        req_state := s_done
        succeedingSource := source
      }
    }
  }.elsewhen(req_state === s_done) {
    io.req.valid := true.B
    when(io.req.fire) {
      reqCounter(succeedingSource) := 0.U
      req_state := s_req_idle
    }
  }
  val whole = Cat(reqPayload(succeedingSource).reverse)
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
