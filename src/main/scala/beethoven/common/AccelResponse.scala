package beethoven.common

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import beethoven.BeethovenParams.{CoreIDLengthKey, SystemIDLengthKey}
import freechips.rocketchip.tile.XLen

import scala.language.experimental.macros

object hasRoccResponseFields {
  /** 96                 64                    0
   * | --C_ID, S_ID, RD |------ data ---------|
   */
  def apply[T <: hasRoccResponseFields with Bundle](gen: T, in: UInt)(implicit p: Parameters): T = {
    val wire = Wire(gen)
    val fsrs = gen.fieldSubranges
    fsrs.foreach { case (name: String, (high: Int, low: Int)) =>
      wire.elements(name) := in(high, low)
    }
    val xlen = p(XLen)
    wire.rd := in(xlen+5 - 1, xlen)
    wire.system_id := in(xlen - 1 + 5 + SystemIDLengthKey, xlen + 5)
    wire.core_id := in(xlen - 1 + 5 + SystemIDLengthKey + CoreIDLengthKey, xlen + 5 + SystemIDLengthKey)
    wire
  }
}

trait hasDataField {
  def nDataBits: Int = 64
  private[beethoven] def getDataField: UInt
}

trait hasRDField {
  val rd: UInt
}

class AccelResponse(val responseName: String) extends Bundle with hasAccessibleUserSubRegions with hasDataField with hasRDField {
  override def sortedElements: Seq[(String, Data)] = elements.toSeq.sortBy(_._1)
  override val reservedNames: Seq[String] = Seq("rd")
  val rd = UInt(5.W)
  private[beethoven] def getDataField: UInt = {
    val datacat = Cat(realElements.map(_._2.asUInt).reverse)
    require(datacat.getWidth <= nDataBits, "Defined response is too large to fit in return payload.\n" +
      "Consider redefining the XLen parameter if this behavior is necessary.")
    if (datacat.getWidth < nDataBits) {
      Cat(0.U((nDataBits-datacat.getWidth).W), datacat)
    } else datacat
  }
}

trait hasRoccResponseFields extends hasAccessibleUserSubRegions with hasDataField {
  val system_id: UInt

  val core_id: UInt

  val rd: UInt


  private[beethoven] def packRocc(): UInt = {
    require(getDataField.getWidth == nDataBits)
    Cat(core_id, system_id, rd, getDataField)
  }
}

class AccelRoccUserResponse(implicit p: Parameters) extends AccelResponse("rocc_response") {
  override val reservedNames: Seq[String] = Seq("rd")
  val data = UInt(p(XLen).W)

  override def getDataField: UInt = data
}

class AccelRoccResponse(implicit p: Parameters) extends AccelRoccUserResponse with hasRoccResponseFields {
  override val reservedNames: Seq[String] = Seq("system_id", "core_id", "rd")
  val system_id = UInt(SystemIDLengthKey.W)
  val core_id = UInt(CoreIDLengthKey.W)
}

object AccelRoccResponse {
  def getWidthBits(implicit p: Parameters): Int = p(XLen) + 32
  def getWidthBytes(implicit p: Parameters): Int = getWidthBits / 8
  def getPow2Bytes(implicit p: Parameters): Int = 1 << log2Up(getWidthBytes)
}