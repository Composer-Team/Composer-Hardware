package composer.common

import chisel3._
import chisel3.internal.sourceinfo.SourceInfoTransform
import chisel3.util._
import composer.ComposerParams.{CoreIDLengthKey, SystemIDLengthKey}

import scala.language.experimental.macros


//class ComposerResponse extends Bundle with hasRoccResponseFields {
//  val __system_id = UInt(SystemIDLengthKey.W)
//  val __core_id = UInt(CoreIDLengthKey.W)
//  val __rd = UInt(5.W)
//
//  override def system_id: UInt = __system_id
//
//  override def core_id: UInt = __core_id
//
//  override def rd: UInt = __rd
//
//  val reservedNames = Seq("__system_id", "__core_id", "__rd")
//
//}

object hasRoccResponseFields {
  def apply[T <: hasRoccResponseFields with Bundle](gen: T, in: UInt): T = {
    val wire = Wire(gen)
    // unpack from @pack
    val fsrs = gen.fieldSubranges
    fsrs.foreach { case (name: String, (high: Int, low: Int)) =>
      wire.elements(name) := in(high, low)
    }
    wire.rd match {
      case Some(real_rd) =>
        real_rd := in(68, 64)
      case None => ;
    }
    wire.core_id := in(63 - SystemIDLengthKey, 64 - SystemIDLengthKey - CoreIDLengthKey)
    wire.system_id := in(63, 64 - SystemIDLengthKey)
    wire
  }
}

trait hasDataField {
  val nDataBits = 64 - SystemIDLengthKey - CoreIDLengthKey
  private[composer] def getDataField: UInt
}

class ComposerUserResponse extends Bundle with hasAccessibleUserSubRegions with hasDataField {
  override def sortedElements: Seq[(String, Data)] = elements.toSeq.sortBy(_._1)
  override val reservedNames: Seq[String] = Seq()
  private[composer] def getDataField: UInt = {
    val datacat = Cat(realElements.map(_._2.asUInt).reverse)
    require(datacat.getWidth <= 50, "Defined response is too large to fit in return payload")
    if (datacat.getWidth < 50) {
      Cat(0.U((50-datacat.getWidth).W), datacat)
    } else datacat
  }
}

trait hasRoccResponseFields extends hasAccessibleUserSubRegions with hasDataField {
  def system_id: UInt

  def core_id: UInt

  def rd: Option[UInt]


  private[composer] def packRocc(): UInt = {
    require(getDataField.getWidth == 50)
    rd match {
      case Some(real_rd) =>
        val rd_ext = Cat(0.U((32 - real_rd.getWidth).W), real_rd)
        Cat(rd_ext, system_id, core_id, getDataField)
      case None =>
        Cat(system_id, core_id, getDataField)
    }
  }
}

class ComposerRoccUserResponse extends ComposerUserResponse {
  override val reservedNames: Seq[String] = Seq()
  /**
   * Typically 50 bits
   */
  val data = UInt((64 - SystemIDLengthKey - CoreIDLengthKey).W)

  override def getDataField: UInt = data
}

class ComposerRoccResponse() extends ComposerRoccUserResponse with hasRoccResponseFields {
  override val reservedNames: Seq[String] = Seq("system_id", "core_id", "rd")
  val system_id = UInt(SystemIDLengthKey.W)
  val core_id = UInt(CoreIDLengthKey.W)
  val rd = Some(UInt(5.W))
}

class ComposerInternallyRoutedRoccResponse() extends ComposerRoccUserResponse with hasRoccResponseFields {
  override val rd: Option[UInt] = None
  override val reservedNames: Seq[String] = Seq("system_id", "core_id")
  val system_id = UInt(SystemIDLengthKey.W)
  val core_id = UInt(CoreIDLengthKey.W)
}

object ComposerRoccResponse {
  def apply(a: UInt): ComposerRoccResponse = {
    require(a.getWidth == 32 * 3)
    // unpack from @pack
    val wire = Wire(new ComposerRoccResponse())
    wire.getDataField := a(63 - SystemIDLengthKey - CoreIDLengthKey, 0)
    wire.rd.get := a(68, 64)
    wire.core_id := a(63 - SystemIDLengthKey, 64 - SystemIDLengthKey - CoreIDLengthKey)
    wire.system_id := a(63, 64 - SystemIDLengthKey)
    wire
  }

  val getWidthBits: Int = 32 * 3
  val getWidthBytes: Int = getWidthBits / 8
}

object ComposerInternallyRoutedRoccResponse {
  val getWidthBits: Int = 32 * 2
  val getWidthBytes: Int = getWidthBits / 8

  def apply(a: UInt): ComposerInternallyRoutedRoccResponse = {
    // unpack from @pack
    val wire = Wire(new ComposerInternallyRoutedRoccResponse())
    wire.getDataField := a(63 - SystemIDLengthKey - CoreIDLengthKey, 0)
    wire.core_id := a(63 - SystemIDLengthKey, 64 - SystemIDLengthKey - CoreIDLengthKey)
    wire.system_id := a(63, 64 - SystemIDLengthKey)
    wire
  }

}