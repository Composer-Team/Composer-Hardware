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
    wire.rd := in(63 - SystemIDLengthKey - CoreIDLengthKey, 59 - SystemIDLengthKey - CoreIDLengthKey)
    wire.core_id := in(63 - SystemIDLengthKey, 64 - SystemIDLengthKey - CoreIDLengthKey)
    wire.system_id := in(63, 64 - SystemIDLengthKey)
    wire
  }
}

class ComposerUserResponse extends Bundle with hasAccessibleUserSubRegions {
  override val reservedNames: Seq[String] = Seq()
  def data_field: UInt = {
    val datacat = Cat(realElements.map(_._2.asUInt))
    require(datacat.getWidth <= 50, "Defined response is too large to fit in return payload")
    if (datacat.getWidth < 50) {
      Cat(0.U((50-datacat.getWidth).W), datacat)
    } else datacat
  }
}

trait hasRoccResponseFields extends hasAccessibleUserSubRegions {
  def system_id: UInt

  def core_id: UInt

  def rd: UInt

  val nDataBits = 64 - SystemIDLengthKey - CoreIDLengthKey

  private[composer] def pack(): UInt = {
    val data_eles = this.realElements
    require(data_eles.map(_._2.getWidth).sum <= 64 - SystemIDLengthKey - CoreIDLengthKey)
    Cat(system_id, core_id, rd, Cat(data_eles.map(_._2.asUInt)))
  }
}

class ComposerRoccUserResponse extends ComposerUserResponse {
  override val reservedNames: Seq[String] = Seq()
  /**
   * Typically 50 bits
   */
  override val data_field = UInt((64 - SystemIDLengthKey - CoreIDLengthKey).W)
}

class ComposerRoccResponse() extends ComposerRoccUserResponse with hasRoccResponseFields {
  override val reservedNames: Seq[String] = Seq("system_id", "core_id", "rd")
  val system_id = UInt(SystemIDLengthKey.W)
  val core_id = UInt(CoreIDLengthKey.W)
  val rd = UInt(5.W)
}

object ComposerRoccResponse {
  def apply(a: UInt): ComposerRoccResponse = {
    // unpack from @pack
    val wire = Wire(new ComposerRoccResponse())
    wire.data_field := a(58 - SystemIDLengthKey - CoreIDLengthKey, 0)
    wire.rd := a(63 - SystemIDLengthKey - CoreIDLengthKey, 59 - SystemIDLengthKey - CoreIDLengthKey)
    wire.core_id := a(63 - SystemIDLengthKey, 64 - SystemIDLengthKey - CoreIDLengthKey)
    wire.system_id := a(63, 64 - SystemIDLengthKey)
    wire
  }

  val getWidthBits: Int = 64
  val getWidthBytes: Int = getWidthBits / 8
}
