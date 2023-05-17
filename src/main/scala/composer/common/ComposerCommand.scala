package composer.common

import chisel3._
import chisel3.internal.requireIsChiselType
import chisel3.util._
import composer.ComposerParams.{CoreIDLengthKey, SystemIDLengthKey}
import composer.RoccHelpers.ComposerOpcode.ACCEL

sealed abstract class AbstractComposerCommand extends Bundle with hasAccessibleUserSubRegions {

  def getCoreID(): UInt
  def getSystemID(): UInt

  def setDestination(coreID: UInt, systemID: UInt) = {
    requireIsChiselType(this, "io type")
    getCoreID() := coreID
    getSystemID() := systemID
  }


  private[composer] def getNBeats(): Int = {
    val real_width = realElements.map(_._2.getWidth).sum
    (real_width.toFloat / 128).ceil.toInt
  }
}

class ComposerCommand extends AbstractComposerCommand {
  override val reservedNames = Seq("__core_id", "__system_id")
  private[composer] val __core_id = UInt(CoreIDLengthKey.W)
  private[composer] val __system_id = UInt(SystemIDLengthKey.W)

  def getCoreID(): UInt = __core_id

  def getSystemID(): UInt = __system_id

  private[composer] def getRoccBeat(idx: Int): UInt = {
    val a: UInt = this.asUInt
    val high = (idx + 1) * 128 - 1
    val low = idx * 128
    if (high > getWidth) {
      Cat(0.U((high + 1 - getWidth).W), a(getWidth - 1, low))
    } else a(high, low)
  }


  private[composer] def getRoccBeat(idx: UInt): ComposerRoccCommand = {
    val nBeats = getNBeats()
    val beats = VecInit((0 until nBeats) map getRoccBeat)
    val roccCommand = Wire(new ComposerRoccCommand)
    roccCommand.inst.core_id := getCoreID()
    roccCommand.inst.system_id := getSystemID()
    roccCommand.inst.opcode := ACCEL
    roccCommand.inst.funct := 0.U
    roccCommand.inst.rd := DontCare
    roccCommand.inst.xs1 := DontCare
    roccCommand.inst.xs2 := DontCare
    beats(idx)
    roccCommand
  }
}

object ComposerCommand {
  def apply[T <: ComposerCommand](in: UInt, gen: T): T = {
    val wire = Wire(gen)
    val fsrs = gen.fieldSubranges
    fsrs.foreach{fsr =>
      val high = fsr._2._1
      val low = fsr._2._2
      wire.elements(fsr._1) := in(high, low)
    }
    wire
  }
}

//noinspection ScalaUnusedSymbol
class ComposerRoccCommand() extends AbstractComposerCommand {
  override val reservedNames = Seq()
  val inst = new Bundle {
    val rd = UInt(5.W)
    val xd = Bool()
    val xs1 = Bool()
    val core_id = UInt(CoreIDLengthKey.W)
    val xs2 = Bool()
    val opcode = UInt(7.W)
    val system_id = UInt(SystemIDLengthKey.W)
    val funct = UInt((7 - SystemIDLengthKey).W)
  }
  val payload1 = UInt(64.W)
  val payload2 = UInt(64.W)

  def pack(bufferToPow2: Boolean = true, withRoutingPayload: Option[UInt] = None): UInt = {
    val s = Cat(inst.rd, inst.core_id, inst.xd, inst.xs1, inst.xs2, inst.opcode, inst.system_id, inst.funct,
      getCoreID, payload1, payload2)
    if (bufferToPow2) {
      val l = 1 << log2Up(s.getWidth)
      withRoutingPayload match {
        case None => Cat(0.U((l - s.getWidth).W), s)
        case Some(pay) =>
          require(pay.getWidth <= l - s.getWidth)
          Cat(0.U((l - s.getWidth - pay.getWidth).W), pay, s)
      }
    } else s
  }

  override def getSystemID(): UInt = inst.system_id

  override def getCoreID(): UInt = inst.core_id

  private[composer] def payloadWidth = payload1.getWidth + payload2.getWidth
}

object ComposerRoccCommand {
  /** unpack from ComposerRoccCommand.pack() */
  def apply(a: UInt): ComposerRoccCommand = {
    val b = Wire(new ComposerRoccCommand())
    b.payload2 := a(63, 0)
    b.payload1 := a(127, 64)
    b.inst.funct := a(134 - SystemIDLengthKey, 128)
    b.inst.system_id := a(134, 135 - SystemIDLengthKey)
    b.inst.opcode := a(141, 135)
    b.inst.xs2 := a(142)
    b.inst.xs1 := a(143)
    b.inst.xd := a(144)
    b.inst.core_id := a(154, 145)
    b.inst.rd := a(159, 155)
    b
  }

  val packLengthBytes = 160 / 8
}
