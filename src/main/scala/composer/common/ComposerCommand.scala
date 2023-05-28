package composer.common

import chisel3._
import chisel3.util._
import composer.ComposerParams.{CoreIDLengthKey, SystemIDLengthKey}
import composer.RoccHelpers.ComposerConsts
import freechips.rocketchip.tile.RoCCCommand

sealed abstract class AbstractComposerCommand extends Bundle with hasAccessibleUserSubRegions {

  def getCoreID: UInt
  def getSystemID: UInt

  def setDestination(coreID: UInt, systemID: UInt): Unit = {
//    requireIsChiselType(this, "io type")
    getCoreID := coreID
    getSystemID := systemID
  }


  private[composer] def getNBeats: Int = {
    val real_width = realElements.map(_._2.getWidth).sum
    (real_width.toFloat / 128).ceil.toInt
  }

  /**
   * split into 128b payloads
   */
  private[composer] def getRoccBeats: Seq[UInt] = {
    val whole = Cat(realElements.map(_._2.asUInt).reverse)
    val nBeats = getNBeats
    (0 until nBeats) map {idx =>
      val high = (idx + 1) * 128 - 1
      val low = idx * 128
      if (high + 1 > whole.getWidth) {
        Cat(0.U((high + 1 - whole.getWidth).W), whole(whole.getWidth - 1, low))
      } else whole(high, low)
    }
  }
}

class ComposerCommand extends AbstractComposerCommand {
  override val reservedNames = Seq("__core_id", "__system_id")
  private[composer] val __core_id = UInt(CoreIDLengthKey.W)
  private[composer] val __system_id = UInt(SystemIDLengthKey.W)

  override def sortedElements: Seq[(String, Data)] = elements.toSeq.sortBy(_._1)

  def getCoreID: UInt = __core_id

  def getSystemID: UInt = __system_id

  private[composer] def getRoccPayload(idx: UInt): (UInt, UInt)  = {
    val beats = VecInit(this.getRoccBeats)
    (beats(idx)(127, 64), beats(idx)(63, 0))
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

  override def sortedElements: Seq[(String, Data)] = elements.toSeq
  // DO NOT CHANGE OPERAND ORDERING
  val inst = new Bundle with hasAccessibleUserSubRegions {
    override val reservedNames: Seq[String] = Seq.empty
    override def sortedElements: Seq[(String, Data)] = elements.toSeq
    val rd = UInt(5.W)
    val core_id = UInt(CoreIDLengthKey.W)
    val xd = Bool()
    val xs1 = Bool()
    val xs2 = Bool()
    val opcode = UInt(7.W)
    val system_id = UInt(SystemIDLengthKey.W)
    val funct = UInt((7 - SystemIDLengthKey).W)
  }
  val payload1 = UInt(64.W)
  val payload2 = UInt(64.W)


  def pack(bufferToPow2: Boolean = true, withRoutingPayload: Option[UInt] = None): UInt = {
    println(fieldSubranges)
    val s = Cat(inst.rd, inst.core_id, inst.xd, inst.xs1, inst.xs2, inst.opcode, inst.system_id, inst.funct,
      payload1, payload2)
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

  override def getSystemID: UInt = inst.system_id

  override def getCoreID: UInt = inst.core_id

  private[composer] def payloadWidth = payload1.getWidth + payload2.getWidth
}

object ComposerRoccCommand {
  val packLengthBytes = 160 / 8
  def fromRoccCommand(gen: RoCCCommand): ComposerRoccCommand = {
    val wr = Wire(new ComposerRoccCommand)
    wr.inst.xd := gen.inst.xd
    wr.inst.funct := gen.inst.funct.tail(SystemIDLengthKey)
    wr.inst.system_id := gen.inst.funct.head(SystemIDLengthKey)
    wr.inst.core_id := Cat(gen.inst.rs1, gen.inst.rs2)
    wr.inst.opcode := gen.inst.opcode
    wr.inst.xs1 := gen.inst.xs1
    wr.inst.xs2 := gen.inst.xs2
    wr.payload1 := gen.rs1
    wr.payload2 := gen.rs2
    wr.inst.rd := gen.inst.rd
    wr
  }
}

