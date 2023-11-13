package composer.common

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.ComposerParams.{CoreIDLengthKey, SystemIDLengthKey}
import freechips.rocketchip.tile.{RoCCCommand, XLen}

sealed abstract class AbstractAccelCommand extends Bundle with hasAccessibleUserSubRegions {

  def getCoreID: UInt
  def getSystemID: UInt

  def setDestination(coreID: UInt, systemID: UInt): Unit = {
//    requireIsChiselType(this, "io type")
    getCoreID := coreID
    getSystemID := systemID
  }


  private[composer] def getNBeats: Int = {
    val real_width = realElements.map(_._2.getWidth).sum
    (real_width.toFloat / (2*64)).ceil.toInt
  }

  /**
   * split into 128b payloads
   */
  private[composer] def getRoccBeats: Seq[UInt] = {
    val whole = Cat(realElements.map(_._2.asUInt).reverse)
    val nBeats = getNBeats
    val payloadWidth = 2 * 64
    (0 until nBeats) map {idx =>
      val high = (idx + 1) * payloadWidth - 1
      val low = idx * payloadWidth
      if (high + 1 > whole.getWidth) {
        Cat(0.U((high + 1 - whole.getWidth).W), whole(whole.getWidth - 1, low))
      } else whole(high, low)
    }
  }
}

class AccelCommand extends AbstractAccelCommand {
  override val reservedNames = Seq("__core_id", "__system_id")
  private[composer] val __core_id = UInt(CoreIDLengthKey.W)
  private[composer] val __system_id = UInt(SystemIDLengthKey.W)

  override def sortedElements: Seq[(String, Data)] = elements.toSeq.sortBy(_._1)

  def getCoreID: UInt = __core_id

  def getSystemID: UInt = __system_id

  private[composer] def getRoccPayload(idx: UInt): (UInt, UInt)  = {
    val beats = VecInit(this.getRoccBeats)
    val xlen = 64
    (beats(idx)(2*xlen-1, xlen), beats(idx)(xlen-1, 0))
  }
}

object AccelCommand {
  def apply[T <: AccelCommand](in: UInt, gen: T): T = {
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
class AccelRoccCommand extends AbstractAccelCommand {
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

object AccelRoccCommand {
  def packLengthBytes(implicit p: Parameters): Int = (32 + 2 * p(XLen)) / 8
  def fromRoccCommand(gen: RoCCCommand)(implicit p: Parameters): AccelRoccCommand = {
    val wr = Wire(new AccelRoccCommand)
    wr.inst.xd := gen.inst.xd
    wr.inst.funct := gen.inst.funct.tail(SystemIDLengthKey)
    wr.inst.system_id := gen.inst.funct.head(SystemIDLengthKey)
    wr.inst.core_id := Cat(gen.inst.rs2, gen.inst.rs1)
    wr.inst.opcode := gen.inst.opcode
    wr.inst.xs1 := gen.inst.xs1
    wr.inst.xs2 := gen.inst.xs2
    wr.payload1 := gen.rs1
    wr.payload2 := gen.rs2
    wr.inst.rd := gen.inst.rd
    wr
  }
}

class DecoupledIOWithCRouting[+T <: Data](gen: T) extends DecoupledIO[T](gen) {
  val expectResponse = Output(Bool())
}

object DecoupledIOWithCRouting {
  def apply[T <: Data](gen: T): DecoupledIOWithCRouting[T] = {
    new DecoupledIOWithCRouting[T](gen)
  }
}

