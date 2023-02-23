package composer.AXIHelpers

import chisel3._
import chisel3.util._
import composer.RoccHelpers.{DecoupledSinkEntry, DecoupledSourceEntry, MCRFile, MCRFileMap, Permissions, ReadOnly, ReadWrite, RegisterEntry}
import composer._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import java.io.FileWriter

/*
 * This file seems to handle memory-mapped IO
 */
abstract class Widget(implicit p: Parameters) extends LazyModule {
  val node = AXI4IdentityNode()
  val crFile: MCRFile
  val module: WidgetModule
}

abstract class WidgetModule(outer: Widget) extends LazyModuleImp(outer) {

  // TODO: use this to get the nastidatabits
  //  val (ctrl, edge) =  outer.node.in

  val nastiXDataBits = p(AXILSlaveBeatBytes) * 8

  var _finalized = false
  val crRegistry = new MCRFileMap()

  def numRegs = crRegistry.numRegs


  protected var wName: Option[String] = None

  def setWidgetName(n: String): Unit = {
    wName = Some(n)
  }

  def getWName: String = {
    wName.getOrElse(throw new RuntimeException("Must build widgets with their companion object"))
  }

  def attach(reg: Data, name: String, permissions: Permissions = ReadWrite): Int = {
    crRegistry.allocate(RegisterEntry(reg, name, permissions))
  }

  // Recursively binds the IO of a module:
  //   For inputs, generates a registers and binds that to the map
  //   For outputs, direct binds the wire to the map
  def attachIO(io: Record, prefix: String = ""): Unit = {
    // this might not work....
    import Chisel._
    def innerAttachIO(node: Data, name: String): Unit = node match {
      case b: Bits =>
        if (b.dir == OUTPUT) {
          attach(b, s"$name", ReadOnly)
        } else {
          genWOReg(b, name)
        }
      case v: Vec[_] =>
        v.zipWithIndex.foreach({ case (elm, idx) => innerAttachIO(elm, s"${name}_$idx") })
      case r: Record =>
        r.elements.foreach({ case (subName, elm) => innerAttachIO(elm, s"${name}_$subName") })
      case _ => new RuntimeException("Cannot bind to this sort of node...")
    }

    io.elements.foreach({ case (name, elm) => innerAttachIO(elm, s"$prefix$name") })
  }

  def attachDecoupledSink(channel: DecoupledIO[UInt], name: String): Int = {
    crRegistry.allocate(DecoupledSinkEntry(channel, name))
  }

  def attachDecoupledSource(channel: DecoupledIO[UInt], name: String): Int = {
    crRegistry.allocate(DecoupledSourceEntry(channel, name))
  }

  def genAndAttachQueue(channel: DecoupledIO[UInt], name: String, depth: Int = 2): DecoupledIO[UInt] = {
    val enq = Wire(channel.cloneType)
    channel <> Queue(enq, entries = depth)
    attachDecoupledSink(enq, name)
    channel
  }

  def genAndAttachReg[T <: Data](
                                  wire: T,
                                  name: String,
                                  default: Option[T] = None,
                                  masterDriven: Boolean = true): T = {
    require(wire.getWidth <= nastiXDataBits)
    val reg = default match {
      case None => Reg(wire.cloneType)
      case Some(init) => RegInit(init)
    }
    if (masterDriven) wire := reg else reg := wire
    attach(reg, name)
    reg suggestName name
    reg
  }

  def genWOReg[T <: Data](wire: T, name: String): T = genAndAttachReg(wire, name)

  def genROReg[T <: Data](wire: T, name: String): T = genAndAttachReg(wire, name, masterDriven = false)

  def genWORegInit[T <: Data](wire: T, name: String, default: T): T =
    genAndAttachReg(wire, name, Some(default))

  def genRORegInit[T <: Data](wire: T, name: String, default: T): T =
    genAndAttachReg(wire, name, Some(default), masterDriven=false)

  def genCRFile(): Unit = {
    crRegistry.bindRegs(outer.crFile.module.io.mcr)
  }

  // Returns a word addresses
  def getCRAddr(name: String): Int = {
    require(_finalized, "Must build Widgets with their companion object")
    crRegistry.lookupAddress(name).getOrElse(
      throw new RuntimeException(s"Could not find CR:$name in widget: $wName"))
  }

  def printCRs(ostream: Option[FileWriter] = None): Unit = crRegistry.printCRs(ostream)
}

// instead of relying on the widget writer
object Widget {
  def apply[T <: Widget](m: => T, wName: String): T = {
    val w = LazyModule(m)
    w.module suggestName wName
    w.module setWidgetName wName
    w.module._finalized = true
    w
  }
}
object Pulsify {
  def apply(in: Bool, pulseLength: Int): Unit = {
    require(pulseLength > 0)
    if (pulseLength > 1) {
      val count = Counter(pulseLength)
      when(in) {
        count.inc()
      }
      when(count.value === (pulseLength - 1).U) {
        in := false.B
        count.value := 0.U
      }
    } else {
      when(in) {
        in := false.B
      }
    }
  }
}
