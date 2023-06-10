package composer.MemoryStreams

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import composer._
import composer.MemoryStreams.RAM.{CFPGAMemory, MemoryCompiler}
import freechips.rocketchip.diplomacy.ValName

import java.io.FileWriter
import scala.annotation.tailrec
import scala.collection.SeqMap

object CMemory {
  def apply(
      latency: Int,
      dataWidth: Int,
      nRows: Int,
      nPorts: Int,
      debugName: Option[String] = None
  )(implicit p: Parameters, valName: ValName): CMemoryIOBundle = {
    p(PlatformTypeKey) match {
      case PlatformType.FPGA =>
        require (nPorts <= 2, s"Trying to elaborate a $nPorts port memory. FPGA BRAM/URAM only" +
          s"supports up to 2 ports.")
        val cmem = Module(
          new CFPGAMemory(
            latency - 2,
            dataWidth,
            nRows,
            debugName = debugName.getOrElse(valName.name)
          )
        )
        cmem.suggestName(valName.name)
        val mio = Wire(Output(new CMemoryIOBundle(nPorts, addrBits = log2Up(nRows), dataWidth)))
        cmem.io.I1 := mio.data_in(0)
        cmem.io.CE1 := mio.clock.asBool
        cmem.io.A1 := mio.addr(0)
        cmem.io.CSB1 := mio.chip_select(0)
        cmem.io.WEB1 := mio.write_enable(0)
        cmem.io.OEB1 := mio.read_enable(0)
        mio.data_out(0) := cmem.io.O1
        cmem.io.CE2 := DontCare
        if (nPorts == 1) {
          cmem.io.I2 := DontCare
          cmem.io.A2 := DontCare
          cmem.io.CSB2 := false.B
          cmem.io.OEB2 := DontCare
          cmem.io.WEB2 := DontCare
        } else {
          cmem.io.I2 := mio.data_in(1)
          cmem.io.CE2 := mio.clock.asBool
          cmem.io.A2 := mio.addr(1)
          cmem.io.CSB2 := mio.chip_select(1)
          cmem.io.WEB2 := mio.write_enable(1)
          cmem.io.OEB2 := mio.read_enable(1)
          mio.data_out(1) := cmem.io.O2
        }
        mio
      case PlatformType.ASIC =>
        val cmem = Module(new CASICMemory(latency, dataWidth, nRows, nPorts))
        cmem.suggestName(valName.name)
        TransitName(cmem.io, cmem)
    }
  }
}

class CMemoryIOBundle(val nPorts: Int, val addrBits: Int, dataWidth: Int) extends Bundle {
  val addr = Input(Vec(nPorts, UInt(addrBits.W)))

  val data_in = Input(Vec(nPorts, UInt(dataWidth.W)))
  val data_out = Output(Vec(nPorts, UInt(dataWidth.W)))

  val chip_select = Input(Vec(nPorts, Bool()))
  val read_enable = Input(Vec(nPorts, Bool()))
  val write_enable = Input(Vec(nPorts, Bool()))

  val clock = Input(Clock())
}

trait HasCMemoryIO {
  val io: CMemoryIOBundle
}

trait withMemoryIOForwarding {
  val addrBits: Int
  val nPorts: Int
  val addr_FW = Output(Vec(nPorts, UInt(addrBits.W)))
  val chip_select_FW = Output(Vec(nPorts, Bool()))
  val read_enable_FW = Output(Vec(nPorts, Bool()))
  val write_enable_FW = Output(Vec(nPorts, Bool()))
}

trait HasMemoryInterface {
  def data_in: Seq[UInt]
  def data_out: Seq[UInt]

  def addr: Seq[UInt]

  def chip_select: Seq[Bool]
  def read_enable: Seq[Bool]
  def write_enable: Seq[Bool]
  def clocks: Seq[Bool]
}

// SRAM DIM
case class SD(dWidth: Int, nRows: Int)

case class CascadeDescriptor(depth: Int, bankWidths: Seq[Int])
/**
 * ALL LOGIC IMPLEMENTED HERE MUST BE ACTIVE LOW
 */
class CASICMemory(latency: Int, dataWidth: Int, nRowsSuggested: Int, nPorts: Int)(implicit p: Parameters)
  extends RawModule
    with HasCMemoryIO {
  private val nRows = Math.max(nRowsSuggested, 4 * latency)
  override val desiredName = f"CMemoryASIC_l${latency}dw${dataWidth}r$nRows"
  implicit val io = IO(new CMemoryIOBundle(nPorts, log2Up(nRows), dataWidth))
  MemoryCompiler.buildSRAM(latency, dataWidth, nRows, nPorts)
}