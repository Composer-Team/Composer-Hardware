package composer.MemoryStreams

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer._
import composer.MemoryStreams.RAM.SyncReadMemMem
import composer.Platforms.{ASICMemoryCompilerKey, PlatformType, PlatformTypeKey}
import composer.Platforms.ASIC.MemoryCompiler
import composer.Platforms.FPGA.Xilinx.FPGAMemoryCompiler
import freechips.rocketchip.diplomacy.ValName

object CMemory {
  def apply(
      latency: Int,
      dataWidth: Int,
      nRows: Int,
      nPorts: Int,
      debugName: Option[String] = None
  )(implicit p: Parameters, valName: ValName): CMemoryIOBundle = {
    val mostPortsSupported = p(PlatformTypeKey) match {
      case PlatformType.FPGA => 2
      case PlatformType.ASIC => p(ASICMemoryCompilerKey).mems.keys.max
    }
    if (nPorts > mostPortsSupported) {
      val regMem = Module(new SyncReadMemMem(nPorts, nRows, dataWidth, latency))
      regMem.mio
    } else {
      p(PlatformTypeKey) match {
        case PlatformType.FPGA =>
          require(latency >= 1)
          val mio = Wire(Output(new CMemoryIOBundle(nPorts, addrBits = log2Up(nRows), dataWidth)))
          if (latency >= 3 && nPorts <= 2) {
            val memoryWidth = FPGAMemoryCompiler.get_bram_width(dataWidth)
            val banks = (dataWidth.toFloat / memoryWidth).ceil.toInt
            val rowRoundPow2 = nRows

            val mems = Seq.tabulate(banks) { bank_idx =>
              val high_idx = {
                val q = (bank_idx + 1) * memoryWidth - 1
                if (q >= dataWidth) dataWidth - 1 else q
              }
              val low_idx = bank_idx * memoryWidth
              val cmem = Module(new FPGAMemoryCompiler(
                latency - 2,
                high_idx - low_idx + 1,
                rowRoundPow2,
                debugName = debugName.getOrElse(valName.name)))
              cmem.suggestName(valName.name)
              cmem.io.I1 := mio.data_in(0)(high_idx, low_idx)
              cmem.io.CE := mio.clock.asBool
              cmem.io.A1 := mio.addr(0)
              cmem.io.CSB1 := mio.chip_select(0)
              cmem.io.WEB1 := mio.write_enable(0)
              cmem.io.OEB1 := mio.read_enable(0)
              mio.data_out(0) := cmem.io.O1
              if (nPorts == 1) {
                cmem.io.I2 := DontCare
                cmem.io.A2 := DontCare
                cmem.io.CSB2 := false.B
                cmem.io.OEB2 := DontCare
                cmem.io.WEB2 := DontCare
              } else {
                cmem.io.I2 := mio.data_in(1)
                cmem.io.A2 := mio.addr(1)
                cmem.io.CSB2 := mio.chip_select(1)
                cmem.io.WEB2 := mio.write_enable(1)
                cmem.io.OEB2 := mio.read_enable(1)
                mio.data_out(1) := cmem.io.O2
              }
              (cmem, bank_idx)
            }
            mio.data_out(0) := Cat(mems.map(_._1.io.O1).reverse)
            if (nPorts == 2) {
              mio.data_out(1) := Cat(mems.map(_._1.io.O2).reverse)
            }
          } else {
            val cmem = Module(new SyncReadMemMem(nPorts, nRows, dataWidth, latency))
            mio <> cmem.mio
            val allocInfo = FPGAMemoryCompiler.getMemoryResources(nRows, dataWidth, debugName.getOrElse("anonymous"))
            FPGAMemoryCompiler.allocateURAM(allocInfo.urams)
            FPGAMemoryCompiler.allocateBRAM(allocInfo.brams)
            // latency == 1 or 2. Recognizing URAM/BRAM is now in god's hands
          }
          mio
        case PlatformType.ASIC =>
          val cmem = Module(new CASICMemory(latency, dataWidth, nRows, nPorts))
          cmem.suggestName(valName.name)
          TransitName(cmem.io, cmem)
      }
    }
  }
}

class CMemorySinglePortIOBundle(val addrBits: Int, dataWidth: Int) extends Bundle {
  val addr = Input(UInt(addrBits.W))

  val data_in = Input(UInt(dataWidth.W))
  val data_out = Output(UInt(dataWidth.W))

  val chip_select = Input(Bool())
  val read_enable = Input(Bool())
  val write_enable = Input(Bool())
}

object CMemorySinglePortIOBundle {
  // constructor for series of single port IOs from a CMemoryIOBundle
  def fromTransposeInput(in: CMemoryIOBundle): Vec[CMemorySinglePortIOBundle] = {
    val nPorts = in.nPorts
    val addrBits = in.addrBits
    val dataWidth = in.dataWidth
    val io = Wire(Vec(nPorts, Output(new CMemorySinglePortIOBundle(addrBits, dataWidth))))
    for (i <- 0 until nPorts) {
      io(i).addr := in.addr(i)
      io(i).data_in := in.data_in(i)
      in.data_out(i) := io(i).data_out
      io(i).chip_select := in.chip_select(i)
      io(i).read_enable := in.read_enable(i)
      io(i).write_enable := in.write_enable(i)
    }
    io
  }

  // constructor for series of single port IOs from a flipped CMemoryIOBundle
  def fromTransposeOutput(in: CMemoryIOBundle): Vec[CMemorySinglePortIOBundle] = {
    val io = Wire(Vec(in.nPorts, Output(new CMemorySinglePortIOBundle(in.addrBits, in.dataWidth))))
    for (i <- 0 until in.nPorts) {
      in.addr := io(i).addr
      in.data_in := io(i).data_in
      io(i).data_out := in.data_out
      in.chip_select := io(i).chip_select
      in.read_enable := io(i).read_enable
      in.write_enable := io(i).write_enable
    }
    io
  }

}

class CMemoryIOBundle(val nPorts: Int, val addrBits: Int, val dataWidth: Int) extends Bundle {
  val addr = Input(Vec(nPorts, UInt(addrBits.W)))

  val data_in = Input(Vec(nPorts, UInt(dataWidth.W)))
  val data_out = Output(Vec(nPorts, UInt(dataWidth.W)))

  val chip_select = Input(Vec(nPorts, Bool()))
  val read_enable = Input(Vec(nPorts, Bool()))
  val write_enable = Input(Vec(nPorts, Bool()))

  val clock = Input(Bool())
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