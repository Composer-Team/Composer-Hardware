package composer.MemoryStreams

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer._
import composer.MemoryStreams.RAM.SyncReadMemMem
import composer.Platforms.{ASICMemoryCompilerKey, PlatformType, PlatformTypeKey}
import composer.Platforms.ASIC.MemoryCompiler
import composer.Platforms.FPGA.Xilinx.{XilinxBRAMSDP, XilinxBRAMTDP}
import freechips.rocketchip.diplomacy.ValName

object CMemory {
  def apply(
      latency: Int,
      dataWidth: Int,
      nRows: Int,
      nReadPorts: Int,
      nWritePorts: Int,
      nReadWritePorts: Int,
      debugName: Option[String] = None
  )(implicit p: Parameters, valName: ValName): CMemoryIOBundle = {
    val mostPortsSupported = p(PlatformTypeKey) match {
      case PlatformType.FPGA => 2
      case PlatformType.ASIC => p(ASICMemoryCompilerKey).mems.keys.max
    }
    val nPorts = nReadPorts + nWritePorts + nReadWritePorts

    if (nPorts > mostPortsSupported && nWritePorts + nReadWritePorts > 1) {
      val regMem = Module(new SyncReadMemMem(nPorts, nRows, dataWidth, latency))
      require(nPorts < 16)
      regMem.mio
    } else if (nPorts > mostPortsSupported) {
      // duplicate the memory
      val nDuplicates = (nPorts.toFloat / (mostPortsSupported - 1)).ceil.toInt
      val mems = Seq.tabulate(nDuplicates) { i =>
        CMemory(
          latency,
          dataWidth,
          nRows,
          mostPortsSupported - 1,
          nWritePorts,
          nReadWritePorts,
          debugName = debugName.map(_ + s"_duplicate_$i")
        )
      }
      val mio = Wire(Output(new CMemoryIOBundle(log2Up(nRows), nReadPorts, nWritePorts, nReadWritePorts, dataWidth)))
      mems.zipWithIndex.foreach { case (mem, mem_idx) =>
        if (nReadWritePorts > 0) {
          val rw = mem.getReadWritePort(0)
          rw <> mio.getReadWritePort(0)
          if (mem_idx > 0) {
            rw.chip_select := mio.getReadWritePort(0).chip_select && mio.getReadWritePort(0).write_enable
          }
        } else if (nWritePorts > 0) {
          val w = mem.getWritePort(0)
          w <> mio.getWritePort(0)
          if (mem_idx > 0) {
            w.chip_select := mio.getWritePort(0).chip_select && mio.getWritePort(0).write_enable
          }
        }
      }
      (0 until nReadPorts) foreach { read_idx =>
        val idx = read_idx / (mostPortsSupported - 1)
        val sub_idx = read_idx % (mostPortsSupported - 1)
        val r = mems(idx).getReadPort(sub_idx)
        r <> mio.getReadPort(read_idx)
      }
      mio
    } else {
      p(PlatformTypeKey) match {
        case PlatformType.FPGA =>
          require(latency >= 1)
          val mio = Wire(Output(new CMemoryIOBundle(log2Up(nRows), nReadPorts, nWritePorts, nReadWritePorts, dataWidth)))
          if (latency >= 3 && nPorts <= 2) {
            val memoryWidth = dataWidth // XilinxBRAMTDP.get_bram_width(dataWidth)
            val banks = 1 // (dataWidth.toFloat / memoryWidth).ceil.toInt
            val rowRoundPow2 = nRows

            val mems = Seq.tabulate(banks) { bank_idx =>
              val high_idx = {
                val q = (bank_idx + 1) * memoryWidth - 1
                if (q >= dataWidth) dataWidth - 1 else q
              }
              val low_idx = bank_idx * memoryWidth

              val cmem = if (nPorts == 1) {
                val cmem = Module(new XilinxBRAMSDP(
                  latency - 2,
                  high_idx - low_idx + 1,
                  rowRoundPow2,
                  debugName = debugName.getOrElse(valName.name)))
                cmem.suggestName(valName.name)
                cmem.io.I := mio.data_in(0)(high_idx, low_idx)
                cmem.io.CE := mio.clock.asBool
                cmem.io.A_read := mio.addr(0)
                cmem.io.A_write := mio.addr(0)
                cmem.io.OEB := mio.read_enable(0)
                cmem.io.CSB_read := mio.chip_select(0)
                cmem.io.CSB_write := mio.chip_select(0)
                cmem.io.WEB := mio.write_enable(0)
                mio.data_out(0) := cmem.io.O
                cmem
              } else {
                val cmem = Module(new XilinxBRAMTDP(
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
                cmem.io.I2 := mio.data_in(1)(high_idx, low_idx)
                cmem.io.A2 := mio.addr(1)
                cmem.io.CSB2 := mio.chip_select(1)
                cmem.io.WEB2 := mio.write_enable(1)
                cmem.io.OEB2 := mio.read_enable(1)
                mio.data_out(1) := cmem.io.O2
                cmem
              }
              (cmem, bank_idx)
            }

            mio.data_out(0) := Cat(mems.map(_._1.data_out(0)).reverse)
            if (nPorts == 2) {
              mio.data_out(1) := Cat(mems.map(_._1.data_out(1)).reverse)
            }
          } else {
            val cmem = Module(new SyncReadMemMem(nPorts, nRows, dataWidth, latency))
            mio <> cmem.mio
            val allocInfo = XilinxBRAMTDP.getMemoryResources(nRows, dataWidth, debugName.getOrElse("anonymous"), nPorts == 1)
            XilinxBRAMTDP.allocateURAM(allocInfo.urams)
            XilinxBRAMTDP.allocateBRAM(allocInfo.brams)
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

class CMemorySingleReadPortIOBundle(addrBits: Int, dataWidth: Int) extends Bundle {
  val addr = Input(UInt(addrBits.W))
  val data_out = Output(UInt(dataWidth.W))
  val chip_select = Input(Bool())
}

class CMemorySingleWritePortIOBundle(addrBits: Int, dataWidth: Int) extends Bundle {
  val addr = Input(UInt(addrBits.W))
  val data_in = Input(UInt(dataWidth.W))
  val chip_select = Input(Bool())
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

class CMemoryIOBundle(val nReadPorts: Int,
                      val nWritePorts: Int,
                      val nReadWritePorts: Int,
                      val addrBits: Int,
                      val dataWidth: Int) extends Bundle {
  val nPorts = nReadPorts + nWritePorts + nReadWritePorts
  val addr = Input(Vec(nPorts, UInt(addrBits.W)))

  val data_in = Input(Vec(nPorts, UInt(dataWidth.W)))
  val data_out = Output(Vec(nPorts, UInt(dataWidth.W)))

  val chip_select = Input(Vec(nPorts, Bool()))
  val read_enable = Input(Vec(nPorts, Bool()))
  val write_enable = Input(Vec(nPorts, Bool()))

  val clock = Input(Bool())

  def getReadPort(idx: Int): CMemorySingleReadPortIOBundle = {
    require(idx < nReadPorts)
    // wrap with Output to enforce default direction
    val io = Wire(Output(new CMemorySingleReadPortIOBundle(addrBits, dataWidth)))
    addr(idx) := io.addr
    io.data_out := data_out(idx)
    data_in(idx) := DontCare
    chip_select(idx) := io.chip_select
    read_enable(idx) := true.B
    write_enable(idx) := false.B
    io
  }

  def getWritePort(idx: Int): CMemorySingleWritePortIOBundle = {
    require(idx < nWritePorts)
    // wrap with Output to enforce default direction
    val io = Wire(Output(new CMemorySingleWritePortIOBundle(addrBits, dataWidth)))
    addr(idx + nReadPorts) := io.addr
    io.data_in := data_in(idx + nReadPorts)
    data_out(idx + nReadPorts) := DontCare
    chip_select(idx + nReadPorts) := io.chip_select
    read_enable(idx + nReadPorts) := false.B
    write_enable(idx + nReadPorts) := true.B
    io
  }

  def getReadWritePort(idx: Int): CMemorySinglePortIOBundle = {
    require(idx < nReadWritePorts)
    // wrap with Output to enforce default direction
    val io = Wire(Output(new CMemorySinglePortIOBundle(addrBits, dataWidth)))
    addr(idx + nReadPorts + nWritePorts) := io.addr
    io.data_in := data_in(idx + nReadPorts + nWritePorts)
    data_out(idx + nReadPorts + nWritePorts) := io.data_out
    chip_select(idx + nReadPorts + nWritePorts) := io.chip_select
    read_enable(idx + nReadPorts + nWritePorts) := true.B
    write_enable(idx + nReadPorts + nWritePorts) := true.B
    io
  }
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
  implicit val io = IO(new CMemoryIOBundle(log2Up(nRows), 0, 0, nPorts, dataWidth))
  MemoryCompiler.buildSRAM(latency, dataWidth, nRows, nPorts)
}