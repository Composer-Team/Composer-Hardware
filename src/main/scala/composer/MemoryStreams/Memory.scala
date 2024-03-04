package composer.MemoryStreams

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.MemoryStreams.RAM.SyncReadMemMem
import composer.Platforms.ASIC.MemoryCompiler
import composer.Platforms.FPGA.Xilinx.Templates.{BRAMSDP, BRAMTDP}
import composer.Platforms._
import freechips.rocketchip.diplomacy.ValName

object Memory {

  def get_name(latency: Int,
               dataWidth: Int,
               nRows: Int,
               nReadPorts: Int,
               nWritePorts: Int,
               nReadWritePorts: Int): String =
    s"Memory_L${latency}_DW${dataWidth}_D${nRows}_R${nReadPorts}_W${nWritePorts}_RW${nReadWritePorts}"

  def apply(
      latency: Int,
      dataWidth: Int,
      nRows: Int,
      nReadPorts: Int,
      nWritePorts: Int,
      nReadWritePorts: Int,
      debugName: Option[String] = None
  )(implicit p: Parameters, valName: ValName): CMemoryIOBundle = {
//    println(s"Creating CMemory with $nReadPorts read ports, $nWritePorts write ports, $nReadWritePorts read/write ports: $debugName, $valName")
    val mostPortsSupported = p(PlatformKey) match {
      case mc: Platform with HasMemoryCompiler => mc.memoryCompiler.mostPortsSupported
      case _ => 2
    }
    val nPorts = nReadPorts + nWritePorts + nReadWritePorts

    if (nPorts > mostPortsSupported && (nWritePorts + nReadWritePorts > 1 || mostPortsSupported == 1)) {
      println("SRMM")
      val regMem = Module(new SyncReadMemMem(nReadPorts, nWritePorts, nReadWritePorts, nRows, dataWidth, latency))
      require(nPorts < 16)
      (0 until nReadPorts) foreach { idx =>
        val ridx = regMem.mio.getReadPortIdx(idx)
        regMem.mio.data_in(ridx) := DontCare
        regMem.mio.chip_select(ridx) := DontCare
      }
      regMem.mio
    }
    else if (nPorts > mostPortsSupported) {
      println("Duplicated Mem")
      // duplicate the memory
      val nDuplicates = ((nPorts - 1).toFloat / (mostPortsSupported - 1)).ceil.toInt
//      println("Duplicate memory " + nDuplicates + " times for " + nPorts + " ports")
      val mems = Seq.tabulate(nDuplicates) { i =>
        Memory(
          latency,
          dataWidth,
          nRows,
          mostPortsSupported - 1,
          0,
          1,
          debugName = debugName.map(_ + s"_duplicate_$i")
        )
      }
      val mio = Wire(Output(new CMemoryIOBundle(nReadPorts, nWritePorts, nReadWritePorts, log2Up(nRows), dataWidth)))
      mems.foreach(_.clock := mio.clock)
      val writer_idx = if (nReadWritePorts > 0) mio.getReadWritePortIdx(0) else mio.getWritePortIdx(0)
      // connect writer port to all memories
      mems.zipWithIndex.foreach { case (mem, i) =>
        val memRW = mem.getReadWritePortIdx(0)
//        println("Connecting writer port " + writer_idx + " to memory " + i + " port " + memRW)
        mem.data_in(memRW) := mio.data_in(writer_idx)
        mem.addr(memRW) := mio.addr(writer_idx)
        mem.write_enable(memRW) := mio.write_enable(writer_idx)
        mem.chip_select(memRW) := mio.chip_select(writer_idx)
        mem.read_enable(memRW) := mio.read_enable(writer_idx)
        mio.data_out(writer_idx) := mem.data_out(memRW)
      }
      // connect reader ports to all memories
      (0 until nReadPorts).foreach { reader_idx =>
        val memIdx = reader_idx  / (mostPortsSupported - 1)
        val memSubIdx = reader_idx % (mostPortsSupported - 1)
        val memRW = mems(memIdx).getReadPortIdx(memSubIdx)
//        println("Connecting reader port " + reader_idx + " to memory " + memIdx + " port " + memRW )
        mems(memIdx).data_in(memRW) := DontCare // mio.data_in(reader_idx)
        mems(memIdx).addr(memRW) := mio.addr(reader_idx)
        mems(memIdx).write_enable(memRW) := false.B // mio.write_enable(reader_idx)
        mems(memIdx).chip_select(memRW) := mio.chip_select(reader_idx)
        mems(memIdx).read_enable(memRW) := true.B // mio.read_enable(reader_idx)
        mio.data_out(reader_idx) := mems(memIdx).data_out(memRW)
      }
      mio
    }
    else {
      p(PlatformKey).platformType match {
        case PlatformType.FPGA =>
          require(latency >= 1)
          val mio = Wire(Output(new CMemoryIOBundle(nReadPorts, nWritePorts, nReadWritePorts, log2Up(nRows), dataWidth)))
          if (latency >= 3 && nPorts <= 2) {
            val memoryWidth = dataWidth // XilinxBRAMTDP.get_bram_width(dataWidth)
            val banks = 1 // (dataWidth.toFloat / memoryWidth).ceil.toInt
            val rowRoundPow2 = nRows
            val isSimple = nPorts == 2 && nReadPorts == 1 && nWritePorts == 1

            val mems = Seq.tabulate(banks) { bank_idx =>
              val high_idx = {
                val q = (bank_idx + 1) * memoryWidth - 1
                if (q >= dataWidth) dataWidth - 1 else q
              }
              val low_idx = bank_idx * memoryWidth

              val mem = if (nPorts == 1) {
                val cmem = Module(new BRAMSDP(
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
                cmem.io.CSB_read := mio.chip_select(0) && mio.read_enable(0)
                cmem.io.CSB_write := mio.chip_select(0) && mio.write_enable(0)
                cmem.io.WEB := mio.write_enable(0)
                mio.data_out(0) := cmem.io.O
                cmem
              } else if (isSimple) {
                val cmem = Module(new BRAMSDP(
                  latency - 2,
                  high_idx - low_idx + 1,
                  rowRoundPow2,
                  debugName = debugName.getOrElse(valName.name)))
                cmem.suggestName(valName.name)
                val ridx = mio.getReadPortIdx(0)
                val widx = mio.getWritePortIdx(0)
                cmem.io.I := mio.data_in(widx)(high_idx, low_idx)
                cmem.io.CE := mio.clock.asBool
                cmem.io.A_write := mio.addr(widx)
                cmem.io.CSB_write := mio.chip_select(widx)
                cmem.io.WEB := mio.write_enable(widx)
                cmem.io.OEB := mio.read_enable(ridx)
                mio.data_out(ridx) := cmem.io.O
                mio.data_out(widx) := DontCare
                cmem.io.A_read := mio.addr(ridx)
                cmem.io.CSB_read := mio.chip_select(ridx)
                cmem
              } else {
                val cmem = Module(new BRAMTDP(
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
              (mem, bank_idx)
            }
//            mio.data_out(0) := Cat(mems.map(_._1.data_out(0)).reverse)
//            if (isSimple) {
//            } else if (nPorts == 2) {
//              mio.data_out(1) := Cat(mems.map(_._1.data_out(1)).reverse)
//            }
          } else {
            val cmem = Module(new SyncReadMemMem(nReadPorts, nWritePorts, nReadWritePorts, nRows, dataWidth, latency))
            mio <> cmem.mio
            val allocInfo = BRAMTDP.getMemoryResources(nRows, dataWidth, debugName.getOrElse("anonymous"), nPorts == 1)
            BRAMTDP.allocateURAM(allocInfo.urams)
            BRAMTDP.allocateBRAM(allocInfo.brams)
            // latency == 1 or 2. Recognizing URAM/BRAM is now in god's hands
          }
          mio
        case PlatformType.ASIC =>
          println("ASIC Mem")
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

class CMemoryIOBundle(val nReadPorts: Int,
                      val nWritePorts: Int,
                      val nReadWritePorts: Int,
                      val addrBits: Int,
                      val dataWidth: Int) extends Bundle {
  def nPorts: Int = nReadPorts + nWritePorts + nReadWritePorts
  val addr = Input(Vec(nPorts, UInt(addrBits.W)))

  val data_in = Input(Vec(nPorts, UInt(dataWidth.W)))
  val data_out = Output(Vec(nPorts, UInt(dataWidth.W)))

  val chip_select = Input(Vec(nPorts, Bool()))
  val read_enable = Input(Vec(nPorts, Bool()))
  val write_enable = Input(Vec(nPorts, Bool()))

  val clock = Input(Bool())

  def getReadPortIdx(idx: Int): Int = {
    idx
  }

  def getWritePortIdx(idx: Int): Int = {
    require(idx < nWritePorts)
    idx + nReadPorts
  }

  def getReadWritePortIdx(idx: Int): Int = {
    require(idx < nReadWritePorts)
    idx + nReadPorts + nWritePorts
  }

  def initLow(clock: Clock): Unit = {
    Seq(chip_select, write_enable, read_enable, addr, data_in) foreach (_.foreach(_ := 0.U))
    this.clock := clock.asBool
  }
}


trait HasCMemoryIO {
  val io: CMemoryIOBundle
}

trait withMemoryIOForwarding {
  val addrBits: Int
  def nPorts: Int
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

case class SD(dWidth: Int, nRows: Int)

case class CascadeDescriptor(depths: Seq[Int], bankWidths: Seq[Int])
/**
 * ALL LOGIC IMPLEMENTED HERE MUST BE ACTIVE LOW
 */
class CASICMemory(latency: Int, dataWidth: Int, nRowsSuggested: Int, nPorts: Int)(implicit p: Parameters)
  extends RawModule
    with HasCMemoryIO {
  private val nRows = Math.max(nRowsSuggested, 4 * latency)
  override val desiredName = Memory.get_name(latency, dataWidth, nRows, 0, 0, nPorts)
  implicit val io = IO(new CMemoryIOBundle(0, 0, nPorts, log2Up(nRows), dataWidth))
  MemoryCompiler.buildSRAM(latency, dataWidth, nRows, nPorts)
}