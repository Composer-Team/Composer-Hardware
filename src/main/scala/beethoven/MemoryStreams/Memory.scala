package beethoven.MemoryStreams

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import beethoven.Generation.BuildMode
import beethoven.MemoryStreams.RAM.SyncReadMemMem
import beethoven.Platforms.ASIC.{MemoryCompiler, SupportsWriteEnable}
import beethoven.Platforms.FPGA.Xilinx.Templates.{BRAMSDP, BRAMTDP}
import beethoven.Platforms._
import beethoven.common.ShiftReg
import freechips.rocketchip.diplomacy.ValName

object Memory {

  def get_name(latency: Int,
               dataWidth: Int,
               nRows: Int,
               nReadPorts: Int,
               nWritePorts: Int,
               nReadWritePorts: Int): String =
    s"Memory_L${latency}_DW${dataWidth}_D${nRows}_R${nReadPorts}_W${nWritePorts}_RW${nReadWritePorts}"

  def apply(latency: Int,
            dataWidth: Int,
            nRows: Int,
            nReadPorts: Int,
            nWritePorts: Int,
            nReadWritePorts: Int,
            withWriteEnable: Boolean = false,
            debugName: Option[String] = None,
            allowFallbackToRegister: Boolean = true
           )(implicit p: Parameters, valName: ValName): MemoryIOBundle = {
    val mostPortsSupported = p(PlatformKey) match {
      case mc: Platform with HasMemoryCompiler =>
        if (withWriteEnable)
          assert(mc.memoryCompiler.isInstanceOf[SupportsWriteEnable])
        mc.memoryCompiler.mostPortsSupported
      case _ =>
        assert(!withWriteEnable)
        2
    }
    val nPorts = nReadPorts + nWritePorts + nReadWritePorts

    if (nPorts > mostPortsSupported && (nWritePorts + nReadWritePorts > 1 || mostPortsSupported == 1)) {
      if (withWriteEnable) throw new Exception("Don't support write enable in SyncReadMem")
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
      throw new Exception("Too many ports!!!")
    } else {
      (p(PlatformKey).platformType, p(BuildModeKey)) match {
        case (PlatformType.FPGA, _) | (_, BuildMode.Simulation) =>
          require(latency >= 1)

          def determineWE(mux_degree: Option[Int],
                          lowDegreeAddr: Option[UInt],
                          weSigIn: UInt,
                          fWidth: Int): UInt = {
            (mux_degree, withWriteEnable) match {
              case (None, _) => weSigIn
              case (Some(q), false) =>
                val weVec = Wire(Vec(fWidth / 8, Bool()))
                val bytesPer = fWidth / q / 8
                (0 until q) foreach { m =>
                  (0 until bytesPer) foreach { i =>
                    weVec(m * bytesPer + i) := Mux(lowDegreeAddr.get === m.U,
                      weSigIn,
                      0.U)
                  }
                }
                weVec.asUInt
              case (Some(q), true) =>
                val weVec = Wire(Vec(fWidth / 8, Bool()))
                val bytesPer = fWidth / q / 8
                (0 until q) foreach { m =>
                  (0 until bytesPer) foreach { i =>
                    weVec(m * bytesPer + i) := Mux(lowDegreeAddr.get === m.U,
                      weSigIn(i),
                      0.U)

                  }
                }
                weVec.asUInt
            }
          }

          def splitAddr(mux_degree: Option[Int], addr: UInt): (UInt, Option[UInt]) = {
            mux_degree match {
              case Some(q) =>
                ((addr >> log2Up(q)).asUInt, Some(addr(log2Up(q), 0)))
              case None => (addr, None)
            }
          }

          if (latency >= 2 && nPorts <= 2) {
            val allocInfo = BRAMTDP.getMemoryResources(nRows,
              dataWidth,
              debugName.getOrElse("anonymous"),
              nPorts == 1,
              canMux = if (p(BuildModeKey) == BuildMode.Simulation) withWriteEnable else true)

            val (fRows, fWidth) = allocInfo.mux_degree match {
              case Some(mux_deg) => ((nRows.toFloat / mux_deg).ceil.toInt, dataWidth * mux_deg)
              case None => (nRows, dataWidth)
            }

            val mio = Wire(Output(new MemoryIOBundle(nReadPorts, nWritePorts, nReadWritePorts, log2Up(nRows), dataWidth, withWriteEnable)))
            val isSimple = nPorts == 2 && nReadPorts == 1 && nWritePorts == 1

            def hook_up(addr_o: UInt, dout_o: UInt, din_o: UInt, web_o: UInt,
                        dout_i: UInt, addr_in: UInt, din_i: UInt, web_i: UInt): Unit = {
                val (addr_high, addr_low) = splitAddr(allocInfo.mux_degree, addr_in)
                addr_o := addr_high
                dout_i := (addr_low match {
                  case None => dout_o
                  case Some(q) =>
                    val muxSel = ShiftReg(q, latency)
                    val muxD = VecInit((0 until allocInfo.mux_degree.get) map { i =>
                      dout_o((i + 1) * dataWidth - 1, i * dataWidth)
                    })
                    muxD(muxSel)
                })
                din_o := (allocInfo.mux_degree match {
                  case None => din_i
                  case Some(q) =>
                    val i_mult = Wire(Vec(q, din_i.cloneType))
                    i_mult foreach (_ := din_i)
                    i_mult.asUInt
                })
                web_o := determineWE(allocInfo.mux_degree, addr_low, web_i, fWidth)
            }
            val mem = if (nPorts == 1) {
              val cmem = Module(new BRAMSDP(
                latency - 1,
                fWidth,
                fRows,
                withWriteEnable || allocInfo.mux_degree.isDefined,
                debugName = debugName.getOrElse(valName.name)))
              cmem.suggestName(valName.name)

              val (addrHigh, addrLow) = splitAddr(allocInfo.mux_degree, mio.addr(0))

              cmem.io.I := (allocInfo.mux_degree match {
                case None => mio.data_in(0)
                case Some(q) =>
                  val i_mult = Wire(Vec(q, mio.data_in(0).cloneType))
                  i_mult foreach (_ := mio.data_in(0))
                  i_mult.asUInt
              })
              cmem.io.CE := mio.clock.asBool
              cmem.io.A_read := addrHigh
              cmem.io.A_write := addrHigh
              cmem.io.OEB := mio.read_enable(0)
              cmem.io.CSB_read := mio.chip_select(0) && mio.read_enable(0)
              cmem.io.CSB_write := mio.chip_select(0) && mio.write_enable(0).asBool
              cmem.io.WEB := determineWE(allocInfo.mux_degree, addrLow, mio.write_enable(0), fWidth)
              mio.data_out(0) := (addrLow match {
                case None => cmem.io.O
                case Some(q) =>
                  val muxSel = ShiftReg(q, latency)
                  val muxD = VecInit((0 until allocInfo.mux_degree.get) map { i =>
                    cmem.io.O((i + 1) * dataWidth - 1, i * dataWidth)
                  })
                  muxD(muxSel)
              })
              cmem
            } else if (isSimple) {
              val cmem = Module(new BRAMSDP(
                latency - 1,
                fWidth,
                fRows,
                withWriteEnable || allocInfo.mux_degree.isDefined,
                debugName = debugName.getOrElse(valName.name)))
              cmem.suggestName(valName.name)
              val ridx = mio.getReadPortIdx(0)
              val widx = mio.getWritePortIdx(0)
              cmem.io.I := (allocInfo.mux_degree match {
                case None => mio.data_in(widx)
                case Some(q) =>
                  val i_mult = Wire(Vec(q, mio.data_in(0).cloneType))
                  i_mult foreach (_ := mio.data_in(0))
                  i_mult.asUInt
              })
              val (r_addr_high, r_addr_low) = splitAddr(allocInfo.mux_degree, mio.addr(ridx))
              val (w_addr_high, w_addr_low) = splitAddr(allocInfo.mux_degree, mio.addr(widx))
              cmem.io.CE := mio.clock.asBool

              cmem.io.A_read := r_addr_high
              cmem.io.CSB_read := mio.chip_select(ridx)

              cmem.io.A_write := w_addr_high
              cmem.io.CSB_write := mio.chip_select(widx)
              cmem.io.WEB := determineWE(allocInfo.mux_degree, w_addr_low, mio.write_enable(widx), fWidth)
              cmem.io.OEB := mio.read_enable(ridx)

              mio.data_out(ridx) := (r_addr_low match {
                case None => cmem.io.O
                case Some(q) =>
                  val muxSel = ShiftReg(q, latency)
                  val muxD = VecInit((0 until allocInfo.mux_degree.get) map { i =>
                    cmem.io.O((i + 1) * dataWidth - 1, i * dataWidth)
                  })
                  muxD(muxSel)
              })
              mio.data_out(widx) := DontCare

              cmem
            } else {
              val cmem = Module(new BRAMTDP(
                latency - 1,
                fWidth,
                fRows,
                withWriteEnable || allocInfo.mux_degree.isDefined,
                debugName = debugName.getOrElse(valName.name)))
              cmem.suggestName(valName.name)
              Seq((cmem.io.A1, cmem.io.O1, cmem.io.I1, cmem.io.WEB1, mio.data_out(0), mio.addr(0), mio.data_in(0), mio.write_enable(0)),
                (cmem.io.A2, cmem.io.O2, cmem.io.I2, cmem.io.WEB2, mio.data_out(1), mio.addr(1), mio.data_in(1), mio.write_enable(1))) foreach {
                q =>
                hook_up(q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
              }

              cmem.io.CE := mio.clock.asBool
              cmem.io.CSB1 := mio.chip_select(0)
              cmem.io.OEB1 := mio.read_enable(0)
              cmem.io.CSB2 := mio.chip_select(1)
              cmem.io.OEB2 := mio.read_enable(1)
              cmem.io
            }
            mio
          } else {
            // latency == 1 or 2. Recognizing URAM/BRAM is in god's hands
            val mio = Wire(Output(new MemoryIOBundle(nReadPorts, nWritePorts, nReadWritePorts, log2Up(nRows), dataWidth, withWriteEnable)))
            val cmem = Module(new SyncReadMemMem(nReadPorts, nWritePorts, nReadWritePorts, nRows, dataWidth, latency))
            mio <> cmem.mio
            val allocInfo = BRAMTDP.getMemoryResources(nRows, dataWidth, debugName.getOrElse("anonymous"), nPorts == 1, canMux = false)
            BRAMTDP.allocateURAM(allocInfo.urams)
            BRAMTDP.allocateBRAM(allocInfo.brams)
            mio
          }
        case (PlatformType.ASIC, BuildMode.Synthesis) =>
          //          println("ASIC Mem")
          val cmem = Module(new CASICMemory(latency, dataWidth, nRows, nPorts, withWriteEnable, allowFallbackToRegister, p(PlatformKey).clockRateMHz))
          cmem.suggestName(valName.name)
          cmem.io
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
  def fromTransposeInput(in: MemoryIOBundle): Vec[CMemorySinglePortIOBundle] = {
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
  def fromTransposeOutput(in: MemoryIOBundle): Vec[CMemorySinglePortIOBundle] = {
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

class MemoryIOBundle(val nReadPorts: Int,
                     val nWritePorts: Int,
                     val nReadWritePorts: Int,
                     val addrBits: Int,
                     val dataWidth: Int,
                     val perByteWE: Boolean) extends Bundle {
  def nPorts: Int = nReadPorts + nWritePorts + nReadWritePorts

  val addr = Input(Vec(nPorts, UInt(addrBits.W)))

  val data_in = Input(Vec(nPorts, UInt(dataWidth.W)))
  val data_out = Output(Vec(nPorts, UInt(dataWidth.W)))

  val chip_select = Input(Vec(nPorts, Bool()))
  val read_enable = Input(Vec(nPorts, Bool()))
  val write_enable = Input(Vec(nPorts,
    if (perByteWE) UInt((dataWidth / 8).W)
    else UInt(1.W)))

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
  val io: MemoryIOBundle
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

  def write_enable: Seq[UInt]

  def clocks: Seq[Bool]
}

case class SD(dWidth: Int, nRows: Int)

case class SRAMArray(array: List[List[List[(Int, Int)]]], characteristics: Map[String, Any] = Map.empty)

/**
 * ALL LOGIC IMPLEMENTED HERE MUST BE ACTIVE LOW
 */
class CASICMemory(latency: Int, dataWidth: Int, nRowsSuggested: Int, nPorts: Int, withWE: Boolean, allowFallbackToRegisters: Boolean, freqMHz: Int)(implicit p: Parameters)
  extends RawModule
    with HasCMemoryIO {
  private val nRows = Math.max(nRowsSuggested, 4 * latency)
  override val desiredName = Memory.get_name(latency, dataWidth, nRows, 0, 0, nPorts)
  implicit val io = IO(new MemoryIOBundle(0, 0, nPorts, log2Up(nRows), dataWidth, withWE))
  MemoryCompiler.buildSRAM(latency, dataWidth, nRows, nPorts, withWE, allowFallbackToRegisters, freqMHz)
}