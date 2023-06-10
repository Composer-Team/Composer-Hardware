package composer.MemoryStreams

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import composer._
import composer.MemoryStreams.RAM.CFPGAMemory
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
        val cmem = Module(
          new CASICMemoryWithActiveHighSelects(latency, dataWidth, nRows)
        )
        cmem.suggestName(valName.name)
        cmem.io
    }
  }
}

class CMemoryIOBundle(val nPorts: Int, addrBits: Int, dataWidth: Int) extends Bundle {
  val addr = Vec(nPorts, UInt(addrBits.W))

  val data_in = Vec(nPorts, UInt(dataWidth.W))
  val data_out = Vec(nPorts, UInt(dataWidth.W))

  val chip_select = Vec(nPorts, Bool())
  val read_enable = Vec(nPorts, Bool())
  val write_enable = Vec(nPorts, Bool())

  val clock = Input(Clock())
}

trait HasCMemoryIO {
  val io: CMemoryIOBundle
}





trait withSupportForwarding {
  val elements: SeqMap[String, Data]

  def connectTo(other: Bundle): Unit = {
    elements.filter(_._1.contains("_FW")).foreach { case (name, dat) =>
      val o = other.elements(name.substring(0, name.length - 3))
      o <> dat
    }
    other.elements.filter(_._1.substring(0, 1) == "I").foreach {
      case (name, dat) =>
        dat <> elements("O" + name.substring(1))
    }
  }
}

trait withSPForwarding extends withSupportForwarding {
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

class MemoryAccessBundle(val addrBits: Int, dataBits: Int, val nPorts: Int) extends Bundle {
  val addr = Input(Vec(nPorts, UInt(addrBits.W)))
  val write_enable = Input(Vec(nPorts, Bool()))
  val read_enable = Input(Vec(nPorts, Bool()))
  val chip_select = Input(Vec(nPorts, Bool()))
  val data_in = Input(Vec(nPorts, UInt(dataBits.W)))
  val data_out = Output(Vec(nPorts, UInt(dataBits.W)))
}

private class CASICSPMemoryCascade(
                                    rows: Int,
                                    dataBits: Int,
                                    cascadeSize: Int,
                                    idx: Int
                                  )(implicit p: Parameters) extends Module {
  val mem = Module(new CMemorySPSRAM(rows, dataBits))
  require(idx < cascadeSize, "sanity")
  val cascadeBits = log2Up(cascadeSize)
  val totalAddr = log2Up(rows) + cascadeBits
  val io = IO(new SPSRAMBundle(totalAddr, dataBits) with withMemoryIOForwarding)
  val cascade_select1 = idx.U === io.A.head(cascadeBits)
  mem.io.CE := clock.asBool
  io.O := mem.io.O
  mem.io.I := io.I
  mem.io.OEB := io.OEB
  val selectMe_activeLow = !(!io.CSB && cascade_select1)
  mem.io.CSB := selectMe_activeLow
  mem.io.WEB := io.WEB
  mem.io.A := io.A.tail(cascadeBits)
  io.A_FW := RegNext(io.A)
  io.OEB_FW := RegNext(io.OEB)
  io.WEB_FW := RegNext(io.WEB)
  io.CSB_FW := RegNext(io.CSB ^ selectMe_activeLow)

  val cascade_select_stage = RegNext(cascade_select1)
  val data_in_stage = RegNext(io.I)
  io.O := Mux(cascade_select_stage, mem.io.O, data_in_stage)
}

private class CASICDPMemoryCascade(rows: Int,
                                   dataBits: Int,
                                   cascadeBits: Int,
                                   idx: Int
                                  )(implicit p: Parameters) extends RawModule {
  val mem = Module(new CMemoryDPSRAM(rows, dataBits))
  val totalAddr = log2Up(rows) + cascadeBits
  val io = IO(new DPSRAMBundle(totalAddr, dataBits) with withDPForwarding {})
  val cascade_select1 = idx.U === io.A1.head(cascadeBits)
  mem.io.CE1 := io.CE1
  io.O1 := mem.io.O1
  mem.io.I1 := io.I1
  mem.io.OEB1 := io.OEB1
  val selectMe_activeLow = !(!io.CSB1 && cascade_select1)
  mem.io.CSB1 := selectMe_activeLow
  mem.io.WEB1 := io.WEB1
  mem.io.A1 := io.A1.tail(cascadeBits)
  withClock(io.CE1.asClock) {
    io.A1_FW := RegNext(io.A1)
    io.OEB1_FW := RegNext(io.OEB1)
    io.WEB1_FW := RegNext(io.WEB1)
    io.CSB1_FW := RegNext(io.CSB1 ^ selectMe_activeLow)

    val cascade_select_stage = RegNext(cascade_select1)
    val data_in_stage = RegNext(io.I1)
    io.O1 := Mux(cascade_select_stage, mem.io.O1, data_in_stage)
  }


  val cascade_select2 = idx.U === io.A2.head(cascadeBits)
  mem.io.CE2 := io.CE2
  io.O2 := mem.io.O2
  mem.io.I2 := io.I2
  mem.io.OEB2 := io.OEB2
  val selectMe2_activeLow = !(!io.CSB2 && cascade_select2)
  mem.io.CSB2 := selectMe2_activeLow
  mem.io.WEB2 := io.WEB2
  mem.io.A2 := io.A2.tail(cascadeBits)
  withClock(io.CE2.asClock) {
    io.A2_FW := RegNext(io.A2)
    io.OEB2_FW := RegNext(io.OEB2)
    io.WEB2_FW := RegNext(io.WEB2)
    io.CSB2_FW := RegNext(io.CSB2 ^ selectMe2_activeLow)

    val cascade_select_stage2 = RegNext(cascade_select2)
    val data_in_stage2 = RegNext(io.I2)
    io.O2 := Mux(cascade_select_stage2, mem.io.O2, data_in_stage2)
  }
}

// SRAM DIM
case class SD(dWidth: Int, nRows: Int)

case class CascadeDescriptor(depth: Int, bankWidths: Seq[Int])

abstract class MemoryCompiler {
  val DP_mems: Seq[SD]
  val SP_mems: Seq[SD]
  val isActiveHighSignals: Boolean

  def getMemoryName(nPorts: Int, nRows: Int, nColumns: Int): String

  def getDPMemoryCascade(suggestedRows: Int, suggestedColumns: Int, latency: Int): CascadeDescriptor = {
    getMemoryCascade(suggestedRows, suggestedColumns, latency, DP_mems)
  }

  def getSPMemoryCascade(suggestedRows: Int, suggestedColumns: Int, latency: Int): CascadeDescriptor = {
    getMemoryCascade(suggestedRows, suggestedColumns, latency, SP_mems)
  }

  private def getMemoryCascade(suggestedRows: Int, suggestedColumns: Int, latency: Int, memSet: Seq[SD]): CascadeDescriptor = {
    // first figure out how deep we have to cascade.
    val maxRows = memSet.map(_.nRows).max
    require(latency * maxRows >= suggestedRows,
      f"Unable to build a $suggestedRows deep memory in $latency cycles. ${(suggestedRows.toFloat / maxRows).ceil.toInt} cycles needed.")
    // find the minimum depth that will make us able to build the cascade
    val chosenDepth = memSet.map(_.nRows).filter(depth => depth * latency >= suggestedRows).min
    // get the widest memory with the selected depth

    /**
     * Sieve of eratosthenes-ish except record shortest sequence to get to a certain width given the basis
     */
    @tailrec
    def sieve(lim: Int, numberChoices: List[Int], originators: Map[Int, List[Int]]):
    Map[Int, List[Int]] = {
      def get_add_list(): Map[Int, List[Int]] = {
        if (numberChoices.isEmpty) return Map.empty
        val inc = numberChoices.head
        val addList = originators.map(o => (o._1 + inc, inc :: o._2))
        val filter = addList.filter { kv =>
          if (kv._1 > lim) false
          else if (!originators.contains(kv._1)) true
          else {
            val other = originators(kv._1)
            if (other.length > kv._2.length) true
            else false
          }
        }
        filter
      }

      val AL = get_add_list()
      if (AL.isEmpty) {
        if (numberChoices.isEmpty) originators
        else sieve(lim, numberChoices.tail, originators)
      } else {
        sieve(lim, numberChoices, originators.removedAll(AL.keys) ++ AL)
      }
    }

    val max_width = 1 << log2Up(suggestedColumns)
    val basis = memSet.map(_.dWidth).distinct.toList
    val sieveRes = sieve(max_width, basis,
      Map.from(basis.map(p => (p, List(p)))))
    val chosenWidth = sieveRes.filter(p => p._1 >= suggestedColumns).toList.minBy(_._1)
    CascadeDescriptor(chosenDepth, chosenWidth._2)
  }
}

object MemoryCompiler {

  def buildDPSRAM(latency: Int, dataWidth: Int, nRows: Int)(implicit io: DPSRAMBundle, p: Parameters): Unit = {

    val memoryStructure = p(ASICMemoryCompilerKey).getDPMemoryCascade(nRows, dataWidth, latency)
    val totalRowBits = log2Up(nRows)
    val cascadeRows = Seq.fill(latency)(memoryStructure.depth)
    //
    val addressBases = cascadeRows.zip(cascadeRows.scan(0)(_ + _)).map {
      case (sz, sum) => sum >> log2Up(sz)
    }
    val banks = Seq.tabulate(memoryStructure.bankWidths.length) { bank_idx =>
      val data_offset = (0 until bank_idx).map(memoryStructure.bankWidths(_)).sum
      val bank_width = memoryStructure.bankWidths(bank_idx)
      val bank_i1 = io.I1(data_offset + bank_width - 1, data_offset)
      val bank_i2 = io.I2(data_offset + bank_width - 1, data_offset)

      val bank_O1 = Wire(UInt(bank_width.W))
      val bank_O2 = Wire(UInt(bank_width.W))
      val cascade = Seq.tabulate(latency)(idx =>
        Module(
          new CASICDPMemoryCascade(
            cascadeRows(idx),
            bank_width,
            totalRowBits - log2Up(cascadeRows(idx)),
            addressBases(idx)
          )
        )
      )
      cascade zip cascade.tail foreach { case (front, back) =>
        front.io.connectTo(back.io)
      }
      cascade.foreach { csc => csc.io.CE1 := io.CE1; csc.io.CE2 := io.CE2 }
      val head = cascade.head
      val tail = cascade.last
      head.io.A1 := io.A1
      head.io.A2 := io.A2
      head.io.I1 := bank_i1
      head.io.I2 := bank_i2
      head.io.CSB1 := io.CSB1
      head.io.CSB2 := io.CSB2
      head.io.WEB1 := io.WEB1
      head.io.WEB2 := io.WEB2
      head.io.OEB1 := io.OEB1
      head.io.OEB2 := io.OEB2
      bank_O1 := tail.io.O1
      bank_O2 := tail.io.O2
      (bank_idx, bank_O1, bank_O2)
    }
    io.O1 := Cat(banks.sortBy(_._1).reverse.map(_._2))
    io.O2 := Cat(banks.sortBy(_._1).reverse.map(_._3))

  }
}


/**
 * ALL LOGIC IMPLEMENTED HERE MUST BE ACTIVE LOW
 */
class CASICMemory(latency: Int, dataWidth: Int, nRowsSuggested: Int)(implicit p: Parameters)
  extends RawModule
    with HasCMemoryIO {
  private val nRows = Math.max(nRowsSuggested, 4 * latency)
  override val desiredName = f"CMemoryASIC_l${latency}dw${dataWidth}r$nRows"
  implicit val io = IO(new DPSRAMBundle(log2Up(nRows), dataWidth))

  MemoryCompiler.buildDPSRAM(latency, dataWidth, nRows)
}

class CASICMemoryWithActiveHighSelects(latency: Int, dataWidth: Int, nRows: Int)(implicit p: Parameters)
    extends RawModule
    with HasMemoryInterface {
  val io = IO(new DPSRAMBundle(log2Up(nRows), dataWidth))

  val mod = Module(
    new CASICMemory(
      latency = latency,
      dataWidth = dataWidth,
      nRowsSuggested = nRows
    )
  )
  mod.io.elements.foreach { case (name, dat) =>
    if (name.substring(0, 2) == "CE") // clock edge
      dat <> io.elements(name)
    else if (dat.isInstanceOf[Bool])
      dat <> !io.elements(name).asInstanceOf[Bool]
    else {
      dat <> io.elements(name)
    }
  }
}
