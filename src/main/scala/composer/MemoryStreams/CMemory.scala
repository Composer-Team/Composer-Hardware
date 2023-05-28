package composer.MemoryStreams

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer._
import composer.MemoryStreams.CFPGAMemory.{bram_used, uram_used}
import composer.MemoryStreams.SRAMUtil.portTy
import composer.MemoryStreams.SRAMUtil.portTy.portTy
import freechips.rocketchip.diplomacy.ValName

import java.io.FileWriter
import scala.annotation.tailrec
import scala.collection.SeqMap

object CMemory {
  def apply(latency: Int, dataWidth: Int, nRows: Int, debugName: Option[String] = None)(implicit p: Parameters, valName: ValName): DPSRAMBundle = {
    p(PlatformTypeKey) match {
      case PlatformType.FPGA =>
        val cmem = Module(new CFPGAMemory(latency - 2, dataWidth, nRows, debugName = debugName.getOrElse(valName.name)))
        cmem.suggestName(valName.name)
        cmem.io
      case PlatformType.ASIC =>
        val cmem = Module(new CASICMemoryWithActiveHighSelects(latency, dataWidth, nRows))
        cmem.suggestName(valName.name)
        cmem.io
    }
  }
}

object CFPGAMemory {
  private var bram_used = 0
  private var uram_used = 0
}

trait HasCMemoryIO {
  val io: DPSRAMBundle
}

//noinspection ScalaUnusedSymbol
private[MemoryStreams] class CFPGAMemory(latency: Int, dataWidth: Int, nRows: Int, debugName: String)(implicit p: Parameters) extends BlackBox with HasCMemoryIO {
  val io = IO(new DPSRAMBundle(log2Up(nRows), dataWidth))
  require(latency >= 1, "Scratchpad latency must be at least 1. Found " + latency)
  private val addrWidth = log2Up(nRows)

  private val bram_dwidth = List(1, 2, 4, 9, 18, 36)
  private val bram_maxdwidth = 36
  private val uram_dwidth = 72
  private val uram_nrows = 4 * 1024

  @tailrec
  private def get_bram_width(requested: Int, h: List[Int] = bram_dwidth): Int = {
    if (h.isEmpty) 36
    else if (requested <= h.head) h.head
    else get_bram_width(requested, h.tail)
  }

  private val bram_width2rows = Map.from(Seq((1, 32 * 1024), (2, 16 * 1024), (4, 8 * 1024), (9, 4 * 1024), (18, 2 * 1024), (36, 1024)))

  private def get_n_brams(widthRequested: Int, rowsRequested: Int): Int = {
    val w = get_bram_width(widthRequested)
    val rows_per_bram = bram_width2rows(w)
    // if asking for a super wide BRAM, then they're likely going to be cascaded together
    val cascade = if (widthRequested > w) {
      (widthRequested.toFloat / w).ceil.toInt
    } else 1
    (rowsRequested.toFloat / rows_per_bram).ceil.toInt * cascade
  }

  private def get_n_urams(widthRequested: Int, rowsRequested: Int): Int = {
    val row_consumption = (rowsRequested.toFloat / uram_nrows).ceil.toInt
    val width_mult = (widthRequested.toFloat / uram_dwidth).ceil.toInt
    width_mult * row_consumption
  }

  val dname_prefix = s"CMemoryL${latency}DW${dataWidth}R$nRows"
  val (memoryAnnotations, dname_suffix) = {
    if (p(ConstraintHintsKey).contains(ComposerConstraintHint.MemoryConstrained)) {
      if (nRows > 4 * 1024 && dataWidth < 64) {
        if (!p(ComposerQuiet)) System.err.println(
          s"One of the memory modules (${debugName}) has a data width less than 64 ($dataWidth) but has a total\n" +
            s"data capacity that makes it appropriate for URAM (applicable for Ultrascale+ devices)\n" +
            s"This may lead to poor URAM cascading. Consider increasing the width if possible.")
      }
      // appropriate data width and at least 90% capacity
      val uram_consumption = get_n_urams(dataWidth, nRows)
      val bram_consumption = get_n_brams(dataWidth, nRows)
      val have_enough_uram = uram_used + uram_consumption < p(PlatformNURAM)
      val have_enough_bram = bram_used + bram_consumption < p(PlatformNBRAM)
      if (dataWidth >= 64 && nRows >= 4 * 1024 * 0.9 && have_enough_uram) {
        uram_used = uram_used + uram_consumption
        System.err.println(s"Using $uram_consumption urams for $debugName - $dname_prefix")
        ("(* ram_style = \"ultra\" *)", "constU")
      } else if (have_enough_bram) {
        System.err.println(s"Using $bram_consumption brams for $debugName - $dname_prefix")
        bram_used = bram_used + bram_consumption
        ("(* ram_style = \"block\" *)", "constB")
      } else {
        System.err.println(
          "Memory Constrained Hint Warning: URAM and BRAM may be entirely consumed by requested\n" +
            "memory. Design may be too big for given platform.\n" +
            s"BRAM Used: $bram_used/${p(PlatformNBRAM)}\n" +
            s"URAM Used: $uram_used/${p(PlatformNURAM)}")
        ("", "constX")
      }
    } else ("", "")
  }
  override val desiredName = f"$dname_prefix$dname_suffix"

  private val memoryRoot = os.pwd / ".memories"
  if (!os.exists(memoryRoot)) os.makeDir(memoryRoot)

  private val component = memoryRoot / f"$desiredName.v"

  ComposerBuild.addSource(component)

  val src =
    f"""
       |module $desiredName (
       |  input clk,
       |  input rst,
       |  input r_mem_en,
       |  input [${addrWidth - 1}:0] r_addr,
       |  input r_regce,
       |  output reg [${dataWidth - 1}:0] r_dout,
       |
       |  input w_mem_en,
       |  input [${dataWidth - 1}:0] w_din,
       |  input [${addrWidth - 1}:0] w_addr);
       |
       |$memoryAnnotations
       |reg [${dataWidth - 1}:0] mem [${nRows - 1}:0];        // Memory Declaration
       |reg [${dataWidth - 1}:0] memreg;
       |reg [${dataWidth - 1}:0] mem_pipe_reg [${latency - 1}:0];    // Pipelines for memory
       |reg [$latency:0] mem_en_pipe_reg;                // Pipelines for memory enable
       |
       |integer          i;
       |always @ (posedge clk)
       |begin
       |  if(r_mem_en) begin
       |    memreg <= mem[r_addr];
       |  end
       |end
       |
       |always @ (posedge clk)
       |begin
       |  if(w_mem_en) begin
       |     mem[w_addr] <= w_din;
       |  end
       |end
       |
       |always @ (posedge clk)
       |begin
       |  mem_en_pipe_reg[0] <= r_mem_en;
       |  for (i=0; i<$latency; i=i+1) begin
       |    mem_en_pipe_reg[i+1] <= mem_en_pipe_reg[i];
       |  end
       |end
       |
       |// RAM output data goes through a pipeline.
       |always @ (posedge clk)
       |begin
       |  if (mem_en_pipe_reg[0]) begin
       |    mem_pipe_reg[0] <= memreg;
       |  end
       |end
       |
       |always @ (posedge clk)
       |begin
       |  for (i = 0; i < $latency-1; i = i+1) begin
       |    if (mem_en_pipe_reg[i+1]) begin
       |      mem_pipe_reg[i+1] <= mem_pipe_reg[i];
       |    end
       |  end
       |end
       |
       |// Final output register gives user the option to add a reset and
       |// an additional enable signal just for the data ouptut
       |always @ (posedge clk)
       |begin
       |  if (mem_en_pipe_reg[$latency] && r_regce) begin
       |    r_dout <= mem_pipe_reg[$latency-1];
       |  end
       |end
       |endmodule
       |
       |""".stripMargin

  val fw = new FileWriter(component.toString())
  fw.write(src)
  fw.close()
}

private[composer] object SRAMUtil {
  object portTy extends Enumeration {
    val dual, single = Value
    type portTy = Value
  }

  // return name of SRAM
  def generateSRAM(pt: portTy, rows: Int, cols: Int): String = {
    if (rows < 4 || rows > 1024) {
      throw new Exception(f"Memory config not available ${rows}x$cols")
    }
    val name = "SRAM" + (pt match {
      case portTy.dual => "2RW"
      case portTy.single => "1RW"
    }) + rows + "x" + cols
    //    return name
    val run_root = os.root / "usr" / "xtmp" / "cmk91" / "install" / "saed_mc_v3_0_1" / "saed_mc"
    val outputDir = run_root / name
    if (!os.exists(run_root))
      throw new Exception("Unable to find memory compiler...")
    if (!os.exists(outputDir / f"$name.v")) {
      val configname = f"${name}_config.cfg"
      val config = run_root / configname
      os.write.over(config,
        f"""mem_type=${if (pt == portTy.dual) "dual" else "single"}_14
           |word_count=$rows
           |word_bits=$cols
           |do_spice=1
           |do_gds=1
           |do_logic=1
           |do_lef=0
           |do_drc=0
           |do_lvs=1
           |do_cx=0
           |do_rcx=0
           |do_lib_nldm=1
           |do_mw=0
           |do_ndm=1
           |work_dir=$name
           |do_toolchoose=icv""".stripMargin)
      println(config.toString())
      val proc = os.proc(Seq("bash", "run_me.sh", (os.home / ".bashrc").toString(), configname)).call(run_root, stdout = os.Inherit)
      if (proc.exitCode != 0) throw new Exception("FAILED")
    }
    ComposerBuild.addSource(outputDir / f"$name.v")
    ComposerBuild.addSymbolicResource(outputDir)
    name
  }
}

class DPSRAMBundle(val addrBits: Int, dataBits: Int) extends Bundle {
  val A1 = Input(UInt(addrBits.W))
  val CE1 = Input(Bool())
  val WEB1 = Input(Bool())
  val OEB1 = Input(Bool())
  val CSB1 = Input(Bool())
  val I1 = Input(UInt(dataBits.W))
  val O1 = Output(UInt(dataBits.W))

  val A2 = Input(UInt(addrBits.W))
  val CE2 = Input(Bool())
  val WEB2 = Input(Bool())
  val OEB2 = Input(Bool())
  val CSB2 = Input(Bool())
  val I2 = Input(UInt(dataBits.W))
  val O2 = Output(UInt(dataBits.W))
}

trait withSupportForwarding {
  val elements: SeqMap[String, Data]

  def connectTo(other: Bundle): Unit = {
    elements.filter(_._1.contains("_FW")).foreach { case (name, dat) =>
      val o = other.elements(name.substring(0, name.length - 3))
      o := dat
    }
    other.elements.filter(_._1.substring(0, 1) == "I").foreach { case (name, dat) =>
      elements("O" + name.substring(1)) := dat
    }
  }
}

trait withDPForwarding extends withSupportForwarding {
  val addrBits: Int
  val A1_FW = Output(UInt(addrBits.W))
  val CSB1_FW = Output(Bool())
  val OEB1_FW = Output(Bool())
  val WEB1_FW = Output(Bool())

  val A2_FW = Output(UInt(addrBits.W))
  val CSB2_FW = Output(Bool())
  val OEB2_FW = Output(Bool())
  val WEB2_FW = Output(Bool())
}

trait withSPForwarding extends withSupportForwarding {
  val addrBits: Int
  val A_FW = Output(UInt(addrBits.W))
  val CSB_FW = Output(Bool())
  val OEB_FW = Output(Bool())
  val WEB_FW = Output(Bool())
}


private class CMemoryDPSRAM(rows: Int, dataBits: Int) extends BlackBox {
  override val desiredName = SRAMUtil.generateSRAM(portTy.dual, rows, dataBits)
  val addrBits = log2Up(rows)
  val io = IO(new DPSRAMBundle(addrBits, dataBits))
}

class SPSRAMBundle(val addrBits: Int, dataBits: Int) extends Bundle {
  val A = Input(UInt(addrBits.W))
  val CE = Input(Bool())
  val WEB = Input(Bool())
  val OEB = Input(Bool())
  val CSB = Input(Bool())
  val I = Input(UInt(dataBits.W))
  val O = Output(UInt(dataBits.W))
}

private class CMemorySPSRAM(rows: Int, dataBits: Int) extends BlackBox {
  override val desiredName = SRAMUtil.generateSRAM(portTy.single, rows, dataBits)
  val addrBits = log2Up(rows)
  val io = IO(new SPSRAMBundle(addrBits, dataBits))
}

private class CASICSPMemoryCascade(rows: Int, dataBits: Int, cascadeSize: Int, idx: Int) extends Module {
  val mem = Module(new CMemorySPSRAM(rows, dataBits))
  require(idx < cascadeSize, "sanity")
  val cascadeBits = log2Up(cascadeSize)
  val totalAddr = log2Up(rows) + cascadeBits
  val io = IO(new SPSRAMBundle(totalAddr, dataBits) with withSPForwarding)
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


private class CASICDPMemoryCascade(rows: Int, dataBits: Int, cascadeBits: Int, idx: Int) extends Module {
  val mem = Module(new CMemoryDPSRAM(rows, dataBits))
  val totalAddr = log2Up(rows) + cascadeBits
  val io = IO(new DPSRAMBundle(totalAddr, dataBits) with withDPForwarding {
  })

  io.WEB1
  val cascade_select1 = idx.U === io.A1.head(cascadeBits)
  mem.io.CE1 := clock.asBool
  io.O1 := mem.io.O1
  mem.io.I1 := io.I1
  mem.io.OEB1 := io.OEB1
  val selectMe_activeLow = !(!io.CSB1 && cascade_select1)
  mem.io.CSB1 := selectMe_activeLow
  mem.io.WEB1 := io.WEB1
  mem.io.A1 := io.A1.tail(cascadeBits)
  io.A1_FW := RegNext(io.A1)
  io.OEB1_FW := RegNext(io.OEB1)
  io.WEB1_FW := RegNext(io.WEB1)
  io.CSB1_FW := RegNext(io.CSB1 ^ selectMe_activeLow)


  val cascade_select_stage = RegNext(cascade_select1)
  val data_in_stage = RegNext(io.I1)
  io.O1 := Mux(cascade_select_stage, mem.io.O1, data_in_stage)

  val cascade_select2 = idx.U === io.A2.head(cascadeBits)
  mem.io.CE2 := clock.asBool
  io.O2 := mem.io.O2
  mem.io.I2 := io.I2
  mem.io.OEB2 := io.OEB2
  val selectMe2_activeLow = !(!io.CSB2 && cascade_select2)
  mem.io.CSB2 := selectMe2_activeLow
  mem.io.WEB2 := io.WEB2
  mem.io.A2 := io.A2.tail(cascadeBits)
  io.A2_FW := RegNext(io.A2)
  io.OEB2_FW := RegNext(io.OEB2)
  io.WEB2_FW := RegNext(io.WEB2)
  io.CSB2_FW := RegNext(io.CSB2 ^ selectMe2_activeLow)


  val cascade_select_stage2 = RegNext(cascade_select2)
  val data_in_stage2 = RegNext(io.I2)
  io.O2 := Mux(cascade_select_stage2, mem.io.O2, data_in_stage2)
}

class CASICMemory(latency: Int, dataWidth: Int, nRowsSuggested: Int) extends RawModule with HasCMemoryIO {
  private val nRows = Math.max(nRowsSuggested, 4 * latency)
  override val desiredName = f"CMemoryASIC_l${latency}dw${dataWidth}r$nRows"
  val io = IO(new DPSRAMBundle(dataWidth, nRows))

  case class MemoryGenerationResult(resultFreqMhz: Float, rows: Int, width: Int, moduleName: String)

  case class MemoryCascade(memoryGenerationResult: MemoryGenerationResult, memoryCascade: Option[MemoryCascade])

  def getCascade(rem: Int, divsRem: Int, lst: List[Int] = List.empty): List[Int] = {
    if (divsRem == 0) {
      require(rem <= 0)
      lst.reverse
    } else {
      val amt = 1 << log2Up(rem / divsRem)
      getCascade(rem - amt, divsRem - 1, amt :: lst)
    }
  }

  private val totalRowBits = log2Up(nRows)
  private val cascadeRows = getCascade(nRows, latency)
  private val addressBases = cascadeRows.zip(cascadeRows.scan(0)(_ + _)).map { case (sz, sum) => sum >> log2Up(sz) }
  private val readWidth = 1 << log2Up(dataWidth)
  private val cascade = Seq.tabulate(latency)(idx => Module(new CASICDPMemoryCascade(cascadeRows(idx), readWidth, totalRowBits - log2Up(cascadeRows(idx)), addressBases(idx))))
  cascade zip cascade.tail foreach { case (front, back) =>
    front.io.connectTo(back.io)
  }
  cascade.foreach { csc => csc.io.CE1 := io.CE1; csc.io.CE2 := io.CE2 }
  private val head = cascade.head
  private val tail = cascade.last
  head.io.A1 := io.A1
  head.io.A2 := io.A2
  head.io.I1 := io.I1
  head.io.I2 := io.I2
  head.io.CSB1 := io.CSB1
  head.io.CSB2 := io.CSB2
  head.io.WEB1 := io.WEB1
  head.io.WEB2 := io.WEB2
  head.io.OEB1 := io.OEB1
  head.io.OEB2 := io.OEB2
  io.O1 := tail.io.O1
  io.O2 := tail.io.O2
}

class CASICMemoryWithActiveHighSelects(latency: Int, dataWidth: Int, nRows: Int) extends RawModule with HasCMemoryIO {
  val io = IO(new DPSRAMBundle(log2Up(nRows), dataWidth))

  val mod = Module(new CASICMemory(latency = latency, dataWidth = dataWidth, nRowsSuggested = nRows))
  mod.io.elements.foreach { case (name, dat) =>
    if (name.substring(0, 2) == "CE") // clock edge
      dat := io.elements(name)
    else if (dat.isInstanceOf[Bool])
      dat := !io.elements(name).asInstanceOf[Bool]
    else
      dat := io.elements(name)
  }
}