package composer.Platforms.FPGA.Xilinx.Templates

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._
import composer.Generation.ComposerBuild
import composer.MemoryStreams.HasMemoryInterface
import composer.Platforms.FPGA.Xilinx.FPGAMemoryPrimitiveConsumer
import composer.Platforms._
import composer._

import java.io.FileWriter
import scala.annotation.tailrec


private[composer] class BRAMTDP(latency: Int,
                                dataWidth: Int,
                                nRows: Int,
                                debugName: String,
                                     )(implicit p: Parameters) extends BlackBox with HasMemoryInterface {
  val io = IO(new Bundle {
    val CE = Input(Bool())
    val WEB1 = Input(Bool())
    val WEB2 = Input(Bool())
    val OEB1 = Input(Bool())
    val OEB2 = Input(Bool())
    val O1 = Output(UInt(dataWidth.W))
    val O2 = Output(UInt(dataWidth.W))
    val I1 = Input(UInt(dataWidth.W))
    val I2 = Input(UInt(dataWidth.W))
    val A1 = Input(UInt(log2Up(nRows).W))
    val A2 = Input(UInt(log2Up(nRows).W))
    val CSB1 = Input(Bool())
    val CSB2 = Input(Bool())
  })

  override def addr: Seq[UInt] = Seq(io.A1, io.A2)

  override def data_out: Seq[UInt] = Seq(io.O1, io.O2)

  override def data_in: Seq[UInt] = Seq(io.I1, io.I2)

  override def chip_select: Seq[Bool] = Seq(io.CSB1, io.CSB2)

  override def read_enable: Seq[Bool] = Seq(io.OEB1, io.OEB2)

  override def write_enable: Seq[Bool] = Seq(io.WEB1, io.WEB2)

  override def clocks: Seq[Bool] = Seq(io.CE)

  require(
    latency >= 1,
    "Scratchpad latency must be at least 1. Found " + latency
  )
  private val addrWidth = log2Up(nRows)

  val dname_prefix = composer.MemoryStreams.Memory.get_name(latency, dataWidth, nRows, 0, 0, 2)
  val (memoryAnnotations, dname_suffix) = {
    if (
      p(ConstraintHintsKey).contains(ComposerConstraintHint.MemoryConstrained)
    ) {
      val info = BRAMTDP.getMemoryResources(nRows, dataWidth, debugName, isSimple = false)
      BRAMTDP.allocateBRAM(info.brams)
      BRAMTDP.allocateURAM(info.urams)
      (info.verilogAnnotations, info.fileNameAnnotation)
    } else ("", "")
  }

  override val desiredName = f"$dname_prefix$dname_suffix"

  private val memoryRoot = os.pwd / ".memories"
  if (!os.exists(memoryRoot)) os.makeDir(memoryRoot)

  private val component = memoryRoot / f"$desiredName.v"

  ComposerBuild.addSource(component)

  val mvR = if (latency > 1) {
    f"""for (i = 0; i < $latency-1; i = i+1) begin
      |      mem_pipe_reg1[i+1] <= mem_pipe_reg1[i];
      |      mem_pipe_reg2[i+1] <= mem_pipe_reg2[i];
      |  end
      |""".stripMargin
  } else ""

  // We need keep hirarchy because in some rare circumstances, cross boundary optimization
  // prevents the memory from being inferred, and further, the memory is completely unrecongized,
  // mapped to a black box, and causes unrecoverable errors during logic synthesis... (Vivado 2022.1)
  val src =
  f"""
     |(* keep_hierarchy = "yes" *)
     |module $desiredName (
     |  input CE,
     |  input WEB1,
     |  input WEB2,
     |  input OEB1,
     |  input OEB2,
     |  output reg [${dataWidth - 1}:0] O1,
     |  input [${dataWidth - 1}:0] I1,
     |  input [${addrWidth - 1}:0] A1,
     |  output reg [${dataWidth - 1}:0] O2,
     |  input [${dataWidth - 1}:0] I2,
     |  input [${addrWidth - 1}:0] A2,
     |  input CSB1,
     |  input CSB2);
     |
     |$memoryAnnotations
     |reg [${dataWidth - 1}:0] mem [${nRows - 1}:0];        // Memory Declaration
     |reg [${dataWidth - 1}:0] memreg1;
     |reg [${dataWidth - 1}:0] memreg2;
     |reg [${dataWidth - 1}:0] mem_pipe_reg1 [${latency - 1}:0];    // Pipelines for memory
     |reg [${dataWidth - 1}:0] mem_pipe_reg2 [${latency - 1}:0];    // Pipelines for memory
     |
     |integer          i;
     |always @ (posedge CE)
     |begin
     |  if(CSB1) begin
     |    if (WEB1) begin
     |      mem[A1] <= I1;
     |    end else begin
     |      memreg1 <= mem[A1];
     |    end
     |  end
     |end
     |
     |always @ (posedge CE)
     |begin
     |  if(CSB2) begin
     |    if (WEB2) begin
     |      mem[A2] <= I2;
     |    end else begin
     |      memreg2 <= mem[A2];
     |    end
     |  end
     |end
     |
     |always @ (posedge CE)
     |begin
     |  mem_pipe_reg1[0] <= memreg1;
     |  mem_pipe_reg2[0] <= memreg2;
     |  $mvR
     |  O1 <= mem_pipe_reg1[$latency-1];
     |  O2 <= mem_pipe_reg2[$latency-1];
     |end
     |endmodule
     |
     |""".stripMargin

  val fw = new FileWriter(component.toString())
  fw.write(src)
  fw.close()

}

object BRAMTDP {
  private[composer] var warningsIssued = 0
  private var bram_used = 0
  private var uram_used = 0

  private val uram_dwidth = 72
  private val uram_nrows = 4 * 1024

  private[composer] def bram_dwidth(isSimple: Boolean) = List(1, 2, 4, 9, 18, 36) ++ (if (isSimple) Seq(72) else Seq())

  def get_bram_width(requested: Int, isSimple: Boolean): Int = {
    @tailrec
    def help(h: List[Int]): Int = {
      if (h.isEmpty) bram_dwidth(isSimple).max
      else if (requested <= h.head) h.head
      else help(h.tail)
    }

    help(bram_dwidth(isSimple))
  }

  // get bram and uram usage respectively for a given memory
  def getMemoryResources(nRows: Int, dwidth: Int, debugName: String, isSimple: Boolean)(implicit p: Parameters): FPGAMemoryPrimitiveConsumer = {
    def get_n_brams(widthRequested: Int, rowsRequested: Int): Int = {
      val w = get_bram_width(widthRequested, isSimple)
      val rows_per_bram = bram_width2rows(isSimple)(w)
      // if asking for a super wide BRAM, then they're likely going to be cascaded together
      val cascade = if (widthRequested > w) {
        (widthRequested.toFloat / w).ceil.toInt
      } else 1
      (rowsRequested.toFloat / rows_per_bram).ceil.toInt * cascade
    }

    def get_n_urams(widthRequested: Int, rowsRequested: Int): Int = {
      val row_consumption = (rowsRequested.toFloat / uram_nrows).ceil.toInt
      val width_mult = (widthRequested.toFloat / uram_dwidth).ceil.toInt
      width_mult * row_consumption
    }

    val useURAM = nRows >= 4 * 1024 && dwidth >= 64

    if (nRows >= 4 * 1024 && dwidth < 64) {
      if (!p(ComposerQuiet) && warningsIssued < 5) {
        System.err.println(
          s"One of the memory modules ($debugName) has a data width less than 64 ($dwidth) but has a total\n" +
            s"data capacity that makes it appropriate for URAM (applicable for Ultrascale+ devices)\n" +
            s"This may lead to poor URAM cascading. Consider increasing the width if possible."
        )
        warningsIssued += 1
      }
    }

    // appropriate data width and at least 90% capacity
    val uram_consumption = get_n_urams(dwidth, nRows)
    val bram_consumption = get_n_brams(dwidth, nRows)
    val have_enough_uram = uram_used + uram_consumption <= p(PlatformNURAM)
    val have_enough_bram = bram_used + bram_consumption <= p(PlatformNBRAM)
    val usage = if (useURAM && have_enough_uram) FPGAMemoryPrimitiveConsumer(0, uram_consumption, "(* ram_style = \"ultra\" *)", "_URAM")
    else if (have_enough_bram) FPGAMemoryPrimitiveConsumer(bram_consumption, 0, "(* ram_style = \"block\" *)", "_BRAM")
    else if (have_enough_uram) FPGAMemoryPrimitiveConsumer(0, uram_consumption, "(* ram_style = \"ultra\" *)", "_URAM")
    else {
      System.err.println(
        s"Memory module $debugName requires $bram_consumption BRAMs and $uram_consumption URAMs,\n" +
          s" but only ${p(PlatformNBRAM) - bram_used} BRAMs and ${p(PlatformNURAM) - uram_used} URAMs\n" +
          s"are available.")
      FPGAMemoryPrimitiveConsumer(0, 0, "", "")
    }

    if (!p(ComposerQuiet)) {
      System.err.println(s"Using ${usage.brams} BRAMs and ${usage.urams} URAMs for $debugName: $nRows x $dwidth")
      System.err.println(s"Total Usage - BRAM(${BRAMTDP.bram_used + usage.brams}/${p(PlatformNBRAM)}) URAM(${BRAMTDP.uram_used + usage.urams}/${p(PlatformNURAM)})")
    }
    usage
  }

  private def bram_maxdwidth(isSimple: Boolean) = if (isSimple) 72 else 36

  private def bram_width2rows(isSimple: Boolean) = Map.from({
    Seq(
      (1, 32 * 1024),
      (2, 16 * 1024),
      (4, 8 * 1024),
      (9, 4 * 1024),
      (18, 2 * 1024),
      (36, 1024)) ++ (if (isSimple) Seq((72, 512)) else Seq())
  })

  def allocateBRAM(nBRAM: Int): Unit = {
    bram_used += nBRAM
  }

  def allocateURAM(nURAM: Int): Unit = {
    uram_used += nURAM
  }

}