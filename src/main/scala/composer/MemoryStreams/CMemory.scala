package composer.MemoryStreams

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.MemoryStreams.CMemory._
import composer._
import composer.Generation.CppGeneration.safe_join

import java.io.FileWriter
import scala.annotation.tailrec

object CMemory {
  def apply(latency: Int, dataWidth: Int, nRows: Int)(implicit p: Parameters): () => CMemory = {
    () => new CFPGAMemory(latency, dataWidth, nRows)
  }

  private[CFPGAMemory] var bram_used = 0
  private[CFPGAMemory] var uram_used = 0
}

/**
 * Experimental: Thinking this may be easier for Vivado to recognize as URAM?
 * Currently encountering problems having Vivado infer the right number of 'pipeline' stages
 *
 * 1 WRITE PORT
 * 1 READ PORT
 */

abstract class CMemory(dataWidth: Int, nRows: Int)(implicit p: Parameters) extends BlackBox {
  private val addrWidth = log2Up(nRows)
  val io = IO(new Bundle() {
    val clk = Input(Clock())
    val rst = Input(Reset())

    // read port
    val r_mem_en = Input(Bool())
    val r_addr = Input(UInt(addrWidth.W))
    val r_regce = Input(Bool())
    val r_dout = Output(UInt(dataWidth.W))

    // write port
    val w_mem_en = Input(UInt((dataWidth / 8).W))
    val w_din = Input(UInt(dataWidth.W))
    val w_addr = Input(UInt(addrWidth.W))
  })
}
//noinspection ScalaUnusedSymbol
private[MemoryStreams] class CFPGAMemory(latency: Int, dataWidth: Int, nRows: Int)(implicit p: Parameters) extends CMemory(dataWidth, nRows) {
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
          s"One of the memory modules has a data width less than 64 ($dataWidth) but has a total\n" +
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
        System.err.println(s"Using $uram_consumption urams for $dname_prefix")
        ("(* ram_style = \"ultra\" *)", "constU")
      } else if (have_enough_bram) {
        System.err.println(s"Using $bram_consumption brams for $dname_prefix")
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

  val dataWidthBlockWidth = dataWidth / 8

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
       |reg [7:0] mem [${nRows - 1}:0][$dataWidthBlockWidth];        // Memory Declaration
       |reg [${dataWidth - 1}:0] memreg;
       |reg [${dataWidth - 1}:0] mem_pipe_reg [${latency - 1}:0];    // Pipelines for memory
       |reg [$latency:0] mem_en_pipe_reg;                // Pipelines for memory enable
       |
       |integer          i;
       |always @ (posedge clk)
       |begin
       |  if(r_mem_en) begin
       |    memreg <= {${safe_join((0 until dataWidthBlockWidth).reverse map (a => f"mem[r_addr][$a]"), ", ")}};
       |  end
       |end
       |
       |always @ (posedge clk)
       |begin
       |  for (i = 0; i < $dataWidthBlockWidth; i = i + 1) begin
       |    if(w_mem_en[i]) begin
       |      mem[w_addr][i] <= w_din[(i+1)*8 -1: i*8];
       |    end
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

private[MemoryStreams] class CASICMemory(dataWidth: Int, nRows: Int)(implicit p: Parameters) extends CMemory(dataWidth, nRows) {
  // figure out

}
