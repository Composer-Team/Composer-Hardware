package composer.MemoryStreams.RAM

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util.log2Up
import composer.MemoryStreams.HasMemoryInterface
import composer.MemoryStreams.RAM.CFPGAMemory.{bram_used, uram_used}
import composer._

import java.io.FileWriter
import scala.annotation.tailrec

case object SimpleDRAMHintKey extends Field[Boolean]

object CFPGAMemory {
  private var bram_used = 0
  private var uram_used = 0
}


//noinspection ScalaUnusedSymbol
private[MemoryStreams] class CFPGAMemory(latency: Int,
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

  private val bram_dwidth = List(1, 2, 4, 9, 18, 36) ++ (if (p(SimpleDRAMHintKey)) Seq(72) else Seq())
  private val bram_maxdwidth = if (p(SimpleDRAMHintKey)) 72 else 36
  private val uram_dwidth = 72
  private val uram_nrows = 4 * 1024
  private val bram_width2rows = Map.from({
    Seq(
      (1, 32 * 1024),
      (2, 16 * 1024),
      (4, 8 * 1024),
      (9, 4 * 1024),
      (18, 2 * 1024),
      (36, 1024)) ++ (if (p(SimpleDRAMHintKey)) Seq(Tuple2(72, 512)) else Seq())
  })

  val dname_prefix = s"CMemoryL${latency}DW${dataWidth}R$nRows"
  override val desiredName = f"$dname_prefix$dname_suffix"

  private val memoryRoot = os.pwd / ".memories"
  if (!os.exists(memoryRoot)) os.makeDir(memoryRoot)

  private val component = memoryRoot / f"$desiredName.v"

  ComposerBuild.addSource(component)


  val (memoryAnnotations, dname_suffix) = {
    if (
      p(ConstraintHintsKey).contains(ComposerConstraintHint.MemoryConstrained)
    ) {
      val useURAM = if (nRows > 4 * 1024 && dataWidth < 64) {
        if (!p(ComposerQuiet)) {
          System.err.println(
            s"One of the memory modules ($debugName) has a data width less than 64 ($dataWidth) but has a total\n" +
              s"data capacity that makes it appropriate for URAM (applicable for Ultrascale+ devices)\n" +
              s"This may lead to poor URAM cascading. Consider increasing the width if possible."
          )
        }
        true
      } else false
      // appropriate data width and at least 90% capacity
      val uram_consumption = get_n_urams(dataWidth, nRows)
      val bram_consumption = get_n_brams(dataWidth, nRows)
      val have_enough_uram = uram_used + uram_consumption < p(PlatformNURAM)
      val have_enough_bram = bram_used + bram_consumption < p(PlatformNBRAM)
      val ret = if ((dataWidth >= 64 && nRows >= 4 * 1024 * 0.5 && have_enough_uram) || useURAM) {
        uram_used = uram_used + uram_consumption
        System.err.println(
          s"Using $uram_consumption urams for $debugName ($dataWidth, $nRows)- $dname_prefix"
        )
        ("(* ram_style = \"ultra\" *)", "constU")
      } else if (have_enough_bram) {
        if (nRows >= 32) {
          System.err.println(
            s"Using $bram_consumption brams for $debugName ($dataWidth, $nRows) - $dname_prefix"
          )
          bram_used = bram_used + bram_consumption
          ("(* ram_style = \"block\" *)", "constB")
        } else {
          System.err.println(s"Memory ${nRows}x${dataWidth} is too small for BRAM and URAM, leaving it up to the synthesizer")
          ("", "")
        }
      } else {
        System.err.println(
          "Memory Constrained Hint Warning: URAM and BRAM may be entirely consumed by requested\n" +
            "memory. Design may be too big for given platform.\n" +
            s"BRAM Used: $bram_used/${p(PlatformNBRAM)}\n" +
            s"URAM Used: $uram_used/${p(PlatformNURAM)}"
        )
        ("", "constX")
      }
      System.err.println(s"Total Usage - BRAM($bram_used/${p(PlatformNBRAM)}) URAM($uram_used/${p(PlatformNURAM)})")
      ret
    } else ("", "")
  }

  @tailrec
  private def get_bram_width(
                              requested: Int,
                              h: List[Int] = bram_dwidth
                            ): Int = {
    if (h.isEmpty) bram_maxdwidth
    else if (requested <= h.head) h.head
    else get_bram_width(requested, h.tail)
  }

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

  val src =
    f"""
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
       |reg [$latency:0] mem_en_pipe_reg1;                // Pipelines for memory enable
       |reg [$latency:0] mem_en_pipe_reg2;                // Pipelines for memory enable
       |
       |integer          i;
       |always @ (posedge CE)
       |begin
       |  if(CSB1) begin
       |    if (WEB1) begin
       |      mem[A1] <= I1;
       |    end
       |    if (OEB1) begin
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
       |    end
       |    if (OEB2) begin
       |      memreg2 <= mem[A2];
       |    end
       |  end
       |end
       |
       |always @ (posedge CE)
       |begin
       |  mem_en_pipe_reg1[0] <= CSB1 && OEB1;
       |  for (i=0; i<$latency; i=i+1) begin
       |    mem_en_pipe_reg1[i+1] <= mem_en_pipe_reg1[i];
       |  end
       |
       |  mem_en_pipe_reg2[0] <= CSB2 && OEB2;
       |  for (i=0; i<$latency; i=i+1) begin
       |    mem_en_pipe_reg2[i+1] <= mem_en_pipe_reg2[i];
       |  end
       |
       |end
       |
       |// RAM output data goes through a pipeline.
       |always @ (posedge CE)
       |begin
       |  if (mem_en_pipe_reg1[0]) begin
       |    mem_pipe_reg1[0] <= memreg1;
       |  end
       |  if (mem_en_pipe_reg2[0]) begin
       |    mem_pipe_reg2[0] <= memreg2;
       |  end
       |
       |end
       |
       |always @ (posedge CE)
       |begin
       |  for (i = 0; i < $latency-1; i = i+1) begin
       |    if (mem_en_pipe_reg1[i+1]) begin
       |      mem_pipe_reg1[i+1] <= mem_pipe_reg1[i];
       |    end
       |    if (mem_en_pipe_reg2[i+1]) begin
       |      mem_pipe_reg2[i+1] <= mem_pipe_reg2[i];
       |    end
       |
       |  end
       |end
       |
       |// Final output register gives user the option to add a reset and
       |// an additional enable signal just for the data ouptut
       |always @ (posedge CE)
       |begin
       |  if (mem_en_pipe_reg1[$latency]) begin
       |    O1 <= mem_pipe_reg1[$latency-1];
       |  end
       |  if (mem_en_pipe_reg2[$latency]) begin
       |    O2 <= mem_pipe_reg2[$latency-1];
       |  end
       |
       |end
       |endmodule
       |
       |""".stripMargin

  val fw = new FileWriter(component.toString())
  fw.write(src)
  fw.close()

}
