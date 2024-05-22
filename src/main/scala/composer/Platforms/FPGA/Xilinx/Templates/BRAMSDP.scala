package composer.Platforms.FPGA.Xilinx.Templates

import chipsalliance.rocketchip.config.Parameters
import chisel3.util.log2Up
import chisel3._
import composer.Generation.ComposerBuild
import composer.MemoryStreams.HasMemoryInterface
import composer._

import java.io.FileWriter

//noinspection ScalaUnusedSymbol
private[composer] class BRAMSDP(latency: Int,
                                dataWidth: Int,
                                nRows: Int,
                                withWE: Boolean,
                                debugName: String,
                               )(implicit p: Parameters) extends BlackBox with HasMemoryInterface {
  val weWidth = if (withWE) dataWidth / 8 else 1
  val adjustedDW = if (withWE) 8 else dataWidth
  val io = IO(new Bundle {
    val CE = Input(Bool())
    val WEB = Input(UInt(weWidth.W))
    val OEB = Input(Bool())
    val O = Output(UInt(dataWidth.W))
    val I = Input(UInt(dataWidth.W))
    val A_read = Input(UInt(log2Up(nRows).W))
    val A_write = Input(UInt(log2Up(nRows).W))
    val CSB_read = Input(Bool())
    val CSB_write = Input(Bool())
  })

  override def addr: Seq[UInt] = Seq(io.A_read, io.A_write)

  override def data_out: Seq[UInt] = Seq(io.O)

  override def data_in: Seq[UInt] = Seq(io.I)

  override def chip_select: Seq[Bool] = Seq(io.CSB_read, io.CSB_write)

  override def read_enable: Seq[Bool] = Seq(io.OEB)

  override def write_enable = Seq(io.WEB)

  override def clocks: Seq[Bool] = Seq(io.CE)

  require(
    latency >= 1,
    "Scratchpad latency must be at least 1. Found " + latency
  )
  private val addrWidth = log2Up(nRows)

  val dname_prefix = composer.MemoryStreams.Memory.get_name(latency, dataWidth, nRows, 1, 1, 0)
  val (memoryAnnotations, dname_suffix) = {
    if (
      p(ConstraintHintsKey).contains(ComposerConstraintHint.MemoryConstrained)
    ) {
      val info = BRAMTDP.getMemoryResources(nRows, dataWidth, debugName,
        isSimple = true, canMux = withWE)
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

  val mvR = if (latency > 1)
    f"""for (i = 0; i < $latency-1; i = i+1) begin
       |    mem_pipe_reg[i+1] <= mem_pipe_reg[i];
       |  end
       |""".stripMargin else ""

  // We need keep hirarchy because in some rare circumstances, cross boundary optimization
  // prevents the memory from being inferred, and further, the memory is completely unrecongized,
  // mapped to a black box, and causes unrecoverable errors during logic synthesis... (Vivado 2022.1)

  val read = if (!withWE)
    "memreg <= mem[0][A_read];"
  else "memreg[(gi+1)*8-1:gi*8] <= mem[gi][A_read];"

  val write = if (!withWE)
    f"""
       |    if (WEB) begin
       |      mem[0][A_write] <= I;
       |    end
       |""".stripMargin
    else
    f"""    if (WEB[gi]) begin
       |      mem[gi][A_write] <= I[(gi+1)*8-1:gi*8];
       |    end
       |""".stripMargin

  val src =
    f"""
       |(* keep_hierarchy = "yes" *)
       |module $desiredName (
       |  input CE,
       |  input [${weWidth - 1}:0] WEB,
       |  input OEB,
       |  output [${dataWidth - 1}:0] O,
       |  input [${addrWidth - 1}:0] A_read,
       |  input [${dataWidth - 1}:0] I,
       |  input [${addrWidth - 1}:0] A_write,
       |  input CSB_read,
       |  input CSB_write);
       |
       |$memoryAnnotations
       |reg [${adjustedDW - 1}:0] mem [${weWidth - 1}:0][${nRows - 1}:0];        // Memory Declaration
       |reg [${dataWidth - 1}:0] memreg;
       |reg [${dataWidth - 1}:0] mem_pipe_reg [${latency - 1}:0];    // Pipelines for memory
       |
       |genvar  gi;
       |integer i;
       |generate
       |for(gi=0;gi<${weWidth};gi=gi+1)
       |begin:read_ops
       |always @ (posedge CE)
       |begin
       |  if(CSB_read) begin
       |    $read
       |  end
       |end
       |end
       |endgenerate
       |
       |generate
       |for(gi=0;gi<${weWidth};gi=gi+1)
       |begin:write_ops
       |always @ (posedge CE)
       |begin
       |  if(CSB_write) begin
       |    $write
       |  end
       |end
       |end
       |endgenerate
       |
       |// RAM output data goes through a pipeline.
       |always @ (posedge CE)
       |begin
       |  mem_pipe_reg[0] <= memreg ;
       |  $mvR
       |// O <= mem_pipe_reg[$latency-1];
       |
       |end
       |
       |assign O = mem_pipe_reg[$latency-1];
       |endmodule
       |
       |""".stripMargin

  val fw = new FileWriter(component.toString())
  fw.write(src)
  fw.close()

}
