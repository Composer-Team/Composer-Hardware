package composer.MemoryStreams

import chisel3._
import chisel3.util._

import java.io.FileWriter

/**
 * Experimental: Thinking this may be easier for Vivado to recognize as URAM?
 * Currently encountering problems having Vivado infer the right number of 'pipeline' stages
 *
 * 1 WRITE PORT
 * 1 READ PORT
 */
//noinspection ScalaUnusedSymbol
class CMemory(latency: Int, dataWidth: Int, nRows: Int, forceURAM: Boolean = false) extends BlackBox {
  require(latency >= 1, "Scratchpad latency must be at least 1. Found " + latency)
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
    val w_mem_en = Input(Bool())
    val w_din = Input(UInt(dataWidth.W))
    val w_addr = Input(UInt(addrWidth.W))
  })

  override val desiredName = f"CMemoryL${latency}DW${dataWidth}R$nRows"

  private val memoryRoot = os.pwd / ".memories"
  if (!os.exists(memoryRoot)) os.makeDir(memoryRoot)

  private val component = memoryRoot / f"$desiredName.v"
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
       |${if (forceURAM) "(* ram_style = \"ultra\" *)" else ""}
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
       |    mem[w_addr] <= w_din;
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
