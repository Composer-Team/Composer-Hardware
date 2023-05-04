package composer.MemoryStreams

import chisel3._
import chisel3.util._

import java.io.FileWriter

/**
 * Experimental: Thinking this may be easier for Vivado to recognize as URAM?
 * Currently encountering problems having Vivado infer the right number of 'pipeline' stages
 */
//noinspection ScalaUnusedSymbol
class CMemory(latency: Int, dataWidth: Int, nRows: Int, ports: Int) extends BlackBox {
  require(latency >= 1, "Scratchpad latency must be at least 1")
  private val addrWidth = log2Up(nRows)
  val io = IO(new Bundle() {
    val clk = Input(Clock())
    val rst = Input(Reset())

    val mem_en = Input(Vec(ports, Bool()))
    val we = Input(Vec(ports, Bool()))
    val regce = Input(Vec(ports, Bool()))
    val addr = Input(Vec(ports, UInt(addrWidth.W)))
    val din = Input(Vec(ports, UInt(dataWidth.W)))

    val dout = Output(Vec(ports, UInt(dataWidth.W)))
  })

  override val desiredName = f"CMemoryL${latency}DW${dataWidth}R$nRows"

  private val memoryRoot = os.pwd / ".memories"
  if (!os.exists(memoryRoot)) os.makeDir(memoryRoot)

  private val component = memoryRoot / f"$desiredName.sv"
  val src =
    f"""
       |
       |module $desiredName (
       |  """.stripMargin + ((0 until ports) map { p =>
      f"""
         |  input [${dataWidth - 1}:0] din_$p,
         |  input [${addrWidth - 1}:0] addr_$p,
         |  input mem_en_$p,
         |  input we_$p,
         |  input regce_$p,
         |  output [${dataWidth - 1}:0] dout_$p,
         |""".stripMargin
    } reduce (_ + _)) + "\n  input clk,\n  input rst);\n" +
      f"""
         |wire [${dataWidth - 1}:0] dout_wire [${ports - 1}:0];
         |wire [${dataWidth - 1}:0] din_wire [${ports - 1}:0];
         |wire [${addrWidth - 1}:0] addr_wire [${ports - 1}:0];
         |wire [${ports - 1}:0] mem_en_wire;
         |wire [${ports - 1}:0] we_wire;
         |wire [${ports - 1}:0] regce_wire;
         |
         |${desiredName}_sub mem(
         |  .clk(clk),
         |  .rst(rst),
         |  .mem_en(mem_en_wire),
         |  .we(we_wire),
         |  .regce(regce_wire),
         |  .din(din_wire),
         |  .addr(addr_wire),
         |  .dout(dout_wire));
         |  """.stripMargin + {
      ((0 until ports) map { p =>
        f"""
           |assign dout_$p = dout_wire[$p];
           |assign din_wire[$p] = din_$p;
           |assign addr_wire[$p] = addr_$p;
           |""".stripMargin
      } reduce (_ + _)) +
        ("assign mem_en_wire = {" + (1 until ports).map(p => f"mem_en_$p, ").reverse.fold("")(_ + _) + f"mem_en_0};\n") +
        ("assign we_wire = {" + (1 until ports).map(p => f"we_$p, ").reverse.fold("")(_ + _) + f"we_0};\n") +
        ("assign regce_wire = {" + (1 until ports).map(p => f"regce_$p, ").reverse.fold("")(_ + _) + f"regce_0};\n")
    } +
      f"""
         |
         |endmodule
         |
         |//  Xilinx UltraRAM Single Port No Change Mode.  This code implements
         |//  a parameterizable UltraRAM block in No Change mode. The behavior of this RAM is
         |//  when data is written, the output of RAM is unchanged. Only when write is
         |//  inactive data corresponding to the address is presented on the output port.
         |//
         |module ${desiredName}_sub (
         |    input clk,                    // Clock
         |    input rst,                    // Reset
         |    input [${ports - 1}:0] we,                     // Write Enable
         |    input [${ports - 1}:0] regce,                  // Output Register Enable
         |    input [${ports - 1}:0] mem_en,                 // Memory Enable
         |    input [${dataWidth - 1}:0] din [${ports - 1}:0],       // Data Input
         |    input [${addrWidth - 1}:0] addr [${ports - 1}:0],      // Address Input
         |    output reg [${dataWidth - 1}:0] dout [${ports - 1}:0]  // Data Output
         |   );
         |
         |reg [${dataWidth - 1}:0] mem [${nRows - 1}:0];        // Memory Declaration
         |reg [${dataWidth - 1}:0] memreg [${ports - 1}:0];
         |reg [${dataWidth - 1}:0] mem_pipe_reg [${latency - 1}:0][${ports - 1}:0];    // Pipelines for memory
         |reg [$latency:0] mem_en_pipe_reg[${ports - 1}:0];                // Pipelines for memory enable
         |
         |integer          i, p;""".stripMargin +
      ((0 until ports) map (p => f"""
         |always @ (posedge clk)
         |begin
         |  if(mem_en[$p]) begin
         |    if(we[$p]) begin
         |      mem[addr[$p]] <= din[$p];
         |    end else begin
         |      memreg[$p] <= mem[addr[$p]];
         |    end
         |  end
         |end
         |""".stripMargin) reduce (_ + _)) +
      f"""
         |// The enable of the RAM goes through a pipeline to produce a
         |// series of pipelined enable signals required to control the data
         |// pipeline.
         |always @ (posedge clk)
         |begin
         |  for (p = 0; p < $ports; p = p+1) begin
         |    mem_en_pipe_reg[p][0] <= mem_en[p];
         |    for (i=0; i<$latency; i=i+1) begin
         |      mem_en_pipe_reg[p][i+1] <= mem_en_pipe_reg[p][i];
         |    end
         |  end
         |end
         |
         |// RAM output data goes through a pipeline.
         |always @ (posedge clk)
         |begin
         |for (i = 0; i < $ports; i = i+1) begin
         |  if (mem_en_pipe_reg[i][0]) begin
         |    mem_pipe_reg[0][i] <= memreg[i];
         |  end
         |end
         |end
         |
         |always @ (posedge clk)
         |begin
         |for (p = 0; p < $ports; p = p+1) begin
         |  for (i = 0; i < $latency-1; i = i+1) begin
         |    if (mem_en_pipe_reg[p][i+1]) begin
         |      mem_pipe_reg[i+1][p] <= mem_pipe_reg[i][p];
         |    end
         |  end
         |end
         |end
         |
         |// Final output register gives user the option to add a reset and
         |// an additional enable signal just for the data ouptut
         |always @ (posedge clk)
         |begin
         |  for (p = 0; p < $ports; p = p+1) begin
         |    if (mem_en_pipe_reg[p][$latency] && regce[p]) begin
         |      dout[p] <= mem_pipe_reg[$latency-1][p];
         |    end
         |  end
         |end
         |endmodule
         |
         |""".stripMargin

  val fw = new FileWriter(component.toString())
  fw.write(src)
  fw.close()
}
