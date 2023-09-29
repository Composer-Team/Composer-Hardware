package composer.Platforms.FPGA.Xilinx.Templates

import chisel3._
import chisel3.util.HasBlackBoxInline
import composer.common.CLog2Up
class BRAMDualClocked(minDepth: Int,
                      minWidth: Int) extends BlackBox {
  val validWidths = Seq(4, 9, 18, 36, 72)
  val widthToValidDepths = Map.from(Seq(
    (4, Seq(1<<12, 1<<13)),
    (9, Seq(1<<11, 1<<12)),
    (18, Seq(1<<10, 1<<11)),
    (36, Seq(512, 1<<10)),
    (72, Seq(512))))
  val realWidth = validWidths.filter(_ >= minWidth).minOption.getOrElse {
    throw new Exception(f"Requested width for FIFO ($minWidth) - unsatisfiable. Must be <= 72")
  }
  val realDepth = widthToValidDepths(realWidth).filter(_ >= minDepth).minOption.getOrElse {
    throw new Exception(f"Requested depth for FIFO ($minDepth) - unsatisfiable." +
    f"Compatiable depths for width ($minWidth) are (${widthToValidDepths(realWidth)}")
  }
  val addrWidth = CLog2Up(realDepth)
  val moduleName = f"BRAM_DC_D${realDepth}_W$realWidth"
  val sourceName = f"$moduleName.v"
  // try to find a setup that satisfies the depth and width
  val io = IO(new Bundle {
    val clka, clkb, ena, enb, wea = Input(UInt(1.W))
    val addra, addrb = Input(UInt(CLog2Up(realDepth).W))
    val dia = Input(UInt(realWidth.W))
    val dob = Output(UInt(realWidth.W))
  })

  val src =
    f"""
       |module $moduleName (clka,clkb,ena,enb,wea,addra,addrb,dia,dob);
       |
       |input clka,clkb,ena,enb,wea;
       |input [${addrWidth-1}:0] addra,addrb;
       |input [${realWidth-1}:0] dia;
       |output [${realWidth-1}:0] dob;
       |reg [${realWidth-1}:0] ram [${realDepth-1}:0];
       |reg [${realWidth-1}:0] dob_reg;
       |assign dob = dob_reg;
       |always @(posedge clka)
       |begin
       |  if (ena)
       |    begin
       |      if (wea)
       |        ram[addra] <= dia;
       |    end
       |end
       |
       |always @(posedge clkb)
       |begin
       |  if (enb)
       |    begin
       |      dob_reg <= ram[addrb];
       |    end
       |end
       |
       |endmodule
       |""".stripMargin

  if (!os.exists(memoryRoot / sourceName)) {
    os.write(memoryRoot / sourceName, src)
  }


}
