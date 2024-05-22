package composer.Platforms.FPGA.Xilinx

package object Templates {
  private[Templates] val memoryRoot = os.pwd / ".memories"
  private[Templates] def writeF(port: String, withWriteEnable: Boolean, weWidth: Int, dataWidth: Int): String = {
    if (withWriteEnable)
      f"""
         |generate
         |for(gi=0;gi<$weWidth;gi=gi+1)
         |begin:port$port
         |always @ (posedge CE)
         |begin
         |  if(CSB$port) begin
         |    if (WEB$port[gi]) begin
         |      mem[gi][A$port] <= I$port[(gi+1)*8-g1:i*8];
         |    end else begin
         |      memreg$port[(gi+1)*8-1:gi*8] <= mem[gi][A$port];
         |    end
         |  end
         |end
         |end
         |endgenerate
         |
         |""".stripMargin
    else
      f"""
         |always @ (posedge CE)
         |begin
         |  if(CSB$port) begin
         |    if (WEB$port) begin
         |      mem[0][A$port] <= I$port[${dataWidth-1}:0];
         |    end else begin
         |      memreg$port <= mem[0][A$port];
         |    end
         |  end
         |end
         |
         |""".stripMargin
  }

}
