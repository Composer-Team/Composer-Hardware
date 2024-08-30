package beethoven.Generation.vcs

import beethoven.Generation.BeethovenBuild
import beethoven.platform
import chipsalliance.rocketchip.config.Parameters

/**
 * Thank you to ChiselTest for inspiring this code.
 */

object HarnessGenerator {
  def generateHarness(): Unit = {
    val r = os.read(BeethovenBuild.targetDir / "BeethovenTop.v").split("\n")
    def sanitize(q: String): Seq[String] = q.trim.split(" +").map(a => a.replace(",", "").trim)
    def is_reserved(q: Seq[String]): Boolean = {
      val r = q.last
      r == "clock" || r == "reset"
    }

    val inputs = r.filter(_.contains("input ")).map(sanitize).filter(!is_reserved(_))
    val outputs = r.filter(_.contains("output ")).map(sanitize).filter(!is_reserved(_))
    val w =
      """
        |module BeethovenTopVCSHarness;
        |""".stripMargin +
        inputs.map { i =>
          val widthMO = if (i.length == 2) 0 else {
            val ss = i(1)
//            println(i)
            ss.substring(1, ss.indexOf(":"))
          }
          f"  reg [$widthMO:0] ${i.last};\n"
        }.mkString("") + "\n" +
        outputs.map { i =>
          val widthMO = if (i.length == 2) 0 else {
            val ss = i(1)
            ss.substring(1, ss.indexOf(":"))
          }
          f"  wire [$widthMO:0] ${i.last};\n"
        }.mkString("") +
        f"""
           |  reg clock = 0;
           |  reg reset = 1;
           |  BeethovenTop top(
           |    .clock(clock),
           |    .reset(reset),
           |""".stripMargin +
        (inputs ++ outputs).map { q =>f"    .${q.last}(${q.last})"}.mkString(",\n") +
        f"""
           |  );
           |
           |  reg dump_reg = 1'b0;
           |  initial begin
           |    #`CLOCK_PERIOD
           |    forever begin
           |      #`CLOCK_PERIOD clock = ~clock;
           |    end
           |  end
           |
           |  initial begin
           |    $$vcdplusfile("BeethovenTrace.vpd");
           |    $$dumpvars(0, top);
           |    $$vcdpluson;
           |    $$init_input_signals(clock, reset, ${inputs.map(_.last).mkString(", ")});
           |    $$init_output_signals(${outputs.map(_.last).mkString(", ")});
           |    $$init_structures;
           |  end
           |
           |  always @(negedge clock) begin
           |    if (!dump_reg) begin
           |      dump_reg = 1'b1;
           |      $$dumpon;
           |    end
           |    $$tick();
           |    $$dumpflush;
           |  end
           |
           |  initial begin
           |  #100 reset = 0;
           |  end
           |endmodule
           |
           |""".stripMargin

    os.write(BeethovenBuild.targetDir / "BeethovenTopVCSHarness.v", w)
  }
}
