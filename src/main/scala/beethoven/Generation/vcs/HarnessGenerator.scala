package beethoven.Generation.vcs

import beethoven.{BeethovenBuild, platform}
import chipsalliance.rocketchip.config.Parameters

/**
 * Thank you to ChiselTest for inspiring this code.
 */

object HarnessGenerator {
  def generateHarness()(implicit p: Parameters): Unit = {
    val r = os.read(BeethovenBuild.hw_build_dir / "BeethovenTop.v").split("\n")

    def sanitize(q: String): Seq[String] = q.trim.split(" +").map(a => a.replace(",", "").trim)

    def is_reserved(q: Seq[String]): Boolean = {
      val r = q.last
      r == "clock" || r == "reset" || r == "RESETn"
    }

    val inputs = r.filter(_.contains("input ")).map(sanitize).filter(!is_reserved(_))
    val outputs = r.filter(_.contains("output ")).map(sanitize).filter(!is_reserved(_))
    val is_reset_active_high = platform.isActiveHighReset
    val reset_active = if (is_reset_active_high) 1 else 0
    val reset_inactive = reset_active ^ 1
    val reset_name = if (is_reset_active_high) "reset" else "RESETn"
    val w =
      """`timescale 1ns/1ps
        |
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
           |  reg $reset_name = $reset_active;
           |  BeethovenTop top(
           |    .clock(clock),
           |    .$reset_name($reset_name),
           |""".stripMargin +
        (inputs ++ outputs).map { q => f"    .${q.last}(${q.last})" }.mkString(",\n") +
        f"""
           |  );
           |
           |  reg dump_reg = 1'b0;
           |  initial begin:a1
           |    #`CLOCK_PERIOD
           |    forever begin
           |      #`CLOCK_PERIOD clock = ~clock;
           |      ;
           |    end
           |  end
           |
           |  initial begin:a2
           |`ifndef ICARUS
           |    $$vcdplusfile("BeethovenTrace.vpd");
           |`endif
           |    $$dumpvars(0, top);
           |`ifndef ICARUS
           |    $$vcdpluson;
           |`endif
           |    $$init_input_signals(clock, $reset_name, ${inputs.map(_.last).mkString(", ")});
           |    $$init_output_signals(${outputs.map(_.last).mkString(", ")});
           |    $$init_structures;
           |  end
           |
           |  always @(negedge clock) begin:a3
           |    if (!dump_reg) begin
           |      dump_reg = 1'b1;
           |      $$dumpon;
           |    end
           |    $$tick();
           |    $$dumpflush;
           |  end
           |
           |  initial begin:a4
           |  integer i;
           |  for (i=0;i<100;i=i+1) begin
           |    # `CLOCK_PERIOD
           |    ;
           |  end
           |  $reset_name = $reset_inactive;
           |  end
           |endmodule
           |
           |""".stripMargin

    os.write(BeethovenBuild.hw_build_dir / "BeethovenTopVCSHarness.v", w)
  }
}
