package composer

import chipsalliance.rocketchip.config.Config
import chisel3.stage.PrintFullStackTraceAnnotation
import freechips.rocketchip.system._
import firrtl.stage.FirrtlMain

object TestDriver {
  def buildConfig(config: Config): Unit = {
    val c_dir = System.getProperty("user.dir")
    val hw_idr = c_dir + "/vsim/generated-src/"
    val full_name = config.getClass.getCanonicalName
    val short_name = full_name.split('.').last
    println(full_name + " " + short_name)
    new RocketChipStage().execute(
      args = Array("-td", hw_idr,
        "-T", "composer.ComposerTop",
        "-C", full_name,
        "--emission-options=disableMemRandomization,disableRegisterRandomization"),
      annotations = Seq())

    FirrtlMain.stage.execute(
      args = Array("-i", hw_idr + "/composer." + short_name + ".fir",
        "-o", hw_idr + "composer.v",
        "-X", "verilog",
        "--emission-options=disableMemRandomization,disableRegisterRandomization",
        "-fct", "firrtl.passes.InlineInstances",
        "--infer-rw", "ComposerTop",
//        "--repl-seq-mem", s"-c:ComposerTop:-o:$hw_idr/ComposerTop.conf"
        ),
      annotations = Seq(PrintFullStackTraceAnnotation))
  }
}
