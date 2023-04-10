package composer

import chipsalliance.rocketchip.config.Config
import chisel3.stage.PrintFullStackTraceAnnotation
import freechips.rocketchip.system._
import firrtl.stage.FirrtlMain
import os.Path

object TestDriver {
  def buildConfig(config: Config): Unit = {
    val gsrc_dir = Path(System.getenv("COMPOSER_ROOT")) / "Composer-Hardware" / "vsim" / "generated-src"
    println("Writing to " + gsrc_dir.toString())
    os.makeDir.all(gsrc_dir)
    val full_name = config.getClass.getCanonicalName
    val short_name = full_name.split('.').last
    println(full_name + " " + short_name)
    new RocketChipStage().execute(
      args = Array("-td", gsrc_dir.toString(),
        "-T", "composer.ComposerTop",
        "-C", full_name,
        "--emission-options=disableMemRandomization,disableRegisterRandomization"),
      annotations = Seq())

    FirrtlMain.stage.execute(
      args = Array("-i", gsrc_dir.toString() + "/composer." + short_name + ".fir",
        "-o", gsrc_dir.toString() + "/composer.v",
        "-X", "verilog",
        "--emission-options=disableMemRandomization,disableRegisterRandomization",
        "-fct", "firrtl.passes.InlineInstances",
        "--infer-rw", "ComposerTop",
//        "--repl-seq-mem", s"-c:ComposerTop:-o:$hw_idr/ComposerTop.conf"
        ),
      annotations = Seq(PrintFullStackTraceAnnotation))

    // make sure that any generated FPUs get copied to the composer hardware generated src directory

    val fpu_cache = os.pwd / ".fpnew_cache"
    if (os.exists(fpu_cache)) {
      os.walk(fpu_cache).filter(_.toString().contains(".v")).foreach { path =>
        val to = gsrc_dir / path.toString().split("/").takeRight(1)(0)
        if (!os.exists(to))
          os.copy(path, to)
      }
    }
  }
}
