package composer

import chipsalliance.rocketchip.config._
import chisel3.stage.PrintFullStackTraceAnnotation
import freechips.rocketchip.system._
import firrtl.stage.FirrtlMain
import os._
import java.util.regex._

object TestDriver {
  private val errorNoCR = "Environment variables 'COMPOSER_ROOT' is not visible and no shell configuration file found.\n" +
    " Please define or configure IDE to see this enviornment variable\n"
  def composerRoot(): String = {
    if (System.getenv("COMPOSER_ROOT") != null) return System.getenv("COMPOSER_ROOT")
    val sh_full = System.getenv("SHELL")
    if (sh_full == null) throw new Exception(errorNoCR)
    val sh = sh_full.split("/").last
    val config = os.read(os.home / f".${sh}rc")
    val pattern = Pattern.compile("export COMPOSER_ROOT=([a-zA-Z/.]*)")
    val matcher = pattern.matcher(config)
    if (matcher.find()) matcher.group(0).split("=")(1).strip()
    else throw new Exception(errorNoCR)
  }
  def buildConfig(config: Config): Unit = {
    val croot = composerRoot()
    val gsrc_dir = Path(croot) / "Composer-Hardware" / "vsim" / "generated-src"
    os.makeDir.all(gsrc_dir)
    val full_name = config.getClass.getCanonicalName
    val short_name = full_name.split('.').last
    println("Elaborating config: " + short_name)
    new RocketChipStage().execute(
      args = Array("-td", gsrc_dir.toString(),
        "-T", "composer.Systems.ComposerTop",
        "-C", full_name,
        "--emission-options=disableMemRandomization,disableRegisterRandomization"),
      annotations = Seq())

    FirrtlMain.stage.execute(
      args = Array("-i", gsrc_dir.toString() + "/composer.Systems." + short_name + ".fir",
        "-o", gsrc_dir.toString() + "/composer.v",
        "-X", "verilog",
        "--emission-options=disableMemRandomization,disableRegisterRandomization",
        "-fct", "firrtl.passes.InlineInstances",
        "--infer-rw", "ComposerTop",
//        "--repl-seq-mem", s"-c:ComposerTop:-o:$hw_idr/ComposerTop.conf"
        ),
      annotations = Seq(PrintFullStackTraceAnnotation))

    // make sure that any generated FPUs get copied to the composer hardware generated src directory

    def copy_dir(dirName: String): Unit = {
      val dir = os.pwd / dirName
      if (os.exists(dir)) {
        os.walk(dir).filter{a =>
          val fname = a.toString
          val fend = fname.split('.').last
          fend == "v" || fend == "sv"}.foreach { path =>
          val to = gsrc_dir / path.toString().split("/").takeRight(1)(0)
          os.copy(path, to, replaceExisting = true)
        }
      }
    }
    copy_dir(".fpnew_cache")
    copy_dir(".memories")
  }
}
