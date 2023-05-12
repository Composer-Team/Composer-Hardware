package composer

import chipsalliance.rocketchip.config._
import chisel3.stage._
import composer.Systems.ComposerTop
import composer.tuning.ExportCSymbolPhase
import firrtl.{AnnotationSeq, CustomDefaultMemoryEmission, CustomDefaultRegisterEmission, MemoryNoInit}
import firrtl.options.PhaseManager.PhaseDependency
import firrtl.options._
import firrtl.stage.FirrtlCli
import freechips.rocketchip.stage.{ConfigsAnnotation, RocketChipCli, TopModuleAnnotation}
import os._

import java.io.FileWriter
import java.util.regex._

class ComposerChipStage extends Stage with Phase {
  override val shell = new Shell("composer-compile") with RocketChipCli with ChiselCli with FirrtlCli
  val targets: Seq[PhaseDependency] = Seq(
    Dependency[freechips.rocketchip.stage.phases.Checks],
    Dependency[freechips.rocketchip.stage.phases.TransformAnnotations],
    Dependency[freechips.rocketchip.stage.phases.PreElaboration],
    Dependency[chisel3.stage.phases.Checks],
    Dependency[chisel3.stage.phases.MaybeAspectPhase],
    Dependency[chisel3.stage.phases.Emitter],
    Dependency[chisel3.stage.phases.Convert],
    Dependency[ExportCSymbolPhase],
    Dependency[firrtl.stage.phases.Compiler])

  private val pm = new PhaseManager(targets)

  override def run(annotations: AnnotationSeq): AnnotationSeq = pm.transform(annotations)

}

object Generator extends StageMain(new ComposerChipStage)

object ComposerBuild {
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

  def composerGenDir: String = composerRoot() + "/Composer-Hardware/vsim/generated-src"

  def composerVsimDir: String = composerRoot() + "/Composer-Hardware/vsim/"

  def composerBin: String = composerRoot() + "/bin/"
}

object BuildArgs {
  private [composer] var args: Map[String, Int] = Map.empty
}

class ComposerBuild(config: Config) {
  final def main(args: Array[String]): Unit = {
    BuildArgs.args = Map.from(args.filter(_.substring(0, 2) == "-D").map { opt =>
      val pr = opt.substring(2).split("=")
      (pr(0), pr(1).toInt)
    })
    val gsrc_dir = Path(ComposerBuild.composerGenDir)
    os.makeDir.all(gsrc_dir)
    val full_name = config.getClass.getCanonicalName
    val short_name = full_name.split('.').last
    println("Elaborating config: " + short_name)
    val outputFile = gsrc_dir / "composer.v"
    val targetDir = gsrc_dir / "composer.fir"
    new ComposerChipStage().transform(AnnotationSeq(Seq(
      new TargetDirAnnotation(targetDir.toString()),
      new TopModuleAnnotation(Class.forName("composer.Systems.ComposerTop")),
      new ConfigsAnnotation(Seq(full_name)),
      new OutputAnnotationFileAnnotation(outputFile.toString()),
      CustomDefaultMemoryEmission(MemoryNoInit),
      CustomDefaultRegisterEmission(false, true)
    )))

    def appendSrcsTo(dir: Path, appendFile: Path): Unit = {
      if (os.exists(dir)) {
        os.walk(dir).filter { a =>
          val fname = a.toString
          val fend = fname.split('.').last
          fend == "v"
        }.foreach { path =>
//          val to = gsrc_dir / path.toString().split("/").takeRight(1)(0)
          os.write.append(appendFile, os.read(path))
        }
      }
    }
    os.move(targetDir / "ComposerTop.v", outputFile)
    appendSrcsTo(os.pwd / ".fpnew_cache", outputFile)
    appendSrcsTo(os.pwd / ".memories", outputFile)

    config(PostProcessorMacro)() // do post-processing per backend
  }
}