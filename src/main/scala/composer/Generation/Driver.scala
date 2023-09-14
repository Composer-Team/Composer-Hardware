package composer.Generation

import chipsalliance.rocketchip.config._
import chisel3.stage._
import composer.Generation
import composer.Generation.ComposerBuild._
import composer.Platforms.{BuildModeKey, PostProcessorMacro}
import firrtl._
import firrtl.annotations.{CircuitName, ModuleName}
import firrtl.options._
import firrtl.options.PhaseManager.PhaseDependency
import firrtl.stage.{CompilerAnnotation, FirrtlCli, RunFirrtlTransformAnnotation}
import firrtl.transforms.{Flatten, FlattenAnnotation}
import freechips.rocketchip.stage._
import os._

import java.nio.file
import java.time.LocalDateTime
import java.util.regex._
import scala.collection.SeqMap
import scala.util.matching.Regex

class ComposerChipStage extends Stage with Phase {
  override val shell = new Shell("composer-compile")
  val targets: Seq[PhaseDependency] = Seq(
    Dependency[freechips.rocketchip.stage.phases.Checks],
    Dependency[composer.Generation.Stage.PreElaborationPass],
    Dependency[chisel3.stage.phases.Checks],
    Dependency[chisel3.stage.phases.MaybeAspectPhase],
    Dependency[chisel3.stage.phases.Convert],   // convert chirrtl to firrtl
    Dependency[composer.Generation.Stage.ExportCSymbolPhase],
    Dependency[firrtl.stage.phases.Compiler]
  )

  private val pm = new PhaseManager(targets)

  override def run(annotations: AnnotationSeq): AnnotationSeq =
    pm.transform(annotations ++ Seq(PrintFullStackTraceAnnotation))
}

object ComposerBuild {
  private val errorNoCR =
    "Environment variables 'COMPOSER_ROOT' is not visible and no shell configuration file found.\n" +
      " Please define or configure IDE to see this enviornment variable\n"

  private var crossBoundaryDisableList: Seq[String] = Seq.empty
  def disableCrossBoundaryOptimizationForModule(moduleName: String): Unit = {
    crossBoundaryDisableList = crossBoundaryDisableList :+ moduleName
  }

  private def filterFIRRTL(path: Path): Unit = {
    os.proc(Seq("sed", "-i.backup", "-e", "1d", "-e", "/printf/d", "-e", "/assert/d", path.toString())).call()
  }

  private[composer] def addSource(): Unit = {}

  def composerRoot(): String = {
    if (System.getenv("COMPOSER_ROOT") != null)
      return System.getenv("COMPOSER_ROOT")
    val sh_full = System.getenv("SHELL")
    if (sh_full == null) throw new Exception(errorNoCR)
    val sh = sh_full.split("/").last
    val config = os.read(os.home / f".${sh}rc")
    val pattern = Pattern.compile("export COMPOSER_ROOT=([a-zA-Z/.]*)")
    val matcher = pattern.matcher(config)
    if (matcher.find()) matcher.group(0).split("=")(1).strip()
    else throw new Exception(errorNoCR)
  }

  private[composer] val composerGenDir: String =
    composerRoot() + "/Composer-Hardware/vsim/generated-src"

  private[composer] val composerVsimDir: String =
    composerRoot() + "/Composer-Hardware/vsim/"
  private[composer] val composerBin: String = composerRoot() + "/bin/"
  private[composer] var symbolicMemoryResources: Seq[Path] = Seq.empty
  private[composer] var sourceList: Seq[Path] = Seq.empty

  def addSource(p: Path): Unit = {
    sourceList = sourceList :+ p
  }

  private[composer] def addSymbolicResource(p: Path): Unit = {
    symbolicMemoryResources = symbolicMemoryResources :+ p
  }
}

object BuildArgs {
  private[composer] var args: Map[String, Int] = Map.empty
}

abstract class BuildMode
object BuildMode {
  case object Synthesis extends BuildMode

  case object Simulation extends BuildMode

  case class Tuning(hwBuildDir: String,
                    execCMAKEDir: String,
                    execName: String,
                    cmakeOpts: Seq[String] = Seq()) extends BuildMode

}

class ComposerBuild(config: => Config, buildMode: BuildMode = BuildMode.Synthesis) {
  final def main(args: Array[String]): Unit = {
    println("main args are : ")
    args.foreach(println(_))
    BuildArgs.args = Map.from(
      args.filter(str => str.length >= 2 && str.substring(0, 2) == "-D").map {
        opt =>
          val pr = opt.substring(2).split("=")
          println(pr(0) + " " + pr(1))
          (pr(0), pr(1).toInt)
      }
    )
    val gsrc_dir = if (args.filter(_.length > 9).exists(_.substring(0, 9) == "--target=")){
      val pth = args.filter(a => a.length > 9 && a.substring(0, 9) == "--target=")(0).substring(9)
      Path(pth)
    } else Path(ComposerBuild.composerGenDir)
    os.makeDir.all(gsrc_dir)
    val targetDir = gsrc_dir / "composer.build"
    val srcDir = gsrc_dir / "external_sources"
    os.makeDir(srcDir)
    new ComposerChipStage().transform(
      AnnotationSeq(
        Seq(
          new firrtl.options.TargetDirAnnotation(targetDir.toString()),
          // if you want to get annotation output for debugging, uncomment the following line
//          new OutputAnnotationFileAnnotation(targetDir.toString()),
          new TopModuleAnnotation(Class.forName("composer.Systems.ComposerTop")),
          Generation.Stage.ConfigsAnnotation(new Config(config.alterPartial {
            case BuildModeKey => buildMode
          })),
          CustomDefaultMemoryEmission(MemoryNoInit),
          CustomDefaultRegisterEmission(useInitAsPreset = false, disableRandomization = true),
          RunFirrtlTransformAnnotation(firrtl.LowFirrtlOptimizedEmitter),
          RunFirrtlTransformAnnotation(new SystemVerilogEmitter),
          RunFirrtlTransformAnnotation(new VerilogEmitter)
        )
      )
    )

    // For the ComposerTop.v, we copy in the sources directly for easing direct simulation
    // we also copy the source files to the composer.build directly should that be more
    // cromulent to the user
    sourceList.distinct foreach { src =>
      if (file.Files.isRegularFile(java.nio.file.Paths.get(src.toString()))) {
        os.write.append(targetDir / "ComposerTop.v", os.read(src))
        val fname = src.toString().split("/").last
        os.copy.over(src, srcDir / fname)
      } else {
        os.copy.over(src, gsrc_dir / src.baseName)
        os.copy.over(src, srcDir / src.baseName)
      }
    }

    ConstraintGeneration.slrMappings.foreach{ slrMapping =>
      crossBoundaryDisableList = crossBoundaryDisableList :+ slrMapping._1
    }
    if (crossBoundaryDisableList.nonEmpty) {
      System.err.println("Adding keep_hierarchy to SLR mappings for design with SLR distribution hint. This may take some time...")
      os.write.over(targetDir / "ComposerTop.v", crossBoundaryDisableList.distinct.fold(os.read(targetDir / "ComposerTop.v")) { case (f, name) =>
        val pattern = s"(\\w+ \\w*$name) ".r
        val replacement = {r: Regex.Match => " (* keep_hierarchy = \"yes\" *) " + r.group(1) + " "}
        pattern.replaceAllIn(f, replacement)
      })
      System.err.println("Done adding keep_hierarchy to SLR mappings.")
    }
    os.walk(targetDir).foreach { file =>
      val extension = file.ext
      os.copy(file, gsrc_dir / ("composer." + extension), replaceExisting = true)
    }
    filterFIRRTL(gsrc_dir / "composer.fir")

    config(PostProcessorMacro)(config) // do post-processing per backend

    buildMode match {
      case bm: BuildMode.Tuning if !args.contains("--notune") =>
        val opts = if (bm.cmakeOpts.length == 1) bm.cmakeOpts(0) else {
          bm.cmakeOpts.reduce(_ + "." + _)
        }
        os.proc(Seq("python3",
          System.getenv("COMPOSER_ROOT") + "/Composer-Hardware/scripts/tune.py",
          this.getClass.getCanonicalName,
          bm.hwBuildDir,
          bm.execCMAKEDir + "." + bm.execName + "." + opts)).call(
          stdout=os.Inherit
        )
      case _ =>
    }
  }
}
