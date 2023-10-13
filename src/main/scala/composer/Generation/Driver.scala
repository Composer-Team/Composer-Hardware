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
    Dependency[chisel3.stage.phases.Convert], // convert chirrtl to firrtl
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

  case object Training extends BuildMode
}

class ComposerBuild(config: => Config, buildMode: BuildMode = BuildMode.Synthesis) {

  private def get_os(): String = {
    os.proc("uname").call().out.trim
  }

  private def get_sed_inline_opt(): Seq[String] = {
    get_os() match {
      case "Darwin" => Seq("-I", "")
      case "Linux" => Seq("-i")
      case _ => throw new Exception("Couldn't figure out OS for " + get_os())
    }
  }

  def addKeepHierarchyAnnotationsForCores(fname: Path): Unit = {
    val sedcmd = Seq("sed") ++ get_sed_inline_opt() ++ Seq("-E",
      "s/(module AccelCoreWrapper)/(* keep_hierarchy = \"yes\" *)\\n\\1/",
      fname.toString())
    os.proc(Seq("sed") ++ get_sed_inline_opt() ++ Seq("-E",
      "s/(module AccelCoreWrapper)/(* keep_hierarchy = \"yes\" *)\\n\\1/",
      fname.toString())).call()
  }

  final def main(args: Array[String]): Unit = {
//    args.foreach(println(_))
    BuildArgs.args = Map.from(
      args.filter(str => str.length >= 2 && str.substring(0, 2) == "-D").map {
        opt =>
          val pr = opt.substring(2).split("=")
//          println(pr(0) + " " + pr(1))
          (pr(0), pr(1).toInt)
      }
    )
    val gsrc_dir = if (args.filter(_.length > 9).exists(_.substring(0, 9) == "--target=")) {
      val pth = args.filter(a => a.length > 9 && a.substring(0, 9) == "--target=")(0).substring(9)
      Path(pth)
    } else Path(ComposerBuild.composerGenDir)
    os.walk(gsrc_dir).foreach { file =>
      os.remove.all(file)
    }
    os.makeDir.all(gsrc_dir)
    val targetDir = gsrc_dir / "composer.build"
    val configWithBuildMode = new Config(config.alterPartial {
      case BuildModeKey => buildMode
    })
    new ComposerChipStage().transform(
      AnnotationSeq(
        Seq(
          new firrtl.options.TargetDirAnnotation(targetDir.toString()),
          // if you want to get annotation output for debugging, uncomment the following line
          new OutputAnnotationFileAnnotation(targetDir.toString()),
          new TopModuleAnnotation(Class.forName("composer.Systems.ComposerTop")),
          Generation.Stage.ConfigsAnnotation(configWithBuildMode),
          CustomDefaultMemoryEmission(MemoryNoInit),
          CustomDefaultRegisterEmission(useInitAsPreset = false, disableRandomization = true),
          RunFirrtlTransformAnnotation(new VerilogEmitter),
        )
      )
    )

    addKeepHierarchyAnnotationsForCores(targetDir / "ComposerTop.v")

    // first, get module names in targetDir / "ComposerTop.v"
    val pattern = Pattern.compile("module (.*) *\\(")
    val matcher = pattern.matcher(os.read(targetDir / "ComposerTop.v"))
    var existingModules = Seq.empty[String]
    while (matcher.find()) {
      existingModules = existingModules :+ matcher.group(1)
    }

    // add escapes for $ in module names
    // add additional escapes for \
    def addEscapes(s: String): String = {
      val back = "\\\\"
      val doubleBack = "\\\\\\\\"
      s.replaceAll(back, doubleBack).replaceAll("\\$", "\\\\\\$")
    }

    // then, for each source in the source list, copy in all modules that aren't already defined
    sourceList.distinct foreach { src =>
      val srcFileName = src.segments.toSeq.last
      if (file.Files.isRegularFile(java.nio.file.Paths.get(src.toString()))) {
        if (srcFileName.endsWith(".v")) {
          val matcher = pattern.matcher(os.read(src))
          var srcModules = Seq.empty[String]
          while (matcher.find()) {
            srcModules = srcModules :+ matcher.group(1)
          }
          // get a list of repeated modules
          val repeats = srcModules.filter(existingModules.contains(_)).distinct
          // for each create a sed command to remove the module
          //        println("In " + src.toString() + " repeats are " + repeats)
          if (repeats.nonEmpty) {
            val sedCmds = repeats.map { mod: String => s"/module ${addEscapes(mod)}/,/endmodule/d" }
            // then, remove the repeated modules from the source
            // join sedCMds with ;
            val sedCmd = Seq("sed", "-E", sedCmds.mkString(";"), src.toString)
            //          println("sedCmd is " + sedCmd)

            os.proc(sedCmd).call(stdout = os.pwd / "tmp.v")
            os.write.append(targetDir / "ComposerTop.v", os.read(os.pwd / "tmp.v"))
            os.remove(os.pwd / "tmp.v")
          } else {
            os.write.append(targetDir / "ComposerTop.v", os.read(src))
          }
          existingModules = existingModules ++ srcModules
        } else {
          // not a verilog source, just copy it to the source directory
          os.copy.over(src, gsrc_dir / srcFileName)
        }
      } else {
        os.copy.over(src, gsrc_dir / srcFileName)
      }
    }

    ConstraintGeneration.slrMappings.foreach { slrMapping =>
      crossBoundaryDisableList = crossBoundaryDisableList :+ slrMapping._1
    }
    if (crossBoundaryDisableList.nonEmpty && !buildMode.isInstanceOf[BuildMode.Training.type]) {
      // create sed command to add keep_hierarchy to SLR mappings
      val sedcmds = crossBoundaryDisableList.distinct.map { name =>
        s"s/(module ${addEscapes(name)}) /(* keep_hierarchy = \"yes\" *) \\1 /"
      }
//      sedcmds foreach {println(_)}
      val sedcmd = Seq("sed") ++ get_sed_inline_opt() ++ Seq("-E", sedcmds.mkString(";"), (targetDir / "ComposerTop.v").toString())
//      os.proc(sedcmd).call()
      System.err.println("Called for cross boundary disables but currently broken...")
    }
    os.walk(targetDir).foreach { file =>
      val extension = file.ext
      os.copy(file, gsrc_dir / ("composer." + extension), replaceExisting = true)
    }
    //    filterFIRRTL(gsrc_dir / "composer.fir")

    config(PostProcessorMacro)(configWithBuildMode) // do post-processing per backend

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
          stdout = os.Inherit
        )
      case _ =>
    }
  }
}
