package composer.Generation

import chipsalliance.rocketchip.config._
import chisel3.stage._
import composer.Generation.Annotators.AnnotateXilinxInterface.XilinxInterface
import composer.Generation
import composer.Generation.Annotators.{CrossBoundaryDisable, WalkPath}
import composer.Generation.ComposerBuild._
import composer.Platforms.FPGA.Xilinx.AWSF1Platform
import composer.Platforms._
import composer.Protocol.FrontBus.FrontBusProtocol
import firrtl._
import firrtl.options._
import firrtl.options.PhaseManager.PhaseDependency
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.NoDCEAnnotation
import freechips.rocketchip.stage._
import freechips.rocketchip.subsystem.ExtMem
import os._

import java.util.regex._

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
    pm.transform(annotations ++ Seq(PrintFullStackTraceAnnotation, NoDCEAnnotation))
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

  private[composer] var partitionModules: Seq[String] = Seq("ComposerTop")

  def requestModulePartition(moduleName: String): Unit =
    partitionModules = partitionModules :+ moduleName

  def getPartitions: Seq[String] = partitionModules

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

  val composerGenDir: String =
    composerRoot() + "/Composer-Hardware/vsim/generated-src"

  val composerVsimDir: String =
    composerRoot() + "/Composer-Hardware/vsim/"
  val composerBin: String = composerRoot() + "/bin/"
  val gsrc_dir = Path(ComposerBuild.composerGenDir)
  val targetDir = gsrc_dir / "composer.build"

  var symbolicMemoryResources: Seq[Path] = Seq.empty
  var sourceList: Seq[Path] = Seq.empty

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
    os.remove.all(targetDir)
    os.makeDir.all(targetDir)
    val configWithBuildMode = new Config(config.alterPartial {
      case BuildModeKey => buildMode
    })
    new ComposerChipStage().transform(
      AnnotationSeq(
        Seq(
          // if you want to get annotation output for debugging, uncomment the following line
          new EmitAllModulesAnnotation(classOf[VerilogEmitter]),
          new TargetDirAnnotation(targetDir.toString()),
          //          new OutputAnnotationFileAnnotation(targetDir.toString()),
          new TopModuleAnnotation(Class.forName("composer.Systems.ComposerTop")),
          Generation.Stage.ConfigsAnnotation(configWithBuildMode),
          CustomDefaultMemoryEmission(MemoryNoInit),
          CustomDefaultRegisterEmission(useInitAsPreset = false, disableRandomization = true),
          RunFirrtlTransformAnnotation(new VerilogEmitter),
          NoDCEAnnotation
        )
      )
    )

    val chiselGeneratedSrcs = WalkPath(targetDir)

    // --------------- Verilog Annotators ---------------
    //    KeepHierarchy(targetDir / "ComposerTop.v")
//    partitionModules foreach println
    val movedSrcs = composer.Generation.Annotators.UniqueMv(sourceList, targetDir)

    ConstraintGeneration.slrMappings.foreach { slrMapping =>
      crossBoundaryDisableList = crossBoundaryDisableList :+ slrMapping._1
    }
    if (crossBoundaryDisableList.nonEmpty && !buildMode.isInstanceOf[BuildMode.Training.type]) {
      CrossBoundaryDisable(crossBoundaryDisableList, targetDir)
    }
    if (configWithBuildMode(PlatformKey).platformType == PlatformType.FPGA &&
      !configWithBuildMode(PlatformKey).isInstanceOf[AWSF1Platform]) {
      val tc_axi = (0 until config(PlatformKey).extMem.nMemoryChannels) map { idx =>
        composer.Generation.Annotators.AnnotateXilinxInterface(
          f"M0${idx}_AXI", (targetDir / "ComposerTop.v").toString(), XilinxInterface.AXI4)
        Some(f"M0${idx}_AXI")
      }
      // implies is AXI
      val tc_front = {
        composer.Generation.Annotators.AnnotateXilinxInterface(
          "S00_AXI", (targetDir / "ComposerTop.v").toString(), XilinxInterface.AXI4)
        Some("S00_AXI")
      }

      val tcs = ((Seq(tc_front) ++ tc_axi) filter (_.isDefined) map (_.get)).mkString(":")
      Annotators.AnnotateTopClock(
        f"\\(\\* X_INTERFACE_PARAMETER = \"ASSOCIATED_BUSIF $tcs \" \\*\\)",
        targetDir / "ComposerTop.v"
      )
    }

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
      case BuildMode.Synthesis =>
        config(PlatformKey) match {
          case pwpp: Platform with HasPostProccessorScript =>
            pwpp.postProcessorMacro(configWithBuildMode, movedSrcs ++ chiselGeneratedSrcs)
          case _ => ;
        }
      case _ => ;
    }
  }
}
