package beethoven.Generation

import chipsalliance.rocketchip.config._
import chisel3.stage._
import beethoven.Floorplanning.ConstraintGeneration
import beethoven.Generation.Annotators.AnnotateXilinxInterface.XilinxInterface
import beethoven.Generation
import beethoven.Generation.Annotators.{CrossBoundaryDisable, WalkPath}
import beethoven.Generation.BeethovenBuild._
import beethoven.Platforms.FPGA.Xilinx.AWSF1Platform
import beethoven.Platforms._
import beethoven.Protocol.FrontBus.FrontBusProtocol
import firrtl._
import firrtl.options._
import firrtl.options.PhaseManager.PhaseDependency
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.NoDCEAnnotation
import freechips.rocketchip.stage._
import freechips.rocketchip.subsystem.ExtMem
import os._

import java.util.regex._

class BeethovenChipStage extends Stage with Phase {
  override val shell = new Shell("beethoven-compile")
  val targets: Seq[PhaseDependency] = Seq(
    Dependency[freechips.rocketchip.stage.phases.Checks],
    Dependency[beethoven.Generation.Stage.PreElaborationPass],
    Dependency[chisel3.stage.phases.Checks],
    Dependency[chisel3.stage.phases.MaybeAspectPhase],
    Dependency[chisel3.stage.phases.Convert], // convert chirrtl to firrtl
    Dependency[beethoven.Generation.Stage.ExportCSymbolPhase],
    Dependency[firrtl.stage.phases.Compiler]
  )

  private val pm = new PhaseManager(targets)

  override def run(annotations: AnnotationSeq): AnnotationSeq =
    pm.transform(annotations)
}

object BeethovenBuild {
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

  private[beethoven] var partitionModules: Seq[String] = Seq("BeethovenTop")

  def requestModulePartition(moduleName: String): Unit =
    partitionModules = partitionModules :+ moduleName

  def getPartitions: Seq[String] = partitionModules

  private[beethoven] def addSource(): Unit = {}

  var postProcessorBundles: Seq[Any] = Seq.empty
  def addPostProcessorBundle(bundle: Any): Unit = {
    postProcessorBundles = postProcessorBundles :+ bundle
  }

  def beethovenRoot(): String = {
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

  val beethovenGenDir: String =
    beethovenRoot() + "/Beethoven-Hardware/vsim/generated-src"

  val beethovenVsimDir: String =
    beethovenRoot() + "/Beethoven-Hardware/vsim/"
  val beethovenBin: String = beethovenRoot() + "/bin/"
  val gsrc_dir = Path(BeethovenBuild.beethovenGenDir)
  val targetDir = gsrc_dir / "beethoven.build"

  var symbolicMemoryResources: Seq[Path] = Seq.empty
  var sourceList: Seq[Path] = Seq.empty

  def addSource(p: Path): Unit = {
    sourceList = sourceList :+ p
  }

  private[beethoven] def addSymbolicResource(p: Path): Unit = {
    symbolicMemoryResources = symbolicMemoryResources :+ p
  }
}

object BuildArgs {
  private[beethoven] var args: Map[String, Int] = Map.empty
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

class BeethovenBuild(config: => Config, buildMode: BuildMode = BuildMode.Synthesis) {


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
    new BeethovenChipStage().transform(
      AnnotationSeq(
        Seq(
          // if you want to get annotation output for debugging, uncomment the following line
          new EmitAllModulesAnnotation(classOf[VerilogEmitter]),
          new TargetDirAnnotation(targetDir.toString()),
          new TopModuleAnnotation(Class.forName("beethoven.Systems.BeethovenTop")),
          Generation.Stage.ConfigsAnnotation(configWithBuildMode),
          CustomDefaultMemoryEmission(MemoryNoInit),
          CustomDefaultRegisterEmission(useInitAsPreset = false, disableRandomization = true),
          RunFirrtlTransformAnnotation(new VerilogEmitter),
          NoDCEAnnotation
        )
      )
    )

    os.remove.all(targetDir / "firrtl_black_box_resource_files.f")
    val chiselGeneratedSrcs = WalkPath(targetDir)

    // --------------- Verilog Annotators ---------------
    //    KeepHierarchy(targetDir / "BeethovenTop.v")
//    partitionModules foreach println
    val movedSrcs = beethoven.Generation.Annotators.UniqueMv(sourceList, targetDir)

    ConstraintGeneration.slrMappings.foreach { slrMapping =>
      crossBoundaryDisableList = crossBoundaryDisableList :+ slrMapping._1
    }
    if (crossBoundaryDisableList.nonEmpty && buildMode == BuildMode.Synthesis) {
      CrossBoundaryDisable(crossBoundaryDisableList, targetDir)
    }
    if (configWithBuildMode(PlatformKey).platformType == PlatformType.FPGA &&
      !configWithBuildMode(PlatformKey).isInstanceOf[AWSF1Platform]) {
      val tc_axi = (0 until config(PlatformKey).extMem.nMemoryChannels) map { idx =>
        beethoven.Generation.Annotators.AnnotateXilinxInterface(
          f"M0${idx}_AXI", (targetDir / "BeethovenTop.v").toString(), XilinxInterface.AXI4)
        Some(f"M0${idx}_AXI")
      }
      // implies is AXI
      val tc_front = {
        beethoven.Generation.Annotators.AnnotateXilinxInterface(
          "S00_AXI", (targetDir / "BeethovenTop.v").toString(), XilinxInterface.AXI4)
        Some("S00_AXI")
      }

      val tcs = ((Seq(tc_front) ++ tc_axi) filter (_.isDefined) map (_.get)).mkString(":")
      Annotators.AnnotateTopClock(
        f"\\(\\* X_INTERFACE_PARAMETER = \"ASSOCIATED_BUSIF $tcs \" \\*\\)",
        targetDir / "BeethovenTop.v"
      )
    }

    os.write.over(gsrc_dir / "cmake_srcs.cmake",
      f"""set(SRCS ${movedSrcs.mkString("\n")}\n${chiselGeneratedSrcs.mkString("\n")})\n""")

    buildMode match {
      case bm: BuildMode.Tuning if !args.contains("--notune") =>
        val opts = if (bm.cmakeOpts.length == 1) bm.cmakeOpts(0) else {
          bm.cmakeOpts.reduce(_ + "." + _)
        }
        os.proc(Seq("python3",
          System.getenv("COMPOSER_ROOT") + "/Beethoven-Hardware/scripts/tune.py",
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
