package beethoven

import beethoven.BeethovenBuild._
import beethoven.Floorplanning.ConstraintGeneration
import beethoven.Generation.Annotators.AnnotateXilinxInterface.XilinxInterface
import beethoven.Generation.Annotators.{CrossBoundaryDisable, WalkPath}
import beethoven.Generation.{Annotators, vcs}
import beethoven.Platforms._
import chipsalliance.rocketchip.config._
import firrtl._
import firrtl.options.PhaseManager.PhaseDependency
import firrtl.options._
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.{NoConstantPropagationAnnotation, NoDCEAnnotation}
import freechips.rocketchip.stage._
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
    Dependency[firrtl.stage.phases.Compiler]
  )

  private val pm = new PhaseManager(targets)

  override def run(annotations: AnnotationSeq): AnnotationSeq =
    pm.transform(annotations)
}

object BeethovenBuild {
  private val errorNoCR =
    "Environment variables 'BEETHOVEN_PATH' is not visible and no shell configuration file found.\n" +
      " Please define or configure IDE to see this enviornment variable\n"

  private var crossBoundaryDisableList: Seq[String] = Seq.empty

  def disableCrossBoundaryOptimizationForModule(moduleName: String): Unit = {
    crossBoundaryDisableList = crossBoundaryDisableList :+ moduleName
  }

  private def filterFIRRTL(path: Path): Unit = {
    os.proc(Seq("sed", "-i.backup", "-e", "1d", "-e", "/printf/d", "-e", "/assert/d", path.toString())).call()
  }

  private[beethoven] var partitionModules: Seq[String] = Seq("BeethovenTop")
  var separateCompileCells: Seq[String] = Seq.empty
  def requestModulePartition(moduleName: String): Unit = {
    partitionModules = (partitionModules :+ moduleName).distinct
  }

  def requestSeparateCompileCell(cellName: String): Unit =
    separateCompileCells = (separateCompileCells :+ cellName).distinct

  def getPartitions: Seq[String] = partitionModules

  private[beethoven] def addSource(): Unit = {}

  var postProcessorBundles: Seq[Any] = Seq.empty
  def addPostProcessorBundle(bundle: Any): Unit = {
    postProcessorBundles = postProcessorBundles :+ bundle
  }

  def beethovenRoot(): String = {
    if (System.getenv("BEETHOVEN_PATH") != null)
      return System.getenv("BEETHOVEN_PATH")
    val sh_full = System.getenv("SHELL")
    if (sh_full == null) throw new Exception(errorNoCR)
    val sh = sh_full.split("/").last
    val config = os.read(os.home / f".${sh}rc")
    val pattern = Pattern.compile("export BEETHOVEN_PATH=([a-zA-Z/.]*)")
    val matcher = pattern.matcher(config)
    if (matcher.find()) matcher.group(0).split("=")(1).strip()
    else throw new Exception(errorNoCR)
  }

  private val beethovenGenDir: String =
    beethovenRoot() + "/build/"
  val top_build_dir = Path(BeethovenBuild.beethovenGenDir)
  val hw_build_dir = top_build_dir / "hw"

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
}

class BeethovenBuild(config: AcceleratorConfig,
                     platform: Platform,
                     buildMode: BuildMode = BuildMode.Synthesis,
                     additional_parameter: Option[PartialFunction[Any, Any]] = None) {
  final def main(args: Array[String]): Unit = {
    //    args.foreach(println(_))
//    println("Running with " + Runtime.getRuntime.freeMemory() + "B memory")
//    println(Runtime.getRuntime.maxMemory.toString + "B")
    BuildArgs.args = Map.from(
      args.filter(str => str.length >= 2 && str.substring(0, 2) == "-D").map {
        opt =>
          val pr = opt.substring(2).split("=")
          //          println(pr(0) + " " + pr(1))
          (pr(0), pr(1).toInt)
      }
    )
    os.remove.all(hw_build_dir)
    os.makeDir.all(hw_build_dir)
    val configWithBuildMode = {
      val w = new WithBeethoven(
        platform = platform).alterPartial {
        case BuildModeKey => buildMode
        case AcceleratorSystems => config.configs
      }
      additional_parameter match {
        case Some(f) => w.alterPartial(f)
        case None => w
      }
    }
    beethoven.platform(configWithBuildMode).platformCheck()

    new BeethovenChipStage().transform(
      AnnotationSeq(
        Seq(
          // if you want to get annotation output for debugging, uncomment the following line
          new EmitAllModulesAnnotation(classOf[VerilogEmitter]),
          new TargetDirAnnotation(hw_build_dir.toString()),
          new TopModuleAnnotation(Class.forName("beethoven.Systems.BeethovenTop")),
          Generation.Stage.ConfigsAnnotation(configWithBuildMode),
          CustomDefaultMemoryEmission(MemoryNoInit),
          CustomDefaultRegisterEmission(useInitAsPreset = false, disableRandomization = true),
          RunFirrtlTransformAnnotation(new VerilogEmitter),
//          NoDCEAnnotation,
//          NoConstantPropagationAnnotation
        )
      )
    )

    os.remove.all(hw_build_dir / "firrtl_black_box_resource_files.f")
    val allChiselGeneratedSrcs = WalkPath(hw_build_dir)
    val chiselGeneratedSrcs = allChiselGeneratedSrcs.filter(a => !a.toString().contains("ShiftReg") && !a.toString().contains("Queue"))
    val shifts = allChiselGeneratedSrcs.filter(a => a.toString().contains("ShiftReg") || a.toString().contains("Queue"))

    // --------------- Verilog Annotators ---------------
    //    KeepHierarchy(targetDir / "BeethovenTop.v")
//    partitionModules foreach println
    val movedSrcs = beethoven.Generation.Annotators.UniqueMv(sourceList, hw_build_dir) :+ {
      val s = hw_build_dir / "BeethovenAllShifts.v"
      val stxts = shifts.map(a => os.read(a))
      os.write(s, stxts.mkString("\n\n"))
      shifts.foreach(os.remove(_))
      s
    }

    ConstraintGeneration.slrMappings.foreach { slrMapping =>
      crossBoundaryDisableList = crossBoundaryDisableList :+ slrMapping._1
    }
    if (crossBoundaryDisableList.nonEmpty && buildMode == BuildMode.Synthesis) {
      CrossBoundaryDisable(crossBoundaryDisableList, top_build_dir)
    }
    if (configWithBuildMode(PlatformKey).platformType == PlatformType.FPGA &&
      !configWithBuildMode(PlatformKey).isInstanceOf[AWSF1Platform]) {
      val tc_axi = (0 until platform.extMem.nMemoryChannels) map { idx =>
        beethoven.Generation.Annotators.AnnotateXilinxInterface(
          f"M0${idx}_AXI", (hw_build_dir / "BeethovenTop.v").toString(), XilinxInterface.AXI4)
        Some(f"M0${idx}_AXI")
      }
      // implies is AXI
      val tc_front = {
        beethoven.Generation.Annotators.AnnotateXilinxInterface(
          "S00_AXI", (hw_build_dir / "BeethovenTop.v").toString(), XilinxInterface.AXI4)
        Some("S00_AXI")
      }

      val tcs = ((Seq(tc_front) ++ tc_axi) filter (_.isDefined) map (_.get)).mkString(":")
      Annotators.AnnotateTopClock(
        f"\\(\\* X_INTERFACE_PARAMETER = \"ASSOCIATED_BUSIF $tcs \" \\*\\)",
        hw_build_dir / "BeethovenTop.v"
      )
    }

    os.makeDir.all(top_build_dir)
    os.write.over(top_build_dir / "cmake_srcs.cmake",
      f"""set(SRCS ${movedSrcs.mkString("\n")}\n${chiselGeneratedSrcs.mkString("\n")})\n""")
//    println("wrote to " + gsrc_dir / "vcs_srcs.in")
    os.write.over(top_build_dir / "vcs_srcs.in",
      chiselGeneratedSrcs.mkString("\n") + "\n" + movedSrcs.mkString("\n"))
    vcs.HarnessGenerator.generateHarness()(configWithBuildMode)

    buildMode match {
      case BuildMode.Synthesis =>
        platform match {
          case pwpp: Platform with HasPostProccessorScript =>
            pwpp.postProcessorMacro(configWithBuildMode, movedSrcs ++ chiselGeneratedSrcs)
          case _ => ;
        }
      case _ => ;
    }
  }
}
