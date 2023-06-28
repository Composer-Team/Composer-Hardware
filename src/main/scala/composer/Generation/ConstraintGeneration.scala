package composer.Generation

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import composer._
import composer.common.BUFG
import composer.Platforms._
import composer.Platforms.FPGA._
import freechips.rocketchip.diplomacy._
import os.Path

import java.io.FileWriter

/**
 * Weird Code Alert:
 * We create these modules at the System Level
 * Although it would be proper to create a whole new sub-hierarchy, I think for the time being that adds a whole
 * bunch of complexity without any clear benefit besides simplifying clock routing. And since the architecture is
 * supposed to be transparent to the underlying platform (for the developer's sake), I'm making the decision that
 * platform-specific details should not be visible from the architectural-level - at least for the time being. I
 * may be wrong about this
 *
 * Anyways, we deal with SLR routing in the following way:
 * Round robin assign cores to SLR mappings with preference for the default SLR. This should ease routing but may
 * overutilize the default SLR because we have so much other logic (e.g. memory subsystem, MMIO drivers) on the
 * default SLR.
 *
 * For the memory system, we condense all of the memory channels down to a single TL port so that it can be easily
 * routed across the SLR. Previous approaches allowed channels to be routed across the SLR fabric independently but
 * even for a modest number of channels (ie 4), we consumed around 50% of the SLR routing resources. For this reason,
 * we limit it to a single TL channel.
 *
 * TODO handle intra system cmd/resp routing
 *
 * There's some stuff here where we're passing around lambdas. We do this because LazyModule doesn't promise a clock/
 * reset signal in the contained module. This lambda is basically to enforce lazy evaluation of clock/reset signals
 */

object ConstraintGeneration {
  // map module -> SLR
  private[composer] var slrMappings: List[(String, Int)] = List.empty

  def addToSLR(moduleName: String, slr: Int): Unit = {
    require(!slrMappings.map(_._1).contains(moduleName), f"The module '$moduleName' has already been assigned to an SLR!")
    slrMappings = (moduleName, slr) :: slrMappings
  }

  def canDistributeOverSLRs()(implicit p: Parameters): Boolean = p(ConstraintHintsKey).contains(ComposerConstraintHint.DistributeCoresAcrossSLRs) && p(PlatformNumSLRs) > 1

  def writeConstraints()(implicit p: Parameters): Unit = {
    p(PlatformTypeKey) match {
      case PlatformType.FPGA =>

        val path = Path(ComposerBuild.composerGenDir)
        val outPath = path / "user_constraints.xdc"
        if (!canDistributeOverSLRs()) {
          os.write.over(outPath, "")
          return
        }

        os.makeDir.all(path)
        val f = new FileWriter(outPath.toString())

        val slrs = p(PlatformSLRs).get
        val id2Name = if (p(IsAWS)) {
          Map.from((0 until p(PlatformNumSLRs)).map(i => (i, slrs(i).name)))
        } else Map.from((0 until p(PlatformNumSLRs)).map(i => (i, "composer_slr" + slrs(i).name)))
        if (!p(IsAWS)) {
          // AWS constraints are appended to existing constraint file which already defines pblock names, no need to
          // redefine
          slrs.indices.foreach { i =>
            f.write(f"create_pblock ${id2Name(i)}\n" +
              f"resize_pblock -add SLR$i ${id2Name(i)} \n")
          }
        }

        (0 until p(PlatformNumSLRs)).foreach { slrID =>
          val cells = slrMappings.filter(_._2 == slrID)
          val plist = cells.map(c => f"*${c._1}*").fold("")(_ + " " + _)
          f.write(f"add_cells_to_pblock ${id2Name(slrID)} [get_cells -hierarchical [list " + plist + " ] ]\n")
        }
        f.close()
      case PlatformType.ASIC => ;
    }
  }
}

class LazyModuleImpWithSLRs(wrapper: LazyModuleWithSLRs)(implicit p: Parameters) extends LazyModuleImp(wrapper) {
  private var clockMap: List[(Module, Int)] = List.empty
  private var gl_id = 0

  /**
   * Automate SLR mapping a little bit with this interface. Wraps around standard Chisel `Module` interface and performs
   * some additonal bookkeeping whenever tieClocks(). If slrId is not manually defined, it assumes the Default SLR
   * specified by the Platform Configuration.
   */
  def ModuleWithSLR[T <: Module](m: => T, real_slrid: Int, requestedName: Option[String] = None)(implicit valName: ValName): T = {
    val mod = Module(m)
    if (!ConstraintGeneration.canDistributeOverSLRs()) return mod
    val name = requestedName.getOrElse(wrapper.baseName + "_" + valName.name + "_" + gl_id)
    mod.suggestName(name)
    gl_id = gl_id + 1
    ConstraintGeneration.addToSLR(name, real_slrid)
    clockMap = (mod, real_slrid) :: clockMap
    mod
  }


  /**
   * Generate and tie off clocks to secondary SLRs. Only call this after LazyModules are finalized and module can be
   * generated.
   */
  def tieClocks(): Unit = {
    if (!ConstraintGeneration.canDistributeOverSLRs()) return

    val slr_ctrls: Map[Int, Clock] = Map.from((0 until p(PlatformNumSLRs)).filter(_ != SLRHelper.DEFAULT_SLR).map { slr =>
      val CLmodName = f"${wrapper.baseName}_SLRClockCrossing_${slr}_clock"
      val cl_mod = Module(new BUFG)
      cl_mod.suggestName(CLmodName)
      cl_mod.io.I := clock.asBool
      ConstraintGeneration.addToSLR(CLmodName, slr)
      (slr, cl_mod.io.O.asClock)
    } ++ Seq((SLRHelper.DEFAULT_SLR, clock)))


    wrapper.lazyClockMap.foreach { case (lm, slr) =>
      val imp = lm.module.asInstanceOf[LazyModuleImp]
      imp.clock := slr_ctrls(slr)

    }

    clockMap.foreach { case (m, slr) =>
      m.clock := slr_ctrls(slr)
    }
  }
}

abstract class AssignmentAnnotation {
  def transform[T <: LazyModule](d: T, slr_id: Int, name: String): T
}

abstract class LazyModuleWithSLRs()(implicit p: Parameters) extends LazyModule {
  var lazyClockMap: List[(LazyModule, Int)] = List.empty
  val baseName: String
  private var gl_id = 0

  def LazyModuleWithSLR[T <: LazyModule](mod: => T, annotations: Seq[AssignmentAnnotation] = Seq.empty, slr_id: Int, requestedName: Option[String] = None)(implicit valName: ValName): T = {
    val lm = LazyModule(mod)
    if (!ConstraintGeneration.canDistributeOverSLRs()) return lm
    val name = requestedName.getOrElse(baseName + "_" + valName.name) + "_" + gl_id
    lm.suggestName(name)
    gl_id = gl_id + 1
    ConstraintGeneration.addToSLR(name, slr_id)
    lazyClockMap = (lm, slr_id) :: lazyClockMap
    annotations.foldRight(lm) { case (annot: AssignmentAnnotation, cs) => annot.transform(cs, slr_id, name) }
  }
}