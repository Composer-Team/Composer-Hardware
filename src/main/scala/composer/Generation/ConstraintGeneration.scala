package composer.Generation

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import composer.Systems._
import composer._
import composer.common.BUFG
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
 * we limit it to a single TL channel. Picture below:
 *
 * SLR default               |    SLR secondary
 * |
 * |             |------channel
 * To dram                   |             |------channel
 * <----X---------buffer---------buffer----X------channel
 * |                    |
 * |---mem node         |
 *
 * dTODO handle intra system cmd/resp routing
 *
 * There's some stuff here where we're passing around lambdas. We do this because LazyModule doesn't promise a clock/
 * reset signal in the contained module. This lambda is basically to enforce lazy evaluation of clock/reset signals
 */

object ConstraintGeneration {
  // map module -> SLR
  private var slrMappings: List[(String, Int)] = List.empty

  def addToSLR(moduleName: String, slr: Int): Unit = {
    require(!slrMappings.map(_._1).contains(moduleName), f"The module '$moduleName' has already been assigned to an SLR!")
    slrMappings = (moduleName, slr) :: slrMappings
  }

  def canDistributeOverSLRs()(implicit p: Parameters): Boolean = p(ConstraintHintsKey).contains(ComposerConstraintHint.DistributeCoresAcrossSLRs) && p(PlatformNumSLRs) > 1

  def writeConstraints()(implicit p: Parameters): Unit = {
    if (!canDistributeOverSLRs()) return
    assert(p(PlatformSLRs).get.count(_.default) == 1, "Require exactly one default SLR in config. Condition not met.")

    val path = Path(System.getenv("COMPOSER_ROOT")) / "Composer-Hardware" / "vsim" / "generated-src"
    os.makeDir.all(path)
    val f = new FileWriter((path / "user_constraints.xdc").toString())

    val defaultSLRName = p(PlatformSLRs).get.filter(_.default == true)(0).name
    val secondarySLRs = p(PlatformSLRs).get.filter(_.default == false).map(_.name)
    if (!p(IsAWS)) {
      // AWS constraints are appended to existing constraint file which already defines pblock names, no need to
      // redefine
      secondarySLRs.foreach { i =>
        f.write(f"create_pblock composer_slr$i\nresize_pblock composer_slr$i -add SLR$i")
      }
    }

    val id2Name = Map.from(Seq((SLRConstants.DEFAULT_SLR, defaultSLRName)) ++ (1 until p(PlatformNumSLRs)).map(i => (i, secondarySLRs(i - 1))))
    (0 until p(PlatformNumSLRs)).foreach { slrID =>
      val cells = slrMappings.filter(_._2 == slrID)
      val plist = cells.map(_._1 + "*").reduce(_ + " " + _)
      f.write(f"add_cells_to_pblock ${id2Name(slrID)} [get_cells -hierarchical [list " + plist + " ] ]\n")
    }

    f.close()
  }
}

class LazyModuleImpWithSLRs(wrapper: LazyModuleWithSLRs)(implicit p: Parameters) extends LazyModuleImp(wrapper) {
  implicit val slrId: Option[Int] = if (ConstraintGeneration.canDistributeOverSLRs()) Some(SLRConstants.DEFAULT_SLR) else None
  private var clockMap: List[(Module, Int)] = List.empty
  private var gl_id = 0

  /**
   * Automate SLR mapping a little bit with this interface. Wraps around standard Chisel `Module` interface and performs
   * some additonal bookkeeping whenever tieClocks(). If slrId is not manually defined, it assumes the Default SLR
   * specified by the Platform Configuration.
   */
  def ModuleWithSLR[T <: Module](m: => T)(implicit valName: ValName): T = {
    val mod = Module(m)
    slrId match {
      case None => ;
      case Some(real_slrid) =>
        val name = wrapper.baseName + "_" + valName.name + "_" + gl_id
        println("wrapper name is " + wrapper.baseName)
        mod.suggestName(name)
        gl_id = gl_id + 1
        ConstraintGeneration.addToSLR(name, real_slrid)
        clockMap = (mod, slrId.get) :: clockMap
    }
    mod
  }

  /**
   * Generate and tie off clocks to secondary SLRs. Only call this after LazyModules are finalized and module can be
   * generated.
   */
  def tieClocks(): Unit = {
    if (!ConstraintGeneration.canDistributeOverSLRs()) return

    val slr_ctrls: Map[Int, Clock] = Map.from((0 until p(PlatformNumSLRs)).filter(_ != SLRConstants.DEFAULT_SLR).map { slr =>
      val CLmodName = f"${wrapper.baseName}_SLRClockCrossing_${slr}_clock"
      val cl_mod = Module(new BUFG)
      cl_mod.suggestName(CLmodName)
      cl_mod.io.I := clock.asBool
      ConstraintGeneration.addToSLR(CLmodName, slr)
      (slr, cl_mod.io.O.asClock)
    } ++ Seq((SLRConstants.DEFAULT_SLR, clock)))


    wrapper.lazyClockMap.foreach { case (lm, slr) =>
      val imp = lm.module.asInstanceOf[LazyModuleImp]
      imp.clock := slr_ctrls(slr)

    }

    clockMap.foreach { case (m, slr) =>
      m.clock := slr_ctrls(slr)
    }
  }

}

abstract class LazyModuleWithSLRs()(implicit p: Parameters) extends LazyModule {
  var lazyClockMap: List[(LazyModule, Int)] = List.empty
  val baseName: String
  val cores: List[(Int, ComposerCoreWrapper)]
  val module: LazyModuleImpWithSLRs
  private var gl_id = 0
  implicit val slrId: Option[Int] = if (ConstraintGeneration.canDistributeOverSLRs()) Some(SLRConstants.DEFAULT_SLR) else None

  object WithSLR {
    def apply[T <: LazyModule](mod: T)(implicit slrId: Option[Int]): T = {
      slrId match {
        case None => ;
        case Some(real_slrid) =>
          val name = baseName + "_" + mod.name + "_" + gl_id
          mod.suggestName(name)
          gl_id = gl_id + 1
          ConstraintGeneration.addToSLR(name, real_slrid)
          lazyClockMap = (mod, slrId.get) :: lazyClockMap
      }
      mod
    }
  }

  def LazyModuleWithSLR[T <: LazyModule](mod: => T)(implicit slrId: Option[Int], valName: ValName): T = {
    val lm = LazyModule(mod)
    val name = baseName + "_" + valName.name + "_" + gl_id
    lm.suggestName(name)
    gl_id = gl_id + 1
    slrId match {
      case None => ;
      case Some(a) =>
        ConstraintGeneration.addToSLR(name, a)
        lazyClockMap = (lm, slrId.get) :: lazyClockMap
    }
    lm

  }

}