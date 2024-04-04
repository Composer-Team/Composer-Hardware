package composer.Floorplanning

import chipsalliance.rocketchip.config.Parameters
import composer.Generation.ComposerBuild
import composer.Platforms.FPGA.Xilinx.AWSF1Platform
import composer.Platforms.{MultiDiePlatform, Platform, PlatformKey, PlatformType}
import composer.{ComposerConstraintHint, ConstraintHintsKey}
import os.Path

import java.io.FileWriter

/**
 * TODO handle intra system cmd/resp routing
 */

object ConstraintGeneration {
  // map module -> SLR
  private[composer] var slrMappings: List[(String, Int)] = List.empty

  def addToSLR(moduleName: String, slr: Int): Unit = {
    require(!slrMappings.map(_._1).contains(moduleName), f"The module '$moduleName' has already been assigned to an SLR!")
    slrMappings = (moduleName, slr) :: slrMappings
  }

  def canDistributeOverSLRs()(implicit p: Parameters): Boolean =
    p(ConstraintHintsKey).contains(ComposerConstraintHint.DistributeCoresAcrossSLRs) &&
      p(PlatformKey).isInstanceOf[MultiDiePlatform]

  def writeConstraints()(implicit p: Parameters): Unit = {
    p(PlatformKey).platformType match {
      case PlatformType.FPGA =>
        val mdplat = p(PlatformKey).asInstanceOf[Platform with MultiDiePlatform]
        val path = Path(ComposerBuild.composerGenDir)
        val outPath = path / "user_constraints.xdc"
        if (!canDistributeOverSLRs()) {
          os.write.over(outPath, "")
          return
        }

        os.makeDir.all(path)
        val f = new FileWriter(outPath.toString())

        val slrs = mdplat.platformDies
        val id2Name = if (mdplat.isInstanceOf[AWSF1Platform])
          Map.from(slrs.zipWithIndex.map(q => q.copy(_1 = q._2, _2 = q._1.name)))
        else
          Map.from(slrs.zipWithIndex.map(q => q.copy(_1 = q._2, _2 = "composer_slr_" + q._1.name)))
        if (!mdplat.isInstanceOf[AWSF1Platform]) {
          // AWS constraints are appended to existing constraint file which already defines pblock names, no need to
          // redefine
          slrs.indices.foreach { i =>
            f.write(f"create_pblock ${id2Name(i)}\n" +
              f"resize_pblock -add SLR$i ${id2Name(i)} \n")
          }
        }

        mdplat.platformDies.zipWithIndex.foreach { case (_, slrID) =>
          val cells = slrMappings.filter(_._2 == slrID)
          val plist = cells.map(c => f"*${c._1}*").fold("")(_ + " " + _)
          f.write(f"add_cells_to_pblock ${id2Name(slrID)} [get_cells -hierarchical [list " + plist + " ] ]\n")
        }
        f.close()
      case PlatformType.ASIC => ;
    }
  }
}
