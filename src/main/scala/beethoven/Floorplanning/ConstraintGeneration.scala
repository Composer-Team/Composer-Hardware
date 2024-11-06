package beethoven.Floorplanning

import chipsalliance.rocketchip.config.Parameters
import beethoven.Platforms._
import beethoven._

import java.io.FileWriter

object ConstraintGeneration {
  // map module -> SLR
  private[beethoven] var slrMappings: List[(String, Int)] = List.empty

  def addToSLR(moduleName: String, slr: Int): Unit = {
    require(!slrMappings.map(_._1).contains(moduleName), f"The module '$moduleName' has already been assigned to an SLR!")
    slrMappings = (moduleName, slr) :: slrMappings
  }

  def writeConstraints()(implicit p: Parameters): Unit = {
    p(PlatformKey).platformType match {
      case PlatformType.FPGA =>
        val outPath = BeethovenBuild.top_build_dir / "user_constraints.xdc"
        if (platform.physicalDevices.length == 1) {
          os.write.over(outPath, "")
          return
        }

        val f = new FileWriter(outPath.toString())

        val slrs = platform.physicalDevices
        val id2Name = if (platform.isInstanceOf[AWSF1Platform])
          Map.from(slrs.zipWithIndex.map(q => q.copy(_1 = q._2, _2 = q._1.name)))
        else
          Map.from(slrs.zipWithIndex.map(q => q.copy(_1 = q._2, _2 = "beethoven_slr_" + q._1.name)))
        if (!platform.isInstanceOf[AWSF1Platform]) {
          // AWS constraints are appended to existing constraint file which already defines pblock names, no need to
          // redefine
          slrs.indices.foreach { i =>
            f.write(f"create_pblock ${id2Name(i)}\n" +
              f"resize_pblock -add SLR$i ${id2Name(i)} \n")
          }
        }

        platform.physicalDevices.zipWithIndex.foreach { case (_, slrID) =>
          val cells = slrMappings.filter(_._2 == slrID)
          val plist = cells.map(c => f"*${c._1}*").fold("")(_ + " " + _)
          f.write(f"add_cells_to_pblock ${id2Name(slrID)} [get_cells -hierarchical [list " + plist + " ] ]\n")
        }
        f.close()
      case PlatformType.ASIC => ;
    }
  }
}
