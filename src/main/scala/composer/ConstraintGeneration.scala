package composer

import chipsalliance.rocketchip.config.Parameters
import os._

import java.io.FileWriter

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

    val id2Name = Map.from(Seq((SLRConstants.DEFAULT_SLR, defaultSLRName)) ++ (1 until p(PlatformNumSLRs)).map(i => (i, secondarySLRs(i-1))))
    (0 until p(PlatformNumSLRs)).foreach { slrID =>
      val cells = slrMappings.filter(_._2 == slrID)
      val plist = cells.map(_._1 + "*").reduce(_ + " " + _)
      f.write(f"add_cells_to_pblock ${id2Name(slrID)} [get_cells -hierarchical [list " + plist + " ] ]\n")
    }

    f.close()
  }
}
