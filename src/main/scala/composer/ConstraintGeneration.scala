package composer

import chipsalliance.rocketchip.config.Parameters
import os._

import java.io.FileWriter

object ConstraintGeneration {
  // map module -> SLR
  private var slrMappings: List[(String, Int)] = List.empty

  def addToSLR(moduleName: String, SLRID: Int): Unit = {
    require(!slrMappings.map(_._1).contains(moduleName), "This module has already been assigned to an SLR!")
    slrMappings = (moduleName, SLRID) :: slrMappings
  }

  def writeConstraints()(implicit p: Parameters): Unit = {
    val path = Path(System.getenv("COMPOSER_ROOT")) / "Composer-Hardware" / "vsim" / "generated-src"
    os.makeDir.all(path)
    val f = new FileWriter((path/"user_constraints.xdc").toString())
    (0 until p(PlatformNumSLRs)).foreach { i =>
      f.write(f"create_pblock composer_slr$i\nresize_pblock composer_slr$i -add SLR$i\n")
    }
    slrMappings.foreach { case (module, slr) =>
      f.write(f"add_cells_to_pblock composer_slr$slr [get_cells ]")
    }
  }
}
