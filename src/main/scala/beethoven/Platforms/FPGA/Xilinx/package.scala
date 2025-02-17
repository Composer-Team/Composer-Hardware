package beethoven.Platforms.FPGA

import beethoven.Platforms.FPGA.Xilinx.F2
import beethoven.{BeethovenBuild, tclMacro}

package object Xilinx {
  def getTclMacros(): Seq[String] = {
    BeethovenBuild.postProcessorBundles.filter(_.isInstanceOf[F2.tclMacro]).map {
      case F2.tclMacro(cmd, _) => cmd
    }
  }
}
