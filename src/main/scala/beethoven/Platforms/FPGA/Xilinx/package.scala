package beethoven.Platforms.FPGA

import beethoven.Generation.BeethovenBuild

package object Xilinx {
  def getTclMacros(): Seq[String] = {
    BeethovenBuild.postProcessorBundles.filter(_.isInstanceOf[tclMacro]).map {
      case tclMacro(cmd, _) => cmd
    }
  }
}
