package composer.Platforms.FPGA

import composer.Generation.ComposerBuild

package object Xilinx {
  def getTclMacros(): Seq[String] = {
    ComposerBuild.postProcessorBundles.filter(_.isInstanceOf[tclMacro]).map {
      case tclMacro(cmd, _) => cmd
    }
  }
}
