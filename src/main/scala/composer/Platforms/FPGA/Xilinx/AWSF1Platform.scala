package composer.Platforms.FPGA.Xilinx

import chipsalliance.rocketchip.config.Config
import composer.Generation._
import composer.Platforms._
import composer.Platforms.FPGA._
import os.Path

object AWS_sole {
  def apply(c: Config, pths: Seq[Path]): Unit = {
  }
}

class AWSF1Platform(memoryNChannels: Int) extends U200Platform(memoryNChannels) with HasPostProccessorScript {
  override val platformDies = Seq(
    DieName("pblock_CL_mid", memoryBus = true),
    DieName("pblock_CL_bot", frontBus = true),
    DieName("pblock_CL_top"))

  override val platformPreferedDieCmdRespRoutingPath: Seq[String] = Seq(
    "pblock_CL_bot",
    "pblock_CL_mid",
    "pblock_CL_top")

  override def postProcessorMacro(c: Config, paths: Seq[Path]): Unit = {
    if (c(BuildModeKey) == BuildMode.Synthesis) {
      // rename composer.v to composer.sv
      val top_file = os.Path(ComposerBuild.composerGenDir) / "aws" / "composer.sv"
      os.makeDir.all(os.Path(ComposerBuild.composerGenDir) / "aws")
      os.remove.all(top_file)
      os.proc("touch", top_file.toString()).call()
      os.walk(os.Path(ComposerBuild.composerGenDir) / "composer.build") foreach { f =>
        os.write.append(top_file, os.read(f))
      }
    }
  }
}