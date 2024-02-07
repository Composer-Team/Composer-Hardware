package composer.Platforms.FPGA.Xilinx

import chipsalliance.rocketchip.config.Config
import composer.Generation._
import composer.Platforms._
import composer.Platforms.FPGA._
import os.Path

object AWS_sole {
  def apply(c: Config, pths: Seq[Path]): Unit = {
    if (c(BuildModeKey) == BuildMode.Synthesis) {
      // rename composer.v to composer.sv
      val composer_sv = os.Path(ComposerBuild.composerGenDir) / "composer.sv"
      val composer_v = os.Path(ComposerBuild.composerGenDir) / "composer.v"
      os.copy.over(composer_v, composer_sv)
    }
  }
}

private[composer] class AWS_sole(clock_rate_mhz: Int)
  extends Config((_, _, _) => {
    // why did this ever become 128? It's 2X the bus width... That doesn't seem to make much sense...
    //  case CacheBlockBytes => 128
    case PlatformSLRs =>
      Some(Seq(
        SLRName("pblock_CL_mid", memoryBus = true),
        SLRName("pblock_CL_bot", frontBus = true),
        SLRName("pblock_CL_top")))
    case PlatformPreferedSLRCmdRespRoutingPath =>
      Some(Seq(
        "pblock_CL_bot",
        "pblock_CL_mid",
        "pblock_CL_top"))
    case DefaultClockRateKey => clock_rate_mhz
    case IsAWS => true
    case PostProcessorMacro => AWS_sole.apply _
  })


class WithAWSPlatform(nMemoryChannels: Int = 1, fpga_clock_rate_mhz: Int = 250)
  extends Config(new U200Base(nMemoryChannels) ++ new AWS_sole(fpga_clock_rate_mhz))
