package composer.Platforms.FPGA.Xilinx

import chipsalliance.rocketchip.config.Config
import composer.ComposerBuild
import composer.Platforms._
import composer.Platforms.FPGA._


private[composer] class AWS_sole(simulation: Boolean)
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
    case DefaultClockRateKey => 125
    case IsAWS => true
    case PostProcessorMacro =>
      _: Config =>
        if (!simulation) {
          require(false, "Fix this to call new top name")
          val cwd = ComposerBuild.composerVsimDir
          val cdir = ComposerBuild.composerBin
          val callable = os.proc(f"$cdir/aws-gen-build")
          callable.call(
            cwd = os.Path(cwd),
            stdin = os.Inherit,
            stdout = os.Inherit
          )
        }
  })


class WithAWSPlatform(nMemoryChannels: Int = 1, simulation: Boolean = true)
  extends Config(new U200Base(nMemoryChannels) ++ new AWS_sole(simulation))
