package composer.Platforms.FPGA.Xilinx

import chipsalliance.rocketchip.config.Config
import composer.Generation._
import composer.Platforms.FPGA.Xilinx.Templates.SynthScript
import composer.Platforms.PlatformType.PlatformType
import composer.Platforms._
import composer.Protocol.FrontBus.{AXIFrontBusProtocol, FrontBusProtocol}
import os.Path


class KriaPlatform(val memoryNChannels: Int = 1) extends Platform with HasPostProccessorScript {

  override val platformType: PlatformType = PlatformType.FPGA
  override val hasDiscreteMemory: Boolean = false

  override val frontBusBaseAddress: Long = 0x2000000000L
  override val frontBusAddressNBits: Int = 40
  override val frontBusAddressMask: Long = 0xffffL
  override val frontBusBeatBytes: Int = 4
  override val frontBusCanDriveMemory: Boolean = false
  override val frontBusProtocol: FrontBusProtocol = new AXIFrontBusProtocol

  override val physicalMemoryBytes: Long = 4L << 30
  override val memorySpaceAddressBase: Long = 0x0
  override val memorySpaceSizeBytes: Long = 1L << 49
  override val memoryControllersAreDisjoint: Boolean = true
  override val memoryControllerIDBits: Int = 6
  override val memoryControllerBeatBytes: Int = 16

  override def postProcessorMacro(c: Config, paths: Seq[Path]): Unit = {
    if (c(BuildModeKey) == BuildMode.Synthesis) {
      os.write.over(os.Path(ComposerBuild.composerGenDir) / "synth.tcl",
        SynthScript(
          "composer",
          "output",
          "xck26-sfvc784-2LV-c",
          "xilinx.com:kv260_som:part0:1.4",
          clockRateMHz.toString
        ))
    }
  }
}