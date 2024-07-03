package beethoven.Platforms
import beethoven.Platforms.FPGA.Xilinx.KriaPlatform
import beethoven.Platforms.PlatformType.PlatformType

class SimulationPlatform(override val clockRateMHz: Int) extends KriaPlatform {
  override val platformType: PlatformType = PlatformType.FPGA
  override val prefetchSourceMultiplicity = 128
  override val memoryControllerBeatBytes = 128
}