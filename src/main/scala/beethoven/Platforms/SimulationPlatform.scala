package beethoven.Platforms
import beethoven.KriaPlatform
import beethoven.Platforms.PlatformType.PlatformType

class SimulationPlatform(override val clockRateMHz: Int) extends KriaPlatform {
  override val platformType: PlatformType = PlatformType.FPGA
  override val prefetchSourceMultiplicity = 128
  override val memoryControllerBeatBytes = 128
}