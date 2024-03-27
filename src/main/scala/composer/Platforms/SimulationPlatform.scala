package composer.Platforms
import composer.Platforms.FPGA.Xilinx.KriaPlatform
import composer.Platforms.PlatformType.PlatformType

class SimulationPlatform(override val clockRateMHz: Int) extends KriaPlatform {
  override val platformType: PlatformType = PlatformType.FPGA
}