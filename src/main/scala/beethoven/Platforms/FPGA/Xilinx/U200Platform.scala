package beethoven.Platforms.FPGA.Xilinx

import beethoven.Platforms.PlatformType.PlatformType
import beethoven.Platforms._
import beethoven.Protocol.FrontBus._

class U200Platform(val memoryNChannels: Int) extends Platform with MultiDiePlatform {

  override val platformType: PlatformType = PlatformType.FPGA
  override val hasDiscreteMemory: Boolean = true

  override val frontBusBaseAddress: Long = 0
  override val frontBusAddressNBits: Int = 16
  override val frontBusAddressMask: Long = 0xFFFF
  override val frontBusBeatBytes: Int = 4
  override val frontBusCanDriveMemory: Boolean = false
  override val frontBusProtocol: FrontBusProtocol = new AXIFrontBusProtocol

  override val physicalMemoryBytes: Long = 0x400000000L
  override val memorySpaceAddressBase: Long = 0x0
  override val memorySpaceSizeBytes: Long = 0x400000000L
  override val memoryControllerIDBits: Int = 16
  override val memoryControllerBeatBytes: Int = 64

  override val prefetchSourceMultiplicity: Int = 64

  override val clockRateMHz: Int = 300

  override val platformDies: Seq[DieName] = Seq(
    DieName("0", frontBus = true),
    DieName("1", memoryBus = true),
    DieName("2")
  )
}