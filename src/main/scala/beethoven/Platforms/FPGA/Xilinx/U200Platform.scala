package beethoven

import beethoven.Platforms.PlatformType.PlatformType
import beethoven.Platforms._
import beethoven.Protocol.FrontBus._

abstract class U200Platform(val memoryNChannels: Int) extends Platform {

  override val platformType: PlatformType = PlatformType.FPGA
  override val hasDiscreteMemory: Boolean = true

  override val frontBusBaseAddress: Long = 0
  override val frontBusAddressNBits: Int = 16
  override val frontBusAddressMask: Long = 0xFFFF
  override val frontBusBeatBytes: Int = 4
  override val frontBusProtocol: FrontBusProtocol = new AXIFrontBusProtocol(true)

  override val physicalMemoryBytes: Long = 0x400000000L
  override val memorySpaceAddressBase: Long = 0x0
  override val memorySpaceSizeBytes: BigInt = BigInt(1) << 34
  override val memoryControllerIDBits: Int = 16
  override val memoryControllerBeatBytes: Int = 64

  override val prefetchSourceMultiplicity: Int = 64

  override val clockRateMHz: Int = 300

  override val physicalDevices = List(
    DeviceConfig(0, "SLR0"),
    DeviceConfig(1, "SLR1"),
    DeviceConfig(2, "SLR2")
  )
}