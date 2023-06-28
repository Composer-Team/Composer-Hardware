package composer.Platforms.ASIC

import chipsalliance.rocketchip.config.Config
import composer.{CoreCommandLatency, CXbarMaxDegree, PlatformPhysicalMemoryBytes}
import composer.Platforms.{DefaultClockRateKey, FrontBusAddressMask, FrontBusBaseAddress, FrontBusBeatBytes, FrontBusProtocol, FrontBusProtocolKey, HasDiscreteMemory, HasDisjointMemoryControllers, HasDMA, IsAWS, PlatformType, PlatformTypeKey}
import freechips.rocketchip.subsystem.{ExtMem, MasterPortParams, MemoryPortParams}


class WithChipKitPlatform(synthesis: Boolean = false)
  extends Config((_, _, _) => {
    case ExtMem =>
      Some(
        MemoryPortParams(
          MasterPortParams(
            base = 0,
            size = 1L << 34,
            beatBytes = 16,
            idBits = 6
          ),
          1
        )
      )
    // 4GB total physical memory
    case PlatformPhysicalMemoryBytes => 1L << 34
    case FrontBusBaseAddress => 0x2000000000L
    case FrontBusAddressMask => 0xffffL
    case HasDMA => None
    case CXbarMaxDegree => 8
    case HasDiscreteMemory => false
    case FrontBusBeatBytes => 4
    case CoreCommandLatency => 0

    case BuildSynthesisKey => synthesis
    case PlatformTypeKey => PlatformType.ASIC
    case FrontBusProtocolKey => FrontBusProtocol.AHB
    case DefaultClockRateKey => 100

    case IsAWS => false
    case HasDisjointMemoryControllers => false

  })