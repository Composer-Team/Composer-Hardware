package composer.Platforms

import chipsalliance.rocketchip.config.Config
import composer.{CXbarMaxDegree, CoreCommandLatency, HasCoherence, PlatformPhysicalMemoryBytes, PrefetchSourceMultiplicity}
import composer.Platforms.FPGA.PlatformSLRs
import composer.Platforms.FrontBusProtocol.FrontBusProtocol
import composer.Platforms.PlatformType.PlatformType
import freechips.rocketchip.subsystem.{ExtMem, MasterPortParams, MemoryPortParams}

class WithSimulationPlatform(nMemoryChannels: Int = 1,
                             platformType: PlatformType = PlatformType.FPGA,
                             frontBusProtocol: FrontBusProtocol = FrontBusProtocol.AXI4,
                             clockRateMHz: Int = 100,
                             prefetchMult: Int = 16
                            ) extends Config((_, _, _) => {
  case ExtMem =>
    Some(
      MemoryPortParams(
        MasterPortParams(
          base = 0,
          size = 1L << 49,
          beatBytes = 16,
          idBits = 6
        ),
        nMemoryChannels
      )
    )
  // 4GB total physical memory
  case PlatformPhysicalMemoryBytes => 4L << 30
  case FrontBusBaseAddress => 0x2000000000L
  case FrontBusAddressMask => 0xffffL
  case HasDMA => None
  // TODO this can be tuned
  case CXbarMaxDegree => 8
  case HasDiscreteMemory => false
  case FrontBusBeatBytes => 4
  case CoreCommandLatency => 0
  case PrefetchSourceMultiplicity => prefetchMult
  case HasCoherence => None
  case FrontBusAddressBits => 16

  case PlatformTypeKey => platformType
  case FrontBusProtocolKey => frontBusProtocol
  case PlatformNBRAM => 100000
  case PlatformNURAM => 100000
  case DefaultClockRateKey => clockRateMHz
  case PlatformNumSLRs => 1
  case PlatformSLRs => None

  case IsAWS => false
  case PostProcessorMacro => _: Config => ;
  case HasDisjointMemoryControllers => false
})
