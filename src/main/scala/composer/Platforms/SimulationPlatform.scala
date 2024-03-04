package composer.Platforms
import composer.Platforms.FPGA.Xilinx.KriaPlatform
import composer.Platforms.PlatformType.PlatformType

class SimulationPlatform(override val clockRateMHz: Int) extends KriaPlatform {
  override val platformType: PlatformType = PlatformType.FPGA
}
//  case ExtMem =>
//    Some(
//      MemoryPortParams(
//        MasterPortParams(
//          base = 0,
//          size = 1L << 49,
//          beatBytes = 16,
//          idBits = 6
//        ),
//        nMemoryChannels
//      )
//    )
//  // 4GB total physical memory
//  case PlatformPhysicalMemoryBytes => 4L << 30
//  case FrontBusBaseAddress => 0x2000000000L
//  case FrontBusAddressMask => 0xffffL
//  case HasDMA => None
//  // TODO this can be tuned
//  case platform.xbarMaxDegree => 8
//  case HasDiscreteMemory => false
//  case FrontBusBeatBytes => 4
//  case CoreCommandLatency => 0
//  case PrefetchSourceMultiplicity => prefetchMult
//  case HasCoherence => None
//  case FrontBusAddressBits => 16
//
//  case PlatformTypeKey => platformType
//  case FrontBusProtocolKey => frontBusProtocol
//  case PlatformNBRAM => 100000
//  case PlatformNURAM => 100000
//  case DefaultClockRateKey => clockRateMHz
//  case PlatformNumSLRs => 1
//  case PlatformSLRs => None
//
//  case IsAWS => false
//  case PostProcessorMacro => (_: Config, _: Seq[Path]) => ;
//  case HasDisjointMemoryControllers => false
//})
