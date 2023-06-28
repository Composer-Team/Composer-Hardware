package composer.Platforms.FPGA.Xilinx

import chipsalliance.rocketchip.config.Config
import composer.{CoreCommandLatency, CXbarMaxDegree, PlatformPhysicalMemoryBytes}
import composer.Platforms.{DefaultClockRateKey, FrontBusAddressMask, FrontBusBaseAddress, FrontBusBeatBytes, FrontBusProtocol, FrontBusProtocolKey, HasDiscreteMemory, HasDisjointMemoryControllers, HasDMA, IsAWS, PlatformNBRAM, PlatformNumSLRs, PlatformNURAM, PlatformType, PlatformTypeKey, PostProcessorMacro}
import composer.Platforms.FPGA.PlatformSLRs
import freechips.rocketchip.subsystem.{ExtMem, MasterPortParams, MemoryPortParams}

class WithKriaPlatform(nMemoryChannels: Int = 1)
  extends Config((_, _, _) => {
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

    case PlatformTypeKey => PlatformType.FPGA
    case FrontBusProtocolKey => FrontBusProtocol.AXI4
    case PlatformNBRAM => 144
    case PlatformNURAM => 64
    case DefaultClockRateKey => 100
    case PlatformNumSLRs => 1
    case PlatformSLRs => None

    case IsAWS => false
    case PostProcessorMacro => _: Config => ;
    case HasDisjointMemoryControllers => false
  })
