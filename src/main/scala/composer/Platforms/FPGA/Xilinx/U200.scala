package composer.Platforms.FPGA.Xilinx

import chipsalliance.rocketchip.config.Config
import composer.{CXbarMaxDegree, CoreCommandLatency, HasCoherence, PlatformPhysicalMemoryBytes, PrefetchSourceMultiplicity}
import composer.Platforms._
import composer.Platforms.FPGA._
import freechips.rocketchip.subsystem.{ExtMem, MasterPortParams, MemoryPortParams}

private[composer] class U200Base(nMemoryChannels: Int)
  extends Config((_, _, _) => {
    case PlatformNumSLRs => 3
    case ExtMem =>
      val q = Some(
        MemoryPortParams(
          MasterPortParams(
            base = 0,
            size = 0x400000000L,
            beatBytes = 64,
            idBits = 6
          ),
          nMemoryChannels
        )
      )
      require(1 <= nMemoryChannels && nMemoryChannels <= 4)
      q
    // 16GB memory per DIMM
    case PlatformPhysicalMemoryBytes => (16L << 30) * nMemoryChannels
    case HasDMA => Some(2)
    // TODO this can be tuned
    case CXbarMaxDegree => 8
    case HasDiscreteMemory => true

    case PlatformTypeKey => PlatformType.FPGA
    case FrontBusProtocolKey => FrontBusProtocol.AXIL
    case FrontBusAddressMask => 0xffffL
    case FrontBusBaseAddress => 0L
    case FrontBusBeatBytes => 4
    case PlatformNURAM => 960
    case PlatformNBRAM => 2160
    case HasCoherence => None
    case PrefetchSourceMultiplicity => 32

    case CoreCommandLatency => 4
    case HasDisjointMemoryControllers => true

  })


private[composer] class U200_sole()
  extends Config((_, _, _) => {
    case PlatformSLRs =>
      Some(
        Seq(
          SLRName("0", frontBus = true),
          SLRName("1", memoryBus = true),
          SLRName("2")
        )
      )
    case PlatformPreferedSLRCmdRespRoutingPath => None
    case DefaultClockRateKey => 300
    case IsAWS => false
    case PostProcessorMacro => _: Config => ;
  })

class WithU200Platform extends Config(new U200Base(1) ++ new U200_sole)
