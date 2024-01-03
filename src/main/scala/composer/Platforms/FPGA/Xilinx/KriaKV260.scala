package composer.Platforms.FPGA.Xilinx

import chipsalliance.rocketchip.config.Config
import composer.Generation._
import composer._
import composer.Platforms._
import composer.Platforms.FPGA.PlatformSLRs
import freechips.rocketchip.amba.axi4.{AXI4MasterParameters, AXI4MasterPortParameters}
import freechips.rocketchip.subsystem._

class WithKriaPlatform(nMemoryChannels: Int = 1, clockRate_MHz: Int = 100)
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
    case FrontBusAddressBits => 40
    case HasDMA => None
    // TODO this can be tuned
    case CXbarMaxDegree => 8
    case HasDiscreteMemory => false
    case FrontBusBeatBytes => 4
    case CoreCommandLatency => 0
    case PrefetchSourceMultiplicity => 16

    case PlatformTypeKey => PlatformType.FPGA
    case FrontBusProtocolKey => FrontBusProtocol.AXI4
    case PlatformNBRAM => 144
    case PlatformNURAM => 64
    case DefaultClockRateKey => 100
    case PlatformNumSLRs => 1
    case PlatformSLRs => None
    case HasCoherence =>
      None
    //      Some(CoherenceConfiguration(MasterPortParams(
    //      base = 0,
    //      size = 1L << 44,
    //      beatBytes = 16,
    //      idBits = 6),
    //      64))
    case IsAWS => false
    case PostProcessorMacro => c: Config => {
      if (c(BuildModeKey) == BuildMode.Synthesis) {
        os.write.over(os.Path(ComposerBuild.composerGenDir) / "synth.tcl",
          composer.Platforms.FPGA.Xilinx.SynthScript(
            "composer",
            "output",
            "xck26-sfvc784-2LV-c",
            "xilinx.com:kv260_som:part0:1.4",
            clockRate_MHz.toString
          ))
      }
    }
    case HasDisjointMemoryControllers => false
  })
