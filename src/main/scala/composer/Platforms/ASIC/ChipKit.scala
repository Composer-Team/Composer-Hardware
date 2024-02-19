package composer.Platforms.ASIC

import chipkit.{LazyComm, PROM_UART}
import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import composer.Generation.ComposerBuild
import composer.Platforms.FPGA.PlatformSLRs
import composer._
import composer.Platforms._
import freechips.rocketchip.amba.ahb._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink.TLIdentityNode
import os.Path
import protocol.COMMTopIO

trait HasM0BasicInterfaces {
  val uart = IO(new PROM_UART)
}

abstract class M0Abstract(implicit p: Parameters) extends LazyModule {
  val node = AHBSlaveSourceNode(
    portParams = Seq(AHBMasterPortParameters(
      masters = Seq(AHBMasterParameters(
        name = "M0_AHB"
      )))))

  override def module: LazyModuleImp with HasM0BasicInterfaces
}


class ChipkitFrontBusProtocol(generator: Parameters => M0Abstract) extends FrontBusProtocol {
  override def deriveTopIOs(tlChainObj: Any, withClock: Clock, withActiveHighReset: Reset)(implicit p: Parameters): Bundle = {
    chipkit.sources foreach ComposerBuild.addSource
    val CHIP = IO(new COMMTopIO)
    val STDUART = IO(new PROM_UART)
    val (moa, lzc) = tlChainObj.asInstanceOf[(M0Abstract, LazyComm)]
    lzc.module.top <> CHIP
    STDUART <> moa.module.uart
    moa.module.reset := !withActiveHighReset.asBool
    CHIP
  }
  override def deriveTLSources(implicit p: Parameters): (Any, TLIdentityNode, Option[TLIdentityNode]) = {
    val chipKitCOMM = LazyModule(new LazyComm)
    val AHBMasterMux = LazyModule(new chipkit.AHBMasterMux(2))
    val m0 = generator(p)
    AHBMasterMux.node := chipKitCOMM.M
    AHBMasterMux.node := m0.node
    val tl_node = TLIdentityNode()
    tl_node := AHBToTL() := AHBMasterMux.node
    ((m0, chipKitCOMM), tl_node, None)
  }
}

class WithChipKitPlatform(m0generator: Parameters => M0Abstract,
                          useFPGAMem: Boolean = false) extends Config((_, _, _) => {
  case ExtMem =>
    Some(
      MemoryPortParams(
        MasterPortParams(
          base = 0,
          size = 1L << 22,
          beatBytes = 4,
          idBits = 6
        ), 1
      )
    )
  // 4GB total physical memory
  case PlatformPhysicalMemoryBytes => 1L << 22
  case FrontBusBaseAddress => 0x400000L
  case FrontBusAddressMask => 0x1fL
  case FrontBusCanDriveMemory => true
  case FrontBusAddressBits => 16
  case PrefetchSourceMultiplicity => 16
  case HasDMA => None
  case CXbarMaxDegree => 8
  case HasDiscreteMemory => false
  case FrontBusBeatBytes => 4
  case CoreCommandLatency => 0
  case PlatformTypeKey => if (useFPGAMem) PlatformType.FPGA else PlatformType.ASIC
  case FrontBusProtocolKey => new ChipkitFrontBusProtocol(m0generator)
  case PlatformNumSLRs => 1
  case PlatformSLRs => None
  case DefaultClockRateKey => 100
  case HasCoherence => None

  case IsAWS => false
  case HasDisjointMemoryControllers => false
})

class WithForceFPGA extends Config((_, _, _) => {
  case PlatformNURAM => 10000
  case PlatformNBRAM => 10000
})