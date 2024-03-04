package composer.Platforms.ASIC

import chipkit.{LazyComm, PROM_UART}
import chipsalliance.rocketchip.config._
import chisel3._
import composer.Generation.ComposerBuild
import composer.Platforms.PlatformType.PlatformType
import composer.Platforms._
import composer.Protocol.FrontBus.FrontBusProtocol
import freechips.rocketchip.amba.ahb._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.TLIdentityNode
import os.Path
import protocol.COMMTopIO

trait HasM0BasicInterfaces {
  val uart = IO(new PROM_UART)
}

abstract class M0Abstract(implicit p: Parameters) extends LazyModule {
  val node = AHBMasterSourceNode(
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
    moa.module.reset := withActiveHighReset.asBool
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

class ChipKitPlatform(m0generator: Parameters => M0Abstract,
                      val technologyLibrary: TechLib,
                      override val clockRateMHz: Int) extends Platform with HasPostProccessorScript with HasMemoryCompiler {
  override val platformType: PlatformType = PlatformType.ASIC
  override val hasDiscreteMemory: Boolean = true
  override val frontBusBaseAddress: Long = 0x400000L
  override val frontBusAddressNBits: Int = 32
  override val frontBusAddressMask: Long = 0x1FL
  override val frontBusBeatBytes: Int = 4
  override val frontBusCanDriveMemory: Boolean = true
  override val frontBusProtocol: FrontBusProtocol = new ChipkitFrontBusProtocol(m0generator)
  override val physicalMemoryBytes: Long = 1L << 22
  override val memorySpaceAddressBase: Long = 0x0
  override val memorySpaceSizeBytes: Long = physicalMemoryBytes
  override val memoryNChannels: Int = 1
  override val memoryControllersAreDisjoint: Boolean = true
  override val memoryControllerIDBits: Int = 4
  override val memoryControllerBeatBytes: Int = 4

  override val memoryCompiler: MemoryCompiler = technologyLibrary.memoryCompiler
  override def postProcessorMacro(c: Config, paths: Seq[Path]): Unit = technologyLibrary.postProcessorMacro(c, paths)
}