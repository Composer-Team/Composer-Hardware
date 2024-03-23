package composer.Platforms.ASIC

import chipkit.{LazyComm, PROM_UART, TLSlaveMux}
import chipsalliance.rocketchip.config._
import chisel3._
import composer.Generation.ComposerBuild
import composer.Platforms.PlatformType.PlatformType
import composer.Platforms._
import composer.Protocol.FrontBus.FrontBusProtocol
import freechips.rocketchip.amba.ahb._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.{TLIdentityNode, TLManagerNode, TLSlaveParameters, TLSlavePortParameters}
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

  val program_sram = TLManagerNode(
    portParams = Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = Seq(AddressSet(0, 0xFFFF)),
        supportsPutFull = TransferSizes(4),
        supportsGet = TransferSizes(4),
      )), beatBytes = 4, endSinkId = 0
    ))
  )

  override def module: LazyModuleImp with HasM0BasicInterfaces
}


class ChipkitFrontBusProtocol(generator: Parameters => M0Abstract) extends FrontBusProtocol {
  override def deriveTopIOs(tlChainObj: Any, withClock: Clock, withActiveHighReset: Reset)(implicit p: Parameters): Unit = {
    chipkit.sources foreach ComposerBuild.addSource
    val CHIP = IO(new COMMTopIO)
    val STDUART = IO(new PROM_UART)
    val (moa, lzc, _) = tlChainObj.asInstanceOf[(M0Abstract, LazyComm, Any)]
    lzc.module.top <> CHIP

//    conv.node.in(0)._1.hsel := conv.node.in(0)._1.htrans =/= 0.U
//    conv.node.in(0)._1.hready := true.B
//    lzc.M.out(0)._1.hready := true.B

//    tsm.module.slave_select := 0.U
//    dontTouch(tsm.module.slave_select)
    STDUART <> moa.module.uart
    moa.module.reset := withActiveHighReset.asBool
  }

  override def deriveTLSources(implicit p: Parameters): (Any, TLIdentityNode, Option[TLIdentityNode]) = {
    val chipKitCOMM = LazyModule(new LazyComm)
    val m0 = generator(p)
    println("ChipKitFrontBusProtocol: deriveTLSources")
//    val select = LazyModule(new TLSlaveMux())
//    select.in := AHBToTL() := chipKitCOMM.M
    val ahb2tl = LazyModule(new AHBToTL())
    m0.program_sram := ahb2tl.node := chipKitCOMM.M // select.out_sram
//    val tl_dma = TLIdentityNode()
//    tl_dma := select.out_dma

    val tl_node = TLIdentityNode()
    tl_node := AHBToTL() := m0.node
    ((m0, chipKitCOMM, ahb2tl), tl_node, None)
//      Some(tl_dma))
  }
}

class ChipKitPlatform(m0generator: Parameters => M0Abstract,
                      val technologyLibrary: TechLib,
                      override val clockRateMHz: Int) extends Platform with HasPostProccessorScript with HasMemoryCompiler {
  override val platformType: PlatformType = PlatformType.ASIC
  override val hasDiscreteMemory: Boolean = true
  override val frontBusBaseAddress: Long = 0x2000FC00L
  override val frontBusAddressNBits: Int = 32
  override val frontBusAddressMask: Long = 0x3FFL
  override val frontBusBeatBytes: Int = 4
  override val frontBusCanDriveMemory: Boolean = true
  override val frontBusProtocol: FrontBusProtocol = new ChipkitFrontBusProtocol(m0generator)
  override val physicalMemoryBytes: Long = 1L << 22
  override val memorySpaceAddressBase: Long = 0x0
  override val memorySpaceSizeBytes: Long = physicalMemoryBytes
  override val memoryNChannels: Int = 1
  override val memoryControllerIDBits: Int = 4
  override val memoryControllerBeatBytes: Int = 4

  override val memoryCompiler: MemoryCompiler = technologyLibrary.memoryCompiler
  override def postProcessorMacro(c: Config, paths: Seq[Path]): Unit = technologyLibrary.postProcessorMacro(c, paths)
}