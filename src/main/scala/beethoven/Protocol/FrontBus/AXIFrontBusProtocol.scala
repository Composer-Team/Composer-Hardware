package beethoven.Protocol.FrontBus

import beethoven.Floorplanning.DeviceContext
import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import beethoven.Generation.DotGen
import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import beethoven.Platforms._
import beethoven.Protocol.AXI.{AXI4Compat, LongAXI4ToTL}
import beethoven.Protocol.RoCC.Helpers.FrontBusHub
import beethoven.Protocol.RoCC._
import beethoven.Systems.make_tl_buffer
import beethoven.platform
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.MasterPortParams
import freechips.rocketchip.tilelink.{TLBuffer, TLIdentityNode}

class AXIFrontBusProtocol(withDMA: Boolean) extends FrontBusProtocol {
  override def deriveTopIOs(tlChainObj: Any, withClock: Clock, withActiveHighReset: Reset)(implicit p: Parameters): Unit = {
    val (port_cast, dma_cast) = tlChainObj.asInstanceOf[(AXI4MasterNode, Option[AXI4MasterNode])]
    val ap = port_cast.out(0)._1.params
    val S00_AXI = IO(Flipped(new AXI4Compat(MasterPortParams(
      base = 0,
      size = 1L << p(PlatformKey).frontBusAddressNBits,
      beatBytes = ap.dataBits / 8,
      idBits = ap.idBits))))
    AXI4Compat.connectCompatSlave(S00_AXI, port_cast.out(0)._1)

    if (withDMA) {
      val dma = IO(Flipped(new AXI4Compat(MasterPortParams(
        base = 0,
        size = platform.extMem.master.size,
        beatBytes = dma_cast.get.out(0)._1.r.bits.data.getWidth/8,
        idBits = 6))))
      AXI4Compat.connectCompatSlave(dma, dma_cast.get.out(0)._1)
    }

  }

  override def deriveTLSources(implicit p: Parameters): Config = {
    val frontInterfaceID = platform.physicalInterfaces.find(_.isInstanceOf[PhysicalHostInterface]).get.locationDeviceID

    DeviceContext.withDevice(frontInterfaceID) {
      val axi_master = AXI4MasterNode(Seq(AXI4MasterPortParameters(
        masters = Seq(AXI4MasterParameters(
          name = "S00_AXI",
          aligned = true,
          maxFlight = Some(1),
          id = IdRange(0, 1 << 16)
        )),
      )))
      val fronthub =
        DeviceContext.withDevice(frontInterfaceID) {
          val fronthub = LazyModuleWithFloorplan(new FrontBusHub(), "zzfront6_axifronthub")
          fronthub.axi_in := axi_master
          fronthub
        }


      val (dma_node, dma_front) = if (withDMA) {
        val node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
          masters = Seq(AXI4MasterParameters(
            name = "S01_AXI",
            maxFlight = Some(1),
            aligned = true,
            id = IdRange(0, 1 << 6)
          ))
        )))
        val dma2tl = TLIdentityNode()
        DeviceContext.withDevice(frontInterfaceID) {
          dma2tl :=
            make_tl_buffer() :=
            LazyModuleWithFloorplan(new LongAXI4ToTL(64)).node :=
            AXI4UserYanker(capMaxFlight = Some(1)) :=
            AXI4IdIndexer(1) :=
            AXI4Buffer() := node
        }
        (Some(node), Some(dma2tl))
      } else (None, None)

      val rocc_xb = DeviceContext.withDevice(frontInterfaceID) { RoccFanout("zzfront_7roccout") }

      rocc_xb := fronthub.rocc_out
      new Config((_, _, _) => {
        case OptionalPassKey => (axi_master, dma_node)
        case RoccNodeKey => rocc_xb
        case DMANodeKey => dma_front
        case DebugCacheProtSignalKey => fronthub.module.io.cache_prot
      })
    }
  }
}

