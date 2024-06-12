package beethoven.Protocol.FrontBus

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import beethoven.Platforms._
import beethoven.Protocol.AXI4Compat
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy.IdRange
import freechips.rocketchip.subsystem.MasterPortParams
import freechips.rocketchip.tilelink.TLIdentityNode

class AXIFrontBusProtocol extends FrontBusProtocol {
  override def deriveTopIOs(tlChainObj: Any, withClock: Clock, withActiveHighReset: Reset)(implicit p: Parameters): Unit = {
    val port_cast = tlChainObj.asInstanceOf[AXI4MasterNode]
    val ap = port_cast.out(0)._1.params
    val S00_AXI = IO(Flipped(new AXI4Compat(MasterPortParams(
      base = 0,
      size = 1L << p(PlatformKey).frontBusAddressNBits,
      beatBytes = ap.dataBits / 8,
      idBits = ap.idBits))))
    AXI4Compat.connectCompatSlave(S00_AXI, port_cast.out(0)._1)
  }

  override def deriveTLSources(implicit p: Parameters): (Any, TLIdentityNode, Option[TLIdentityNode]) = {
    val node = TLIdentityNode()
    val axi_master = AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "S00_AXI",
        aligned = true,
        maxFlight = Some(1),
        id = IdRange(0, 1 << 16)
      )),
    )))
    node :=  AXI4ToTL() :=  AXI4UserYanker(capMaxFlight = Some(4)) := AXI4Fragmenter() := AXI4IdIndexer(1) := axi_master
    (axi_master, node, None)
  }
}

