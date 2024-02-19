package composer.Platforms

import chipkit.{LazyComm, PROM_UART}
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import composer.Platforms.ASIC.M0Abstract
import composer.Protocol.AXI4Compat
import freechips.rocketchip.amba.ahb.{AHBMasterSourceNode, AHBToTL}
import freechips.rocketchip.amba.axi4.{AXI4Fragmenter, AXI4IdIndexer, AXI4MasterNode, AXI4MasterParameters, AXI4MasterPortParameters, AXI4ToTL, AXI4UserYanker}
import freechips.rocketchip.diplomacy.{IdRange, LazyModule}
import freechips.rocketchip.subsystem.MasterPortParams
import freechips.rocketchip.tilelink.TLIdentityNode
import protocol.COMMTopIO


abstract class FrontBusProtocol {
  /**
   * tlChainObj provides the diplomacy objects generated in `deriveTLSources` so that you, from a non-lazy context
   * can generate the physical IOs and tie them to the diplomacy object bundle IOs
   */
  def deriveTopIOs(tlChainObj: Any, withClock: Clock, withActiveHighReset: Reset)(implicit p: Parameters): Bundle

  /**
   * This function is executed from the lazy context. Generate the following:
   * 1. the top-level protocol nodes of your choice. These will be `Any`-typed and passed to `deriveTopIOs` to be
   *    tied-off.
   * 2. A `TLIdentityNode` with a driver. In the simple case, it is directly driven by the aforementioned protocol
   *    nodes. The generated node (2) is used to drive the front-bus modules and receive commands to the accelerator
   *    system.
   * 3. Optionally, there may be DMA from the front-bus-associated modules, so those can be exposed here as well
   *    in the TileLink format.
   */
  def deriveTLSources(implicit p:Parameters) : (Any, TLIdentityNode, Option[TLIdentityNode])
}

class AXIFrontBusProtocol extends FrontBusProtocol {
  override def deriveTopIOs(tlChainObj: Any, withClock: Clock, withActiveHighReset: Reset)(implicit p: Parameters): Bundle = {
    val port_cast = tlChainObj.asInstanceOf[AXI4MasterNode]
    val ap = port_cast.out(0)._1.params
    val S00_AXI = IO(Flipped(new AXI4Compat(MasterPortParams(
      base = 0,
      size = 1L << p(FrontBusAddressBits),
      beatBytes = ap.dataBits / 8,
      idBits = ap.idBits))))
    AXI4Compat.connectCompatSlave(S00_AXI, port_cast.out(0)._1)
    S00_AXI
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
