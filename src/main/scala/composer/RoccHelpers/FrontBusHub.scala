package composer.RoccHelpers

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import chipsalliance.rocketchip.config._
import composer._
import composer.common.AccelRoccResponse
import composer.Platforms.{FrontBusProtocol, FrontBusProtocolKey}
import freechips.rocketchip.amba.ahb.{AHBMasterIdentityNode, AHBSlaveIdentityNode, AHBToTL}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.{RoCCCommand, RoCCResponse, TileVisibilityNodeKey}
import freechips.rocketchip.tilelink.TLIdentityNode

class FrontBusHub(implicit p: Parameters) extends LazyModule {
//  val axil_aggregator = LazyModule(new AXILAggregator())
  // Widget contains MMIO stuff

  val widget = LazyModule(new FrontBusWidget())

  val tl_head = TLIdentityNode()

  val node = p(FrontBusProtocolKey) match {
    case FrontBusProtocol.AXI4 | FrontBusProtocol.AXIL =>
      val node = AXI4IdentityNode()
      // NOTE: need fragmenter because AXI4_2_TL requires a very restrictive AXI format
      // NOTE2: NEED fragmenter because other platforms (e.g., Kria) can emit unpredictable transaction lengths
      //        for MMIO peeks. We witnessed 64b reads on a 32b pointer dereference. Fragmenter splits it up
      widget.node := tl_head := AXI4ToTL() :=  AXI4UserYanker(capMaxFlight = Some(4)) := AXI4Fragmenter() := AXI4IdIndexer(1) := node
      node
    case FrontBusProtocol.AHB =>
      val node = AHBSlaveIdentityNode()
      widget.node := tl_head :=  AHBToTL() := node
      node
  }

  lazy val module = new AXILHubModule(this)(p.alterPartial {
    case TileVisibilityNodeKey => tl_head
  })
}

class AXILHubModule(outer: FrontBusHub)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val rocc_in = Decoupled(new RoCCCommand)
    val rocc_out = Flipped(Decoupled(new AccelRoccResponse))
  })
  val axil_widget = outer.widget.module

  val axil_to_rocc = Module(new AXILToRocc)
  val rocc_to_axil = Module(new RoccToAXIL)

  axil_widget.io.resp <> rocc_to_axil.io.out
  rocc_to_axil.io.rocc <> io.rocc_out
  axil_to_rocc.io.in <> axil_widget.io.cmds
  io.rocc_in <> axil_to_rocc.io.rocc

}
