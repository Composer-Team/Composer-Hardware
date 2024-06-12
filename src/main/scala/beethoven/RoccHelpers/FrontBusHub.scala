package beethoven.RoccHelpers

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import beethoven.common.AccelRoccResponse
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.TLIdentityNode

class FrontBusHub(implicit p: Parameters) extends LazyModule {
//  val axil_aggregator = LazyModule(new AXILAggregator())
  // Widget contains MMIO stuff

  val widget = LazyModule(new FrontBusWidget())

  val tl_head = TLIdentityNode()
  widget.node := tl_head

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
