package composer.RoccHelpers

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.{RoCCCommand, RoCCResponse, TileVisibilityNodeKey}

class AXILHub(implicit p: Parameters) extends LazyModule {
//  val axil_aggregator = LazyModule(new AXILAggregator())
  // Widget contains MMIO stuff
  val axil_widget = LazyModule(new AXILWidget()(p))
  val node = AXI4IdentityNode() // ocl-port
//  val mem_out = AXI4IdentityNode()

//  mem_out := axil_aggregator.node
  axil_widget.node := node
  lazy val module = new AXILHubModule(this)(p.alterPartial{
    case TileVisibilityNodeKey => axil_widget.throughId
  })
}

class AXILHubModule(outer: AXILHub)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val rocc_in = Decoupled(new RoCCCommand)
    val rocc_out = Flipped(Decoupled(new RoCCResponse))
  })
  val axil_widget = outer.axil_widget.module

  val axil_to_rocc = Module(new AXILToRocc)
  val rocc_to_axil = Module(new RoccToAXIL)

  axil_widget.io.resp <> rocc_to_axil.io.out
  rocc_to_axil.io.rocc <> io.rocc_out
  axil_to_rocc.io.in <> axil_widget.io.cmds
  io.rocc_in <> axil_to_rocc.io.rocc

}
