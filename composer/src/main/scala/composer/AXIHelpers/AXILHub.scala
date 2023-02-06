package composer.AXIHelpers

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.{RoCCCommand, RoCCResponse}

class AXILHub(implicit p: Parameters) extends LazyModule {
  val axil_aggregator = LazyModule(new AXILAggregator())
  // Widget contains MMIO stuff
  val axil_widget = LazyModule(new AXILWidget()(p))
  val node = AXI4IdentityNode() // ocl-port
  val mem_out = AXI4IdentityNode()

  mem_out := axil_aggregator.node
  axil_widget.node := node
  lazy val module = new AXILHubModule(this)
}

class AXILHubModule(outer: AXILHub)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val rocc_in = Decoupled(new RoCCCommand)
    val rocc_out = Flipped(Decoupled(new RoCCResponse))
  })
  val axil_aggregator = outer.axil_aggregator.module
  val axil_widget = outer.axil_widget.module

  val axil_rocc_converter = Module(new AXILRoccConverter)
  val rocc_axil_converter = Module(new RoccAXILConverter)

  axil_widget.io.resp <> rocc_axil_converter.io.out
  rocc_axil_converter.io.rocc <> io.rocc_out
  axil_rocc_converter.io.in <> axil_widget.io.cmds
  io.rocc_in <> axil_rocc_converter.io.rocc

  axil_aggregator.io.write_in <> axil_widget.io.mem_in
  axil_aggregator.io.read_in <> axil_widget.io.read_addrs
  axil_widget.io.mem_out <> axil_aggregator.io.read_out

}
