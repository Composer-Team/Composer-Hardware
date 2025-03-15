package beethoven.Protocol.RoCC.Helpers

import beethoven.Protocol.RoCC.{RoccClientNode, RoccMasterParams}
import beethoven.platform
import chisel3._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.amba.axi4.AXI4IdentityNode
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.TLIdentityNode

class FrontBusHub(implicit p: Parameters) extends LazyModule {

  val widget = LazyModule(new FrontBusWidget())

//  val tl_in = TLIdentityNode()
//  widget.node := tl_in

  val axi_in = AXI4IdentityNode()
  widget.node := axi_in

  val rocc_out = RoccClientNode(RoccMasterParams())

  lazy val module = new AXILHubModule(this)(p.alterPartial {
    case TileVisibilityNodeKey => axi_in
  })
}

class AXILHubModule(outer: FrontBusHub)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val axil_widget = outer.widget.module

  val axil_to_rocc = Module(new AXILToRocc)
  val rocc_to_axil = Module(new RoccToAXIL)

  val io = IO(new Bundle {
    val cache_prot = if (platform.hasDebugAXICACHEPROT) Some(Output(UInt(7.W))) else None
  })
  if (io.cache_prot.isDefined) io.cache_prot.get := axil_widget.io.cache_prot.get

  axil_widget.io.resp <> rocc_to_axil.io.out
  rocc_to_axil.io.rocc <> outer.rocc_out.out(0)._1.resp
  axil_to_rocc.io.in <> axil_widget.io.cmds

  val rocc_out = outer.rocc_out.out(0)._1.req
  val rocc_cmd = axil_to_rocc.io.rocc
  rocc_out.valid := rocc_cmd.valid
  rocc_cmd.ready := rocc_out.ready
  rocc_out.bits := rocc_cmd.bits
}
