package beethoven.Protocol.tilelink.TLSlave

import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import chisel3._

class TLToTLSlave()(implicit p: Parameters) extends LazyModule {
  val node = TLToTLSlaveNode()
  lazy val module = new LazyModuleImp(this) {
    require(node.in.size == 1)
    require(node.out.size == 1)
    val in = node.in.head._1
    val out = node.out.head._1
    out.tl.valid := in.a.valid
    out.tl.bits := in.a.bits
    in.a.ready := true.B

    in.d.valid := RegNext(in.a.valid)
    in.d.bits := node.in(0)._2.AccessAck(RegNext(in.a.bits))
  }
}

object TLToTLSlave {
  private var tl_to_tl_slave_idx = 0
  def apply()(implicit p: Parameters): TLToTLSlaveNode = LazyModuleWithFloorplan(new TLToTLSlave(), {
    val id = tl_to_tl_slave_idx
    tl_to_tl_slave_idx += 1
    s"zztl_to_tl_slave_$id"
  }).node

  def apply(name: String)(implicit p: Parameters): TLToTLSlaveNode = LazyModuleWithFloorplan(new TLToTLSlave(), name).node
}
