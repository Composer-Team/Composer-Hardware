package beethoven.Protocol.tilelink.TLSlave

import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import beethoven.common.ShiftReg
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

class TLSlaveBufferedBroadcast(depth: Int = 2)(implicit p: Parameters) extends LazyModule {
  val node = TLSlaveBroadcastNode()
  val module = new LazyModuleImp(this) {
    require(node.in.size == 1)
    val in = node.in.head._1
    node.out.foreach { case (out, _) =>
      out.tl.valid := ShiftReg(in.tl.valid, depth, clock, allow_fpga_shreg = false, as = _.asBool)
      out.tl.bits := ShiftReg(in.tl.bits, depth, clock, allow_fpga_shreg = false, as = _.asBool)
    }
  }
}

object TLSlaveBufferedBroadcast {
  private var tl_slave_buffered_broadcast_idx = 0
  def apply(depth: Int = 2)(implicit p: Parameters): TLSlaveBroadcastNode = {
    LazyModuleWithFloorplan(new TLSlaveBufferedBroadcast(depth), {
      val id = tl_slave_buffered_broadcast_idx
      tl_slave_buffered_broadcast_idx += 1
      s"zztl_slave_buffered_broadcast_$id"
    }).node
  }

  def apply(name: String, depth: Int)(implicit p: Parameters): TLSlaveBroadcastNode = {
    LazyModuleWithFloorplan(new TLSlaveBufferedBroadcast(depth), name).node
  }
}
