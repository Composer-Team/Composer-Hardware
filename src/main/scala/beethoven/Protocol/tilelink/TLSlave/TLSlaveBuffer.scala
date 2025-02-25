package beethoven.Protocol.tilelink.TLSlave

import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import beethoven.common.ShiftReg
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AdapterNode, LazyModule, LazyModuleImp}

class TLSlaveBuffer(depth: Int = 1)(implicit p: Parameters) extends LazyModule {
  val node = new AdapterNode(TLSlaveOnlyImp)(dFn = a => a, uFn = a => a.v1copy(minLatency = a.minLatency + depth))
  lazy val module = new LazyModuleImp(this) {
    require(node.in.size == 1)
    require(node.out.size == 1)
    val in = node.in.head._1
    node.out(0)._1.tl.valid := ShiftReg(in.tl.valid, depth, clock, allow_fpga_shreg = false, as = a => a.asBool)
    node.out(0)._1.tl.bits := ShiftReg(in.tl.bits, depth, clock, allow_fpga_shreg = false, as = a => a.asBool)
  }
}

object TLSlaveBuffer {
  private var tl_slave_buffer_idx = 0
  def apply(depth: Int = 2)(implicit p: Parameters): TLSlaveNode = {
    LazyModuleWithFloorplan(new TLSlaveBuffer(depth), {
      val id = tl_slave_buffer_idx
      tl_slave_buffer_idx += 1
      s"zztl_slave_buffer_$id"
    }).node
  }

  def apply(name: String, depth: Int)(implicit p: Parameters): TLSlaveNode = {
    LazyModuleWithFloorplan(new TLSlaveBuffer(depth), name).node
  }
}
