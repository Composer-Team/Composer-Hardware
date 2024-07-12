package beethoven.Protocol.RoCC

import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

class RoccCompositeXbar(implicit p: Parameters) extends LazyModule {
  val fanin = RoccFanin(f"1fanIn")
  val buffer = RoccBuffer(s"2buff")
  val fanout = RoccFanout(f"3fanOut")
  fanout := buffer := fanin
  val node = RoccCompositeNexusNode(fanin, fanout)
  lazy val module = new LazyModuleImp(this) {}
}

object RoccCompositeXbar {
  private var rocc_composite_xbar_idx = 0
  def apply()(implicit p: Parameters): RoccCompositeNexusNode = LazyModuleWithFloorplan(new RoccCompositeXbar(), {
    val id = rocc_composite_xbar_idx
    rocc_composite_xbar_idx += 1
    s"rocc_composite_xbar_$id"
  }).node
  def apply(name: String)(implicit p: Parameters): RoccCompositeNexusNode = LazyModuleWithFloorplan(new RoccCompositeXbar(), name).node
}
