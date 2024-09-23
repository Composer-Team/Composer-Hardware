package beethoven.Protocol.RoCC

import beethoven.Floorplanning.LazyModuleWithSLRs
import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import beethoven.Protocol.RoCC.RoccCompositeXbar.named_idx
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

class RoccCompositeXbar(implicit p: Parameters) extends LazyModule {
  LazyModuleWithSLRs.freezeSLRPush = true
  val fanin = RoccFanin(f"1fanIn_${named_idx}")
  val buffer = RoccBuffer(s"2buff_${named_idx}")
  val fanout = RoccFanout(f"3fanOut_${named_idx}")
  named_idx += 1
  fanout := buffer := fanin
  val node = RoccCompositeNexusNode(fanin, fanout)
  lazy val module = new LazyModuleImp(this) {}
  LazyModuleWithSLRs.freezeSLRPush = false
}

object RoccCompositeXbar {
  private var named_idx = 0
  private var rocc_composite_xbar_idx = 0
  def apply()(implicit p: Parameters): RoccCompositeNexusNode = LazyModuleWithFloorplan(new RoccCompositeXbar(), {
    val id = rocc_composite_xbar_idx
    rocc_composite_xbar_idx += 1
    s"rocc_composite_xbar_$id"
  }).node
  def apply(name: String)(implicit p: Parameters): RoccCompositeNexusNode = LazyModuleWithFloorplan(new RoccCompositeXbar(), name).node
}
