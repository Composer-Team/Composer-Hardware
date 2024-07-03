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
  def apply()(implicit p: Parameters): RoccCompositeNexusNode = LazyModuleWithFloorplan(new RoccCompositeXbar()).node
  def apply(name: String)(implicit p: Parameters): RoccCompositeNexusNode = LazyModuleWithFloorplan(new RoccCompositeXbar()).node
}
