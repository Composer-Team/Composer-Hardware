package beethoven.Protocol.RoCC
import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import chipsalliance.rocketchip.config.Parameters
import chisel3.util.Queue
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}


class RoccBuffer(val nEntries: Int = 2)(implicit p: Parameters) extends LazyModule {
  val node = new RoCCBufferNode
  lazy val module = new LazyModuleImp(this) {
    val src = node.in(0)._1
    val dst = node.out(0)._1
    dst.req <> Queue(src.req, nEntries)
    src.resp <> Queue(dst.resp, nEntries)

  }
}

object RoccBuffer {
  def apply()(implicit p: Parameters): RoCCBufferNode = LazyModuleWithFloorplan(new RoccBuffer()).node
  def apply(name: String)(implicit p: Parameters): RoCCBufferNode = {
    LazyModuleWithFloorplan(new RoccBuffer(), name).node
  }

}