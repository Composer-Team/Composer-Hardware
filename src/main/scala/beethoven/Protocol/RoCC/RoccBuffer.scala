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
  private var rocc_buffer_idx = 0
  def apply()(implicit p: Parameters): RoCCBufferNode = LazyModuleWithFloorplan(new RoccBuffer(), {
    val id = rocc_buffer_idx
    rocc_buffer_idx += 1
    s"rocc_buffer_$id"
  }).node
  def apply(name: String)(implicit p: Parameters): RoCCBufferNode = {
    LazyModuleWithFloorplan(new RoccBuffer(), name).node
  }

}