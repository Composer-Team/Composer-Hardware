package composer.TLManagement

import chipsalliance.rocketchip.config._
import composer.Platforms.PlatformKey
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.tilelink._

//noinspection ScalaFileName
object makeTLMultilayerXbar {

  def apply(in: Seq[TLNode], out: Seq[TLNode])(implicit p: Parameters): Unit = {
    val xbarLim = p(PlatformKey).xbarMaxDegree
    def recursivelyGroupToLim[T <: TLNode](
        group: Seq[T],
        isClient: Boolean
    ): Seq[TLNode] = {
      if (group.length <= xbarLim) group
      else {
        val subGroup = group.grouped(xbarLim)
        val subX = subGroup.map { g =>
          val xb = LazyModule(new TLXbar())

          // put buffer AFTER xbar
          if (isClient) {
            g foreach (xb.node := TLBuffer() := _)
          } else {
            g foreach (_ := TLBuffer() := xb.node)
          }
          xb.node
        }
        subX.toSeq
      }
    }
    val inReduce = recursivelyGroupToLim(in, isClient = true)
    val outReduce = recursivelyGroupToLim(out, isClient = false)
    inReduce foreach (i => outReduce.foreach(o => o := i))
  }
}
