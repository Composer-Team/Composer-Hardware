package beethoven.Protocol.tilelink

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLEdgeIn, TLIdentityNode}

class TLSupportCheckerImp(outer: TLSupportChecker) extends LazyModuleImp(outer) {
  outer.node.in.foreach(a => require(outer.check(a._2),
    f"Check failed (id: ${outer.identifier}): Get ${a._2.master.allSupportGet}, Put ${a._2.master.allSupportPutFull}"))
//  println(s"All checks passed in ${outer.identifier}")
}

class TLSupportChecker(val check: TLEdgeIn => Boolean, val identifier: String)(implicit p: Parameters) extends LazyModule {
  val node = TLIdentityNode()
  lazy val module = new TLSupportCheckerImp(this)
}

object TLSupportChecker {
  def apply(check: TLEdgeIn => Boolean, identifier: String = "")(implicit p: Parameters): TLIdentityNode = {
    val checker = LazyModule(new TLSupportChecker(check, identifier))
    checker.node
  }

  def writeCheck(identifier: String)(implicit p: Parameters): TLIdentityNode =
    TLSupportChecker(a => a.master.allSupportPutFull.max > 0 && a.master.allSupportGet.max == 0, identifier)

  def readCheck(identifier: String)(implicit p: Parameters): TLIdentityNode =
    TLSupportChecker(a => a.master.allSupportGet.max > 0 && a.master.allSupportPutFull.max == 0, identifier)

}
