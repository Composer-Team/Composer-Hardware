package composer.Platforms.FPGA

import chipsalliance.rocketchip.config.{Field, Parameters}
import composer.Platforms.{PlatformPreferedSLRCmdRespRoutingPath, PlatformType, PlatformTypeKey}

case class SLRName(
                    name: String,
                    frontBus: Boolean = false,
                    memoryBus: Boolean = false
                  )
// implementation specifics
case object PlatformSLRs extends Field[Option[Seq[SLRName]]]

object SLRHelper {
  private[composer] val SLRRespRoutingFanout = 5
  private[composer] val SLRCmdRoutingFanout = 5
  private[composer] val CmdEndpointsPerSLR = 1
  private[composer] val RespEndpointsPerSLR = 2

  final val DEFAULT_SLR = 0
  final def getFrontBusSLR(implicit p: Parameters): Int = {
    if (p(PlatformTypeKey) != PlatformType.FPGA) return DEFAULT_SLR
    if (p(PlatformSLRs).isEmpty) return DEFAULT_SLR
    val slr = p(PlatformSLRs).get.zipWithIndex.find(_._1.frontBus)
    slr.getOrElse((SLRName("0"), 0))._2
  }
  final def getMemoryBusSLR(implicit p: Parameters): Int = {
    if (p(PlatformTypeKey) != PlatformType.FPGA) return DEFAULT_SLR
    if (p(PlatformSLRs).isEmpty) return DEFAULT_SLR
    val slr = p(PlatformSLRs).get.zipWithIndex.find(_._1.memoryBus)
    slr.getOrElse((SLRName("0"), 0))._2
  }
  final def getSLRFromName(slrName: String)(implicit p: Parameters): Int = {
    val slr = p(PlatformSLRs).get.zipWithIndex.find(_._1.name == slrName)
    require(slr.isDefined)
    slr.get._2
  }
  final def getSLRFromIdx(idx: Int)(implicit p: Parameters): String = {
    p(PlatformSLRs) match {
      case None => ""
      case Some(a) => a(idx).name
    }
  }

  final def getCmdRespPath()(implicit p: Parameters): Option[Seq[Int]] = {
    if (p(PlatformTypeKey) != PlatformType.FPGA) return None
    p(PlatformPreferedSLRCmdRespRoutingPath) match {
      case None => None
      case Some(path) =>
        val path_conv = path.map(p(PlatformSLRs).get.map(_.name).indexOf(_))
        require(path_conv.forall(_ >= 0))
        Some(path_conv)
    }
  }
}
