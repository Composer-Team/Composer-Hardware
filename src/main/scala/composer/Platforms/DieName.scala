package composer.Platforms

import chipsalliance.rocketchip.config._

case class DieName(name: String,
                   frontBus: Boolean = false,
                   memoryBus: Boolean = false)
// implementation specifics

object DieName {
  private[composer] val SLRRespRoutingFanout = 5
  private[composer] val SLRCmdRoutingFanout = 5
  private[composer] val CmdEndpointsPerSLR = 1
  private[composer] val RespEndpointsPerSLR = 2

  final val DEFAULT_SLR = 0

  def mdp()(implicit p: Parameters): MultiDiePlatform = p(PlatformKey).asInstanceOf[MultiDiePlatform]

  final def getFrontBusSLR(implicit p: Parameters): Int = {
    if (!p(PlatformKey).isInstanceOf[MultiDiePlatform]) return DEFAULT_SLR
    val slr = mdp.platformDies.zipWithIndex.find(_._1.frontBus)
    slr.getOrElse((DieName("0"), 0))._2
  }

  final def getMemoryBusSLR(implicit p: Parameters): Int = {
    if (!p(PlatformKey).isInstanceOf[MultiDiePlatform]) return DEFAULT_SLR
    val slr = mdp.platformDies.zipWithIndex.find(_._1.memoryBus)
    slr.getOrElse((DieName("0"), 0))._2
  }

  final def getSLRFromIdx(idx: Int)(implicit p: Parameters): String = {
    if (!p(PlatformKey).isInstanceOf[MultiDiePlatform]) ""
    else mdp().platformDies(idx).name
  }

  final def getCmdRespPath()(implicit p: Parameters): Option[Seq[Int]] = {
    if (!p(PlatformKey).isInstanceOf[MultiDiePlatform]) return None
    val path_conv = mdp.platformPreferedDieCmdRespRoutingPath.map(mdp.platformDies.map(_.name).indexOf(_))
    require(path_conv.forall(_ >= 0))
    Some(path_conv)
  }
}
