package beethoven.Platforms

import beethoven.platform
import chipsalliance.rocketchip.config._

case class DeviceConfig(identifier: Int,
                        name: String)
// implementation specifics

object DeviceConfig {

  final def getFrontBusSLR(implicit p: Parameters): Int = {
    platform.physicalInterfaces.find(_.isInstanceOf[PhysicalHostInterface]).get.locationDeviceID
  }

  final def getMemoryBusSLR(implicit p: Parameters): Seq[PhysicalMemoryInterface] = {
    platform.physicalInterfaces.filter(_.isInstanceOf[PhysicalMemoryInterface]).map(_.asInstanceOf[PhysicalMemoryInterface])
  }

  final def getDeviceNameFromID(idx: Int)(implicit p: Parameters): String = {
    platform.physicalDevices(idx).name
  }

//  final def getCmdRespPath()(implicit p: Parameters): Option[Seq[Int]] = {
//    if (!p(PlatformKey).isInstanceOf[MultiDiePlatform]) return None
//    val path_conv = mdp.dieConnectivity.map(mdp.platformDevices.map(_.name).indexOf(_))
//    require(path_conv.forall(_ >= 0))
//    Some(path_conv)
//  }
}
