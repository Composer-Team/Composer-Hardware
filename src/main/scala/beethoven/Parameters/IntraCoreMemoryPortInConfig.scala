package beethoven.Parameters

import beethoven.MemoryStreams.MemChannelConfig
import beethoven.Parameters.IntraCoreMemoryPortInConfig.IntraCoreCommunicationDegree
import beethoven.Parameters.IntraCoreMemoryPortInConfig.IntraCoreCommunicationDegree.{BroadcastAllChannels, BroadcastAllCores, IntraCoreCommunicationDegree, PointToPoint}
import chisel3.util._

object IntraCoreMemoryPortInConfig {
  object IntraCoreCommunicationDegree extends Enumeration {
    val PointToPoint, BroadcastAllCores, BroadcastAllCoresChannels, BroadcastAllChannels = Value
    type IntraCoreCommunicationDegree = Value
  }
  def has_core_select(deg: IntraCoreCommunicationDegree): Boolean = deg == PointToPoint || deg == BroadcastAllChannels
  def has_channel_select(deg: IntraCoreCommunicationDegree): Boolean = deg == PointToPoint || deg == BroadcastAllCores
}

case class IntraCoreMemoryPortInConfig(name: String,
                                       nChannels: Int,
                                       portsPerChannel: Int,
                                       dataWidthBits: Int,
                                       nDatas: Int,
                                       communicationDegree: IntraCoreCommunicationDegree.IntraCoreCommunicationDegree,
                                       readOnly: Boolean = false,
                                       latency: Number = 2) extends MemChannelConfig {
  require(isPow2(dataWidthBits) && dataWidthBits >= 8, "the width of CIntraCoreMemory ports is" +
    "currently restricted to power-of-2 sizes. If you need this changed, please contact developer.")
}

