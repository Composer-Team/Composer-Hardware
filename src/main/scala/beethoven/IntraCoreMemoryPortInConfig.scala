package beethoven

import beethoven.CommunicationDegree.{CommunicationDegree, _}
import chisel3.util._

object IntraCoreMemoryPortInConfig {

  def has_core_select(deg: CommunicationDegree): Boolean = deg == PointToPoint || deg == BroadcastAllChannels

  def has_channel_select(deg: CommunicationDegree): Boolean = deg == PointToPoint || deg == BroadcastAllCores
}

case class IntraCoreMemoryPortInConfig(name: String,
                                  nChannels: Int,
                                  portsPerChannel: Int,
                                  dataWidthBits: Int,
                                  nDatas: Int,
                                  communicationDegree: CommunicationDegree,
                                  readOnly: Boolean = false,
                                  latency: Number = 2) extends MemChannelConfig {
  require(isPow2(dataWidthBits) && dataWidthBits >= 8, "the width of CIntraCoreMemory ports is" +
    "currently restricted to power-of-2 sizes. If you need this changed, please contact developer.")
}

