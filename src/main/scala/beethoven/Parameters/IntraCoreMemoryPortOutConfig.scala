package beethoven.Parameters

import beethoven.MemoryStreams.MemChannelConfig
import beethoven.Parameters.IntraCoreCommunicationDegree.IntraCoreCommunicationDegree

object IntraCoreCommunicationDegree extends Enumeration {
  val PointToPoint, BroadcastAllCores, BroadcastAllCoresChannels = Value
  type IntraCoreCommunicationDegree = Value
}

case class IntraCoreMemoryPortOutConfig(name: String,
                                        toSystem: String,
                                        toMemoryPort: String,
                                        commStyle: IntraCoreCommunicationDegree) extends MemChannelConfig {
  // this has to be a bit special because this port is coupled to another existing port declared somewhere else
  override val nChannels: Int = -1
}
