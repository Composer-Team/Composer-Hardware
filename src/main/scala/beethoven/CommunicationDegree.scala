package beethoven

object CommunicationDegree extends Enumeration {
  val PointToPoint, BroadcastAllCores, BroadcastAllCoresChannels, BroadcastAllChannels = Value
  type CommunicationDegree = Value
}
