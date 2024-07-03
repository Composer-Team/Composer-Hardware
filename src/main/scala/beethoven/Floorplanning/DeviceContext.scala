package beethoven.Floorplanning

private[beethoven] object DeviceContext {
  var currentDevice: Option[Int] = None

  def withDevice[T](deviceID: Int)(block: => T): T = {
    val current_parent = currentDevice
    currentDevice = Some(deviceID)
    val res = block
    currentDevice = current_parent
    res
  }
}
