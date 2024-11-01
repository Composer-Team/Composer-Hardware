package beethoven


/**
 * Read Channel group
 *
 * @param name           The name of the channel
 * @param nChannels      number of memory access channels of this type
 * @param maxInFlightTxs maximum number of AXI/TileLink memory transactions can be inflight per reader module at once
 */


case class ReadChannelConfig(name: String,
                             dataBytes: Int,
                             nChannels: Int = 1,
                             maxInFlightTxs: Option[Int] = None,
                             bufferSizeBytesMin: Option[Int] = None) extends MemChannelConfig {
  require(maxInFlightTxs.getOrElse(1) > 0, s"Max In Flight Transactions must be greater than 0. Got: $maxInFlightTxs")
}
