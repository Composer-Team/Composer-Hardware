package beethoven.Parameters

import beethoven.MemoryStreams.MemChannelConfig

/**
 * Write channel group
 *
 * @param name               The name of the channel
 * @param nChannels          number of memory access channels of this type
 * @param maxInFlightTxs     maximum number of AXI/TileLink memory transactions can be inflight per writer module at
 *                           once
 * @param bufferSizeBytesMin The minimum size (in bytes) of the write buffer. This ensures that data can be enqueued
 *                           without stalls until this limit
 */
case class WriteChannelConfig(name: String,
                              dataBytes: Int,
                              nChannels: Int = 1,
                              maxInFlightTxs: Option[Int] = None,
                              bufferSizeBytesMin: Option[Int] = None) extends MemChannelConfig {
}
