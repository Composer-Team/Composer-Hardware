package beethoven.MemoryStreams.Readers

import freechips.rocketchip.tilelink.TLBundle

private[beethoven] trait ReaderModuleIO {
  val io: ReadChannelIO
  val tl_out: TLBundle
}
