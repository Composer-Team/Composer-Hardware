package beethoven.MemoryStreams.Readers

import beethoven.{ChannelTransactionBundle, DataChannelIO}
import chipsalliance.rocketchip.config.Parameters
import chisel3.util.Decoupled
import chisel3.{Bundle, Flipped}

class ReadChannelIO(val dWidth: Int)(implicit p: Parameters) extends Bundle {
  val req = Flipped(Decoupled(new ChannelTransactionBundle))
  val channel = new DataChannelIO(dWidth)
}