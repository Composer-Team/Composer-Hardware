package composer.MemoryStreams
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.MaxChannelTransactionLenKey

class ChannelTransactionBundle(addressBits: Int)(implicit p: Parameters) extends Bundle{
  val addr = UInt(addressBits.W)
  val len = UInt(log2Up(p(MaxChannelTransactionLenKey)).W)
}

object MemoryChannelClass extends Enumeration {
  type MemoryChannelClass = Value
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
}
