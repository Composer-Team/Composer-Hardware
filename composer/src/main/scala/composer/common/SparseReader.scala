package composer.common

import composer._

import chisel3._
import chisel3.util._

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.config.Parameters

class SparseReadIO(addressBits: Int, maxBytes: Int) extends Bundle {
  val req = Flipped(new DataChannelIO((addressBits / 8) + 1))
  val channel = new DataChannelIO(maxBytes)
  val start = Flipped(Decoupled(Bool()))
  val busy = Output(Bool())
}

/**
 * Reads data from memory addressed by a stream of indices
 *
 * @param maxBytes the largest number of bytes per element the reader will produce
 */
class SparseReadChannel(maxBytes: Int)(implicit p: Parameters) extends LazyModule()(p) {

  val blockBytes = p(CacheBlockBytes)
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "SparseReadChannel",
    sourceId = IdRange(0, 1),
    supportsProbe = TransferSizes(1, blockBytes),
    supportsGet = TransferSizes(1, blockBytes)
  )))))

  lazy val module = new LazyModuleImp(this) {

    val (tl, edge) = node.out(0)
    //private val sizeBits = log2Ceil(maxBytes) + 1
    val addressBits = log2Up(edge.manager.maxAddress)

    require(isPow2(maxBytes))
    // TO-DO: addressBits is wrong here?
    val io = IO(new SparseReadIO(addressBits, maxBytes))

    val beatBytes = edge.manager.beatBytes

    val addr = Reg(UInt(addressBits.W))
    val beatAddr = Wire(UInt(addressBits.W))

    val buffer = Reg(UInt((beatBytes * 8).W))
    val buffer_pending = RegInit(false.B)

    val byte_offset = Reg(UInt((log2Ceil(beatBytes) + 1).W))
    //val size = Reg(UInt(sizeBits.W))
    //val stride = Reg(UInt(addressBits.W))
    //val block_stride = stride(addressBits - 1, tlBeatAddrBits + tlByteAddrBits)

    val s_idle :: s_ready :: s_acquire :: s_send :: s_finishing :: s_stopping :: Nil = Enum(6)
    val state = RegInit(s_idle)

    val finished = RegInit(false.B)

    val byte_pending = FillInterleaved(beatBytes, buffer_pending)
    val byte_mask = ((1 << (1 << log2Ceil(maxBytes))) - 1).U(maxBytes.W)
    val data_valid = !((byte_pending >> byte_offset).asUInt & byte_mask).orR
    val data_data = buffer.asUInt >> Cat(byte_offset, 0.U(3.W))
    val masked_data = data_data.asUInt & FillInterleaved(8, byte_mask)

    io.start.ready := state === s_idle
    when(io.start.fire) {
      state := s_ready
      finished := false.B
    }

    io.req.data.ready := state === s_ready

    // when a new address streams in:
    when(io.req.data.fire) {
      addr := io.req.data.bits
      byte_offset := io.req.data.bits(log2Ceil(beatBytes) - 1, 0)

      state := s_acquire

      val mask = (1.U << log2Ceil(maxBytes).U).asUInt - 1.U
      assert((io.req.data.bits & mask) === 0.U,
        "SparseReadChannel: unaligned address")
    }

    tl.a.valid := state === s_acquire
    beatAddr := Cat(addr(addressBits - 1, log2Ceil(beatBytes)),
      0.U(log2Ceil(beatBytes).W))
    tl.a.bits := edge.Get(fromSource = 0.U,
      toAddress = beatAddr,
      lgSize = log2Ceil(beatBytes).U)._2

    tl.d.ready := buffer_pending
    io.channel.data.valid := (state === s_send) && data_valid
    io.channel.data.bits := masked_data
    io.busy := state =/= s_idle

    when(tl.a.fire) {
      buffer_pending := true.B
      state := s_send
    }

    when(tl.d.fire) {
      buffer := tl.d.bits.data
      buffer_pending := false.B
    }

    when(io.channel.data.fire) {
      state := s_ready
    }
    when(io.req.finished && state =/= s_idle) {
      finished := true.B
    }
    when(finished && !buffer_pending && state === s_ready) {
      state := s_finishing
    }
    when(io.channel.stop && state =/= s_idle) {
      state := s_stopping
    }

    io.channel.finished := false.B
    when(state === s_finishing) {
      finished := false.B
      io.channel.finished := true.B
      state := s_idle
    }

    when(state === s_stopping && !buffer_pending) {
      state := s_idle
    }
  }

}
