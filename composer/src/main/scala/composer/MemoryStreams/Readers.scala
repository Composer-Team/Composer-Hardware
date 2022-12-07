package composer.MemoryStreams

import chisel3._
import chisel3.util._
import composer._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tilelink._

class ReadChannelIO(addressBits: Int, dataBytes: Int, vlen: Int)(implicit p: Parameters) extends Bundle {
  val req = Flipped(Decoupled(new ChannelTransactionBundle(addressBits)))
  val channel = new DataChannelIO(dataBytes, vlen)
  val busy = Output(Bool())
}

class SequentialReader(dataBytes: Int, tlparams: TLBundleParameters, tledge: TLEdgeOut, vlen: Int = 1)(implicit p: Parameters) extends Module {
  val blockBytes = p(CacheBlockBytes)
  val addressBits = log2Up(tledge.manager.maxAddress)
  val maxBytes = dataBytes * vlen
  require(isPow2(maxBytes))

  // io goes to user, TL connects with AXI4
  val io = IO(new ReadChannelIO(addressBits, dataBytes, vlen))
  val tl = IO(new TLBundle(tlparams))

  val beatBytes = tledge.manager.beatBytes

  io.channel.finished := true.B
  val channelWidthBits = maxBytes * 8
//  val logBeatBytes = log2Up(beatBytes)
  val logChannelSize = log2Up(io.channel.data.bits.getWidth) - 3
  val logBlockBytes = log2Up(blockBytes)
  val beatsPerBlock = blockBytes / beatBytes
  val channelsPerBlock = blockBytes / maxBytes
  val channelsPerBeat = beatBytes / maxBytes
  val logChannelsPerBeat = log2Up(channelsPerBeat)
  require(beatBytes >= maxBytes, "Size of channel cannot be wider than AXI bus. If this functionality is" +
  " necessary, please buffer your reads/writes")

  val addr = Reg(UInt(addressBits.W))
  val blockAddr = Cat(addr(addressBits-1, log2Up(blockBytes)), 0.U(log2Up(blockBytes).W))
  val len = Reg(UInt((addressBits - logBlockBytes).W))

  val buffer = Seq.fill(channelsPerBeat)(Reg(Vec(beatsPerBlock, UInt(channelWidthBits.W))))

  val tl_idx = RegInit(0.U((log2Up(beatsPerBlock)+1).W))
  val ch_idx = RegInit(0.U((log2Up(channelsPerBlock)+1).W))

  val s_idle :: s_send_mem_request :: s_read_memory :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val channel_buffer = Reg(Vec(vlen, UInt((dataBytes * 8).W)))
  val channel_buffer_valid = RegInit(false.B)

  io.channel.data.valid := channel_buffer_valid
  io.channel.data.bits := channel_buffer
  tl.a.valid := false.B
  tl.d.ready := state === s_read_memory && tl_idx < beatsPerBlock.U
  io.req.ready := state === s_idle
  io.busy := state =/= s_idle

  tl.a.bits := tledge.Get(fromSource = 0.U,
    toAddress = blockAddr,
    lgSize = log2Ceil(blockBytes).U)._2

  switch(state) {
    is(s_idle) {
      when(io.req.fire) {
        addr := io.req.bits.addr
        len := io.req.bits.len >> logBlockBytes
        state := s_send_mem_request

        val mask = (1.U << log2Ceil(maxBytes).U).asUInt - 1.U
        //val mask = (1.U << io.req.bits.size) - 1.U
        assert((io.req.bits.addr & mask) === 0.U, "ColumnReadChannel: unaligned address")
        when(io.req.bits.len === 0.U) {
          state := s_idle
        }
      }
    }
    is(s_send_mem_request) {
      tl.a.valid := true.B
      when(tl.a.fire) {
        tl_idx := 0.U
        if (logBlockBytes > logChannelSize) {
          ch_idx := addr(logBlockBytes - 1, logChannelSize)
        } else
          ch_idx := 0.U
        addr := addr + blockBytes.U
        state := s_read_memory
        len := len - 1.U
      }
    }
    // wait for responses from s_send_mem_request
    is (s_read_memory) {
      // when we get a message back store it into a buffer
      when(tl.d.fire) {
        (0 until channelsPerBeat) foreach { ch_buffer_idx =>
          val high = (ch_buffer_idx+1) * channelWidthBits - 1
          val low = channelWidthBits * ch_buffer_idx
          buffer(ch_buffer_idx)(tl_idx) := tl.d.bits.data(high, low)
        }
        tl_idx := tl_idx + 1.U
      }
      when(ch_idx === channelsPerBlock.U) {
        when(len === 0.U) {
          state := s_idle
        }.otherwise {
          state := s_send_mem_request
        }
      }
    }
  }

  when((ch_idx >> logChannelsPerBeat).asUInt < tl_idx && state === s_read_memory && !channel_buffer_valid) {
    (0 until channelsPerBeat) foreach { ch =>
      when (ch.U === ch_idx(logChannelsPerBeat-1, 0)) {
        (0 until vlen) foreach {vidx =>
          val start = vidx * dataBytes * 8
          val end = (vidx + 1) * dataBytes * 8 - 1
          channel_buffer(vidx) := buffer(ch)((ch_idx >> logChannelsPerBeat).asUInt)(end, start)
        }
        channel_buffer_valid := true.B
      }
    }
  }
  when (io.channel.data.fire) {
    ch_idx := ch_idx + 1.U
    channel_buffer_valid := false.B
  }
}