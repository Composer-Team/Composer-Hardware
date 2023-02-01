package composer.MemoryStreams

import chisel3._
import chisel3.util._
import composer._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tilelink._

class ReadChannelIO(dataBytes: Int, vlen: Int)(implicit p: Parameters) extends Bundle {
  val req = Flipped(Decoupled(new ChannelTransactionBundle))
  val channel = new DataChannelIO(dataBytes, vlen)
  val busy = Output(Bool())
}

abstract class txEmitBehavior

case class txEmitAsOneTx() extends txEmitBehavior

case class txEmitCacheBlock() extends txEmitBehavior

class CReader(dataBytes: Int,
              vlen: Int = 1,
              prefetchRows: Int = 0,
              fetchBehavior: txEmitBehavior,
              tlclient: TLClientNode)(implicit p: Parameters) extends Module {
  val usesPrefetch = prefetchRows > 0
  val blockBytes = p(CacheBlockBytes)
  val (tl_outer, tledge) = tlclient.out(0)
  val addressBits = log2Up(tledge.manager.maxAddress)
  val maxBytes = dataBytes * vlen
  require(isPow2(maxBytes))


  // io goes to user, TL connects with AXI4
  val io = IO(new ReadChannelIO(dataBytes, vlen))
  val tl_out = IO(new TLBundle(tl_outer.params))
  val beatBytes = tledge.manager.beatBytes

  io.channel.finished := true.B
  val channelWidthBits = maxBytes * 8

  val beatsPerBlock = blockBytes / beatBytes
  val channelsPerBlock = blockBytes / maxBytes
  val channelsPerBeat = beatBytes / maxBytes

  val logChannelsPerBeat = log2Up(channelsPerBeat)
  val logChannelSize = log2Up(io.channel.data.bits.getWidth) - 3
  val logBlockBytes = log2Up(blockBytes)
  //  val logBeatBytes = log2Up(beatBytes)

  require(beatBytes >= maxBytes, "Size of channel cannot be wider than AXI bus. If this functionality is" +
    " necessary, please buffer your reads/writes")

  val addr = Reg(UInt(addressBits.W))
  val blockAddr = Cat(addr(addressBits - 1, log2Up(blockBytes)), 0.U(log2Up(blockBytes).W))
  val len = RegInit(0.U(addressBits.W))
  //  val lenBlocks = len >> logBlockBytes
  //  val lenBeats = len >> logBeatBytes

  val buffer_fill_level = RegInit(0.U((log2Up(beatsPerBlock) + 1).W))
  val data_channel_read_idx = RegInit(0.U((log2Up(channelsPerBlock) + 1).W))

  val s_idle :: s_send_mem_request :: s_read_memory :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val prefetch_readIdx, prefetch_writeIdx = Counter(prefetchRows)
  val prefetch_buffers = Seq.fill(beatsPerBlock)(SyncReadMem(prefetchRows, UInt((beatBytes * 8).W)))
  val prefetch_buffers_valid = Reg(Vec(prefetchRows, Bool()))
  val prefetch_head_buffer_valid = RegInit(false.B)

  val buffer = Seq.fill(beatsPerBlock, channelsPerBeat)(Reg(UInt(channelWidthBits.W)))
  when(reset.asBool) {
    prefetch_buffers_valid.foreach(_ := false.B)
  }
  val channel_buffer = Reg(Vec(vlen, UInt((dataBytes * 8).W)))
  val channel_buffer_valid = RegInit(false.B)

  io.channel.data.valid := channel_buffer_valid(0)
  io.channel.data.bits := channel_buffer
  tl_out.a.valid := false.B
  io.req.ready := len === 0.U && state === s_idle
  io.busy := state =/= s_idle

  // has to be pow2 to ensure OHToUInt works like we want
  assert(!(io.req.valid && ((io.req.bits.len & (io.req.bits.len - 1.U)).asUInt =/= 0.U)))
  tl_out.a.bits := tledge.Get(
    fromSource = 0.U,
    toAddress = blockAddr,
    lgSize = fetchBehavior match {
      case txEmitAsOneTx() =>
        OHToUInt(len) // log
      case txEmitCacheBlock() =>
        log2Up(blockBytes).U
    }
  )._2
  //////////////////////////////////////////////////////
  // handle data input

  // load a cache block at a time. If it's underfilled then we accept more beats
  tl_out.d.ready := (buffer_fill_level < beatsPerBlock.U) && !prefetch_buffers_valid(prefetch_writeIdx.value)
  // last buffer in FIFO gets D channel data
  when(tl_out.d.fire) {
    // IF we're using prefetch, then we store the entire block in BRAM before moving it to buffer
    // IF we're NOT using prefetch, we can stream in and out of the buffer
    if (usesPrefetch) {
      when(buffer_fill_level === (beatsPerBlock - 1).U) {
        prefetch_buffers_valid(prefetch_writeIdx.value) := true.B
        prefetch_writeIdx.inc()
        buffer_fill_level := 0.U
      }
      if (beatsPerBlock > 1) {
        (0 until beatsPerBlock) foreach { bufferBeatIdx =>
          when(bufferBeatIdx.U === buffer_fill_level) {
            prefetch_buffers(bufferBeatIdx).write(prefetch_writeIdx.value, tl_out.d.bits.data)
          }
        }
      } else prefetch_buffers(0).write(prefetch_writeIdx.value, tl_out.d.bits.data)

    } else {
      (0 until beatsPerBlock) foreach { bufferBeatIdx =>
        // whenever we get a beat on the data bus, split it into sections and put into buffer
        when(bufferBeatIdx.U === buffer_fill_level) {
          // get sub ranges of the beat
          val splits = (0 until channelsPerBeat) map { ch_buffer_idx =>
            val high = (ch_buffer_idx + 1) * channelWidthBits - 1
            val low = channelWidthBits * ch_buffer_idx
            tl_out.d.bits.data(high, low)
          }
          // assign them into buffer
          buffer(bufferBeatIdx) zip splits foreach { case (buf, spl) => buf := spl }
          // handle state machine for the case of prefetching
        }
      }
    }
    buffer_fill_level := buffer_fill_level + 1.U
  }

  if (usesPrefetch) {
    val l_idle :: l_assign :: Nil = Enum(2)
    val load_state = RegInit(l_idle)
    val do_read = WireInit(false.B)
    val loads = prefetch_buffers.map{ reader: chisel3.SyncReadMem[UInt] => reader.read(prefetch_readIdx.value, do_read) }


    switch(load_state) {
      is(l_idle) {
        when(!prefetch_head_buffer_valid && prefetch_buffers_valid(prefetch_readIdx.value)) {
          load_state := l_assign
          do_read := true.B
        }
      }
      is(l_assign) {
        val splits = (0 until beatsPerBlock) flatMap { bufferBeatIdx =>
          (0 until channelsPerBeat) map { ch_buffer_idx =>
            val high = (ch_buffer_idx + 1) * channelWidthBits - 1
            val low = channelWidthBits * ch_buffer_idx
            loads(bufferBeatIdx)(high, low)
          }
        }
        buffer.flatten.zip(splits).foreach { case (buff, load) => buff := load }
        prefetch_head_buffer_valid := true.B
        prefetch_buffers_valid(prefetch_readIdx.value) := false.B
        prefetch_readIdx.inc()
        load_state := l_idle
      }
    }
  }
  // end handle data input
  //////////////////////////////////////////////

  switch(state) {
    is(s_idle) {
      when(io.req.fire) {
        addr := io.req.bits.addr
        len := io.req.bits.len
        state := s_send_mem_request
        when(io.req.bits.len === 0.U) {
          state := s_idle
        }
      }
    }
    is(s_send_mem_request) {
      tl_out.a.valid := true.B
      when(tl_out.a.fire) {
        //        printf("tl_out a fire len(%d)", tl_out.a.bits.size)
        buffer_fill_level := 0.U
        if (logBlockBytes > logChannelSize) {
          data_channel_read_idx := addr(logBlockBytes - 1, logChannelSize)
        } else
          data_channel_read_idx := 0.U
        addr := addr + blockBytes.U
        state := s_read_memory
      }
    }
    // wait for responses from s_send_mem_request
    is(s_read_memory) {
      // when we get a message back store it into a buffer
      when(data_channel_read_idx === channelsPerBlock.U) {
        data_channel_read_idx := 0.U
        if (usesPrefetch) {
          // if prefetching then wait for the fetch buffer to be valid so that we can fill it in here
          // then it'll be loaded to the top at once
          prefetch_head_buffer_valid := false.B
        } else {
          // if no prefetching then this is also the loading buffer and we need to start filling it from the bottom
          buffer_fill_level := 0.U
        }
        when(len === 0.U) {
          state := s_idle
        }.otherwise {
          if (fetchBehavior.isInstanceOf[txEmitCacheBlock]) {
            state := s_send_mem_request
          } // if one transaction mode then the beats just keep coming
        }
      }
    }
  }


  val buffer_ch_idx = data_channel_read_idx(logChannelsPerBeat - 1, 0)
  val buffer_beat_idx = data_channel_read_idx(data_channel_read_idx.getWidth - 1, logChannelsPerBeat)
  when(
    if (usesPrefetch) prefetch_head_buffer_valid
    else buffer_beat_idx < buffer_fill_level) {
    // Vec kinda sucks in its current form. Very hard to do multidimensional arrays so we just use Seqs
    //  and `when` instead. Accomplishes the same thing but semantically has multiple-dimensions and is reasonable
    //  to read
    (0 until channelsPerBeat) foreach { ch_idx =>
      (0 until beatsPerBlock) foreach { beat_idx =>
        (0 until vlen) foreach { vidx =>
          when(ch_idx.U === buffer_ch_idx && beat_idx.U === buffer_beat_idx) {
            val start = vidx * dataBytes * 8
            val end = (vidx + 1) * dataBytes * 8 - 1
            channel_buffer(vidx) := buffer(beat_idx)(ch_idx)(end, start)
            channel_buffer_valid := true.B
          }
        }
      }
    }
  }

  when(io.channel.data.fire) {
    len := len - maxBytes.U
    data_channel_read_idx := data_channel_read_idx + 1.U
    channel_buffer_valid := false.B
  }
}