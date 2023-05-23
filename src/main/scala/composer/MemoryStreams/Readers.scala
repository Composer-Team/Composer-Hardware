package composer.MemoryStreams

import chisel3._
import chisel3.util._
import composer._
import chipsalliance.rocketchip.config._
import composer.Systems.DataChannelIO
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tilelink._

class ReadChannelIO(dataBytes: Int, vlen: Int)(implicit p: Parameters) extends Bundle {
  val req = Flipped(Decoupled(new ChannelTransactionBundle))
  val channel = new DataChannelIO(dataBytes, vlen)
  val busy = Output(Bool())
}

class CReader(dataBytes: Int,
              vlen: Int = 1,
              tlclient: TLClientNode)(implicit p: Parameters) extends Module {
  val prefetchRows = tlclient.portParams(0).endSourceId
  require(prefetchRows > 0)
  val usesPrefetch = prefetchRows > 1
  val (tl_outer, tledge) = tlclient.out(0)
  val beatBytes = tledge.manager.beatBytes
  val addressBits = log2Up(tledge.manager.maxAddress)
  val maxBytes = dataBytes * vlen
  require(isPow2(maxBytes))


  // io goes to user, TL connects with AXI4
  val io = IO(new ReadChannelIO(dataBytes, vlen))
  val tl_out = IO(new TLBundle(tl_outer.params))

  val channelWidthBits = maxBytes * 8

  val channelsPerBeat = beatBytes / maxBytes

  val logChannelSize = log2Up(io.channel.data.bits.getWidth) - 3
  val logBeatBytes = log2Up(beatBytes)

  require(beatBytes >= maxBytes, "Size of channel cannot be wider than AXI bus. If this functionality is" +
    " necessary, please buffer your reads/writes")

  val addr = Reg(UInt(addressBits.W))
  val blockAddr = Cat(addr(addressBits - 1, logBeatBytes), 0.U(logBeatBytes.W))
  val len = RegInit(0.U(addressBits.W))
  //  val lenBlocks = len >> logBlockBytes
  //  val lenBeats = len >> logBeatBytes

  val data_channel_read_idx = RegInit(0.U((log2Up(channelsPerBeat) + 1).W))

  val s_idle :: s_send_mem_request :: s_read_memory :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val buffer = Seq.fill(channelsPerBeat)(Reg(UInt(channelWidthBits.W)))
  val channel_buffer = Reg(Vec(vlen, UInt((dataBytes * 8).W)))
  val channel_buffer_valid = RegInit(false.B)

  io.channel.data.valid := channel_buffer_valid(0)
  io.channel.data.bits := channel_buffer
  tl_out.a.valid := false.B
  io.req.ready := len === 0.U && state === s_idle
  io.busy := state =/= s_idle

  // has to be pow2 to ensure OHToUInt works like we want

  val nSources = tlclient.portParams(0).endSourceId
  if (nSources - 1 > prefetchRows) {
    println(s"CReader was parameterized with maxInFlightTxs($nSources), but only prefetches $prefetchRows rows." +
      s" Consider increasing the number of prefetched rows to at least $nSources or decrease the number of sources." +
      s" Having more source bits than necessary may increase the resource utilization of your design.")
  }
  val sourceWire = Wire(UInt(log2Up(nSources).W))
  sourceWire := 0.U

  //////////////////////////////////////////////////////
  // handle data input

  // load a cache block at a time. If it's underfilled then we accept more beats
  // last buffer in FIFO gets D channel data
  // IF we're using prefetch, then we store the entire block in BRAM before moving it to buffer
  // IF we're NOT using prefetch, we can stream in and out of the buffer
  val sourceIdleBits = Reg(Vec(nSources, Bool()))
  when(reset.asBool) {
    sourceIdleBits.foreach(_ := true.B)
  }
  val sourceAvailable = sourceIdleBits.reduce(_ || _)
  val atLeastOneSourceActive = sourceIdleBits.map(!_).reduce(_ || _)
  val chosenSource = PriorityEncoder(sourceIdleBits)

  io.channel.in_progress := state =/= s_idle

  assert(!(io.req.valid && ((io.req.bits.len & (io.req.bits.len - 1.U)).asUInt =/= 0.U)))
  tl_out.a.bits := tledge.Get(
    fromSource = chosenSource,
    toAddress = blockAddr,
    lgSize = log2Up(beatBytes).U
  )._2
  if (usesPrefetch) {

    val prefetch_readIdx, prefetch_writeIdx = Counter(prefetchRows)

    val prefetch_blatency = 3
    val prefetch_buffers = Module(new CMemory(prefetch_blatency - 2, beatBytes * 8, prefetchRows))
    prefetch_buffers.io.clk := clock
    prefetch_buffers.io.rst := reset
    prefetch_buffers.io.r_addr := DontCare
    prefetch_buffers.io.r_mem_en := false.B
    prefetch_buffers.io.r_regce := true.B
    prefetch_buffers.io.w_addr := DontCare
    prefetch_buffers.io.w_mem_en := 0.U
    prefetch_buffers.io.w_din := DontCare

    val prefetch_buffers_valid = Reg(Vec(prefetchRows, Bool()))
    val prefetch_head_buffer_valid = RegInit(false.B)

    when(reset.asBool) {
      prefetch_buffers_valid.foreach(_ := false.B)
    }

    val l_idle :: l_assign :: Nil = Enum(2)
    val load_state = RegInit(l_idle)

    tl_out.d.ready := atLeastOneSourceActive

    val sourceToIdx = Reg(Vec(nSources, UInt(log2Up(prefetchRows).W)))

    when(tl_out.d.fire) {
      val dSource = tl_out.d.bits.source
      val prefetchIdx = sourceToIdx(dSource)
      prefetch_buffers_valid(prefetchIdx) := true.B
      sourceIdleBits(dSource) := true.B
      prefetch_buffers.io.w_mem_en := true.B
      prefetch_buffers.io.w_addr := prefetchIdx
      prefetch_buffers.io.w_din := tl_out.d.bits.data
    }

    val readCycleCounter = Counter(prefetch_blatency+4)
    switch(load_state) {
      is(l_idle) {
        when(!prefetch_head_buffer_valid && prefetch_buffers_valid(prefetch_readIdx.value)) {
          load_state := l_assign
          prefetch_buffers.io.r_addr := prefetch_readIdx.value
          prefetch_buffers.io.r_mem_en := true.B
          readCycleCounter.reset()
        }
      }
      is(l_assign) {
        readCycleCounter.inc()
        when(readCycleCounter.value === (prefetch_blatency-1).U) {
          (0 until channelsPerBeat) foreach { ch_buffer_idx =>
            val high = (ch_buffer_idx + 1) * channelWidthBits - 1
            val low = channelWidthBits * ch_buffer_idx
            buffer(ch_buffer_idx) := prefetch_buffers.io.r_dout(high, low)
          }
          prefetch_head_buffer_valid := true.B
          prefetch_buffers_valid(prefetch_readIdx.value) := false.B
          prefetch_readIdx.inc()
          load_state := l_idle
        }
      }
    }

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
        tl_out.a.valid := sourceAvailable && !prefetch_buffers_valid(prefetch_writeIdx.value)
        when(tl_out.a.fire) {
          addr := addr + beatBytes.U
          len := len - beatBytes.U

          state := s_read_memory
          sourceToIdx(chosenSource) := prefetch_writeIdx.value
          sourceIdleBits(chosenSource) := false.B
          prefetch_writeIdx.inc()
        }
      }
      // wait for responses from s_send_mem_request
      is(s_read_memory) {
        // when we get a message back store it into a buffer
        when(len === 0.U) {
          state := s_idle
        }.otherwise {
          state := s_send_mem_request
        }
      }
    }

    when(prefetch_head_buffer_valid) {
      channel_buffer_valid := true.B
      for (vidx <- 0 until vlen) {
        // gather all of the bit subsets that will ever correspond to this vector element
        val selection = (0 until channelsPerBeat) map { c_idx =>
          val cat = Cat(buffer.reverse)
          val off = c_idx * maxBytes * 8
          val start = off + vidx * dataBytes * 8
          val end = off + (vidx + 1) * dataBytes * 8
          cat(end - 1, start)
        }
        channel_buffer(vidx) := VecInit(selection)(data_channel_read_idx)
      }
    }
    when(io.channel.data.fire) {
      data_channel_read_idx := data_channel_read_idx + 1.U
      channel_buffer_valid := false.B
      when(data_channel_read_idx === (channelsPerBeat-1).U) {
        data_channel_read_idx := 0.U
        prefetch_head_buffer_valid := false.B
      }
    }
  } else { // else no prefetch
    // else if no prefetch
    val bufferValid = RegInit(false.B)

    tl_out.d.ready := !bufferValid
    when(tl_out.d.fire) {
      len := len - beatBytes.U
      // whenever we get a beat on the data bus, split it into sections and put into buffer
        // get sub ranges of the beat
      val splits = (0 until channelsPerBeat) map { ch_buffer_idx =>
        val high = (ch_buffer_idx + 1) * channelWidthBits - 1
        val low = channelWidthBits * ch_buffer_idx
        tl_out.d.bits.data(high, low)
      }

      // assign them into buffer
      buffer zip splits foreach { case (buf, spl) => buf := spl }
    }

    switch(state) {
      is(s_idle) {
        when(io.req.fire) {
          addr := io.req.bits.addr
          len := io.req.bits.len
          state := s_send_mem_request
          assert(io.req.bits.len =/= 0.U, "Do not request 0 length transactions.")
        }
      }
      is(s_send_mem_request) {
        tl_out.a.valid := true.B
        when(tl_out.a.fire) {
          if (logBeatBytes > logChannelSize) {
            data_channel_read_idx := addr(logBeatBytes - 1, logChannelSize)
          } else
            data_channel_read_idx := 0.U
          addr := addr + beatBytes.U
          state := s_read_memory
        }
      }
      // wait for responses from s_send_mem_request
      is(s_read_memory) {
        // when we get a message back store it into a buffer
        when(data_channel_read_idx === channelsPerBeat.U) {
          when(len === 0.U) {
            state := s_idle
            len := 0.U
          }.otherwise {
            state := s_send_mem_request
          }
        }
      }
    }

    when(bufferValid) {
      channel_buffer_valid := true.B
      for (vidx <- 0 until vlen) {
        // gather all of the bit subsets that will ever correspond to this vector element
        val selection = (0 until channelsPerBeat) map { c_idx =>
          val cat = Cat(buffer.reverse)
          val off = c_idx * maxBytes * 8
          val start = off + vidx * dataBytes * 8
          val end = off + (vidx + 1) * dataBytes * 8
          cat(end - 1, start)
        }
        channel_buffer(vidx) := VecInit(selection)(data_channel_read_idx)
      }
    }
    when(io.channel.data.fire) {
      when (data_channel_read_idx === (channelsPerBeat-1).U) {
        bufferValid := false.B
        data_channel_read_idx := 0.U
      }.otherwise {
        data_channel_read_idx := data_channel_read_idx + 1.U
      }
      channel_buffer_valid := false.B
    }
  }
}