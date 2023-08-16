package composer.MemoryStreams

import chipsalliance.rocketchip.config._
import chisel3.{Reg, _}
import chisel3.util._
import composer.PrefetchSourceMultiplicity
import composer.Systems.DataChannelIO
import composer.common.{CLog2Up, ShiftReg}
import composer.Generation.{BuildMode}
import composer.Generation.Tune._
import composer.Platforms.{ASICMemoryCompilerKey, BuildModeKey, PlatformType, PlatformTypeKey}
import freechips.rocketchip.tilelink._

class ReadChannelIO(dataBytes: Int, vlen: Int)(implicit p: Parameters) extends Bundle {
  val req = Flipped(Decoupled(new ChannelTransactionBundle))
  val channel = new DataChannelIO(dataBytes, vlen)
}

object CReader {
  private var has_warned_dp: Boolean = false
}

class CReader(dataBytes: Int,
              vlen: Int = 1,
              tlclient: TLClientNode,
              debugName: Option[String] = None)(implicit p: Parameters) extends Module {
  val (tl_outer, tledge) = tlclient.out(0)
  val beatBytes = tledge.manager.beatBytes
  val beatBits = beatBytes * 8
  val addressBits = log2Up(tledge.manager.maxAddress)
  val maxBytes = dataBytes * vlen
  val largeTxNBeats = Math.max(p(PrefetchSourceMultiplicity), maxBytes / beatBytes)
  val prefetchRows = tlclient.portParams(0).endSourceId * p(PrefetchSourceMultiplicity)
  val hasOneSource = tlclient.portParams(0).endSourceId == 1
  require(isPow2(maxBytes))


  // io goes to user, TL connects with AXI4
  val io = IO(new ReadChannelIO(dataBytes, vlen))
  val tl_out = IO(new TLBundle(tl_outer.params))
  val tl_reg = Module(new Queue(new TLBundleA(tl_out.params), 2, false, false, false))
  tl_out.a <> tl_reg.io.deq

  val channelWidthBits = maxBytes * 8
  val storedDataWidthBytes = Math.max(beatBytes, dataBytes)
  val storedDataWidth = storedDataWidthBytes * 8
  val channelsPerStorage = storedDataWidthBytes / maxBytes

  val addr = Reg(UInt(addressBits.W))
  val len = RegInit(0.U(addressBits.W))

  val data_channel_read_idx = RegInit(0.U((log2Up(channelsPerStorage) + 1).W))

  val s_idle :: s_send_mem_request :: s_read_memory :: Nil = Enum(3)
  val state = RegInit(s_idle)

  //  val buffer = Seq.fill(channelsPerBeat)(Reg(UInt(channelWidthBits.W)))
  val channel_buffer = Wire(Vec(vlen, UInt((dataBytes * 8).W)))
  val channel_buffer_valid = Wire(Bool())

  io.channel.data.valid := channel_buffer_valid
  io.channel.data.bits := channel_buffer

  tl_reg.io.enq.valid := false.B
  tl_reg.io.enq.bits := DontCare

  // has to be pow2 to ensure OHToUInt works like we want

  val nSources = tlclient.portParams(0).endSourceId
  if (nSources - 1 > prefetchRows) {
    println(s"CReader was parameterized with maxInFlightTxs($nSources), but only prefetches $prefetchRows rows." +
      s" Consider increasing the number of prefetched rows to at least $nSources or decrease the number of sources." +
      s" Having more source bits than necessary may increase the resource utilization of your design.")
  }
  val sourceWire = Wire(UInt(log2Up(nSources).W))
  sourceWire := 0.U

  val sourceIdleBits = Reg(Vec(nSources, Bool()))
  when(reset.asBool) {
    sourceIdleBits.foreach(_ := true.B)
  }
  val sourceAvailable = sourceIdleBits.reduce(_ || _)
  val atLeastOneSourceActive = sourceIdleBits.map(!_).reduce(_ || _)
  val chosenSource = PriorityEncoder(sourceIdleBits)

  io.channel.in_progress := state =/= s_idle

  val largestRead = beatBytes * largeTxNBeats
  val lgLargestRead = log2Up(largestRead)


  val prefetch_readIdx, prefetch_writeIdx = RegInit(0.U(log2Up(prefetchRows).W))

  val prefetch_blatency = p(PlatformTypeKey) match {
    case PlatformType.FPGA => 3
    case PlatformType.ASIC => (prefetchRows.toFloat / 256).ceil.toInt + 1
  }

  val hasDualPortMemory = p(PlatformTypeKey) match {
    case PlatformType.FPGA => true
    case PlatformType.ASIC => p(ASICMemoryCompilerKey).mems.exists(_._1 == 2)
  }

  if (!hasDualPortMemory && !CReader.has_warned_dp) {
    CReader.has_warned_dp = true
    System.err.println("Warning: CReader is using a single port memory. This may cause performance degradation.")
  }
  val prefetch_buffers_valid = Reg(Vec(prefetchRows, Bool()))
  println("prefetch rows; " + prefetchRows)
  val sourceToIdx = Reg(Vec(nSources, UInt(log2Up(prefetchRows).W)))
  val beatsRemaining = Reg(Vec(nSources, UInt(log2Up(largeTxNBeats).W)))

  when(reset.asBool) {
    prefetch_buffers_valid.foreach(_ := false.B)
  }
  val prefetch_buffers = CMemory(prefetch_blatency, storedDataWidth, prefetchRows, 0, 0, if (hasDualPortMemory) 2 else 1)

  val (write_ready, read_ready, write_port_idx) = if (hasDualPortMemory) {
    prefetch_buffers.clock := clock.asBool
    prefetch_buffers.addr(0) := prefetch_readIdx
    prefetch_buffers.data_in(0) := DontCare
    prefetch_buffers.write_enable(0) := false.B
    prefetch_buffers.read_enable(0) := true.B
    prefetch_buffers.chip_select(0) := false.B

    prefetch_buffers.chip_select(1) := false.B
    prefetch_buffers.read_enable(1) := false.B
    prefetch_buffers.write_enable(1) := true.B
    prefetch_buffers.data_in(1) := DontCare
    prefetch_buffers.addr(1) := DontCare
    (true.B, true.B, 1)
  } else {
    prefetch_buffers.clock := clock.asBool
    val fillLevel = Reg(UInt(log2Up(prefetchRows).W))
    when (tl_out.d.fire) {
      fillLevel := fillLevel + 1.U
    }
    prefetch_buffers.addr(0) := prefetch_readIdx
    prefetch_buffers.data_in(0) := DontCare
    prefetch_buffers.write_enable(0) := false.B
    prefetch_buffers.read_enable(0) := true.B
    prefetch_buffers.chip_select(0) := false.B
    (true.B, !tl_out.d.valid, 0)
  }

  tl_out.d.ready := atLeastOneSourceActive && write_ready
  val slots_per_alloc = if (beatBytes >= maxBytes) {
    when(tl_out.d.fire) {
      val dSource = tl_out.d.bits.source
      val prefetchIdx = sourceToIdx(dSource)

      prefetch_buffers.data_in(write_port_idx) := tl_out.d.bits.data
      prefetch_buffers.addr(write_port_idx) := prefetchIdx
      prefetch_buffers.chip_select(write_port_idx) := true.B

      sourceToIdx(dSource) := sourceToIdx(dSource) + 1.U
      prefetch_buffers_valid(prefetchIdx) := true.B
      when(beatsRemaining(dSource) === 0.U) {
        sourceIdleBits(dSource) := true.B
      }
      beatsRemaining(dSource) := beatsRemaining(dSource) - 1.U
    }
    largeTxNBeats
  } else {
    val beatsPerLine = maxBytes / beatBytes
    val beatBufferPerSource = Reg(Vec(nSources, Vec(beatsPerLine - 1, UInt(beatBits.W))))
    val perSourceBeatProgress = Reg(Vec(nSources, UInt(log2Up(beatsPerLine).W)))
    when(tl_reg.io.enq.fire) {
      assert(tl_reg.io.enq.bits.size >= log2Up(maxBytes).U,
        "requested length is smaller than a single channel beat. This behavior is disallowed." +
          "If this is absolutely necessary, contact developer.")
      perSourceBeatProgress(chosenSource) := 0.U
    }
    when(tl_out.d.fire) {
      val src = tl_out.d.bits.source
      val whole_buff = beatBufferPerSource(src)
      whole_buff(perSourceBeatProgress(src)) := tl_out.d.bits.data
      beatsRemaining(src) := beatsRemaining(src) - 1.U
      when(beatsRemaining(src) === 0.U) {
        sourceIdleBits(src) := true.B
      }

      perSourceBeatProgress(src) := perSourceBeatProgress(src) + 1.U
      when(perSourceBeatProgress(src) === (beatsPerLine - 1).U) {
        perSourceBeatProgress(src) := 0.U

        prefetch_buffers_valid(sourceToIdx(src)) := true.B
        sourceToIdx(src) := sourceToIdx(src) + 1.U

        prefetch_buffers.addr(write_port_idx) := sourceToIdx(src)
        prefetch_buffers.data_in(write_port_idx) := Cat(tl_out.d.bits.data, Cat(beatBufferPerSource(src).reverse))
        prefetch_buffers.chip_select(write_port_idx) := true.B

      }
    }
    largeTxNBeats / beatsPerLine
  }

  switch(state) {
    is(s_idle) {
      when(io.req.fire) {
        addr := io.req.bits.addr
        len := io.req.bits.len
        assert(io.req.bits.len(log2Up(beatBytes)-1, 0) === 0.U,
          f"The provided length is not aligned to the data bus size. Please align to $beatBytes.\n" +
          f"Just make sure that you always flush through the data when you're done to make the reader usable again.")
        state := s_send_mem_request
        prefetch_writeIdx := 0.U
        prefetch_readIdx := 0.U
        when(io.req.bits.len === 0.U) {
          state := s_idle
        }
      }
    }
    is(s_send_mem_request) {
      when(len < largestRead.U) {
        tl_reg.io.enq.valid := sourceAvailable && ((prefetch_writeIdx + 1.U) =/= prefetch_readIdx)
        tl_reg.io.enq.bits := tledge.Get(
          fromSource = chosenSource,
          toAddress = addr,
          lgSize = CLog2Up(beatBytes).U
        )._2
        when(tl_reg.io.enq.fire) {
          addr := addr + beatBytes.U
          len := len - beatBytes.U
          prefetch_writeIdx := prefetch_writeIdx + 1.U
          beatsRemaining(chosenSource) := 0.U
        }
      }.otherwise {
        val haveRoomToAlloc = Wire(Bool())
        val dist2Roof = (prefetchRows - 1).U - prefetch_writeIdx
        if (hasOneSource) {
          haveRoomToAlloc := prefetch_readIdx === prefetch_writeIdx
        } else {
          when(dist2Roof < slots_per_alloc.U) {
            haveRoomToAlloc := prefetch_readIdx > (slots_per_alloc.U - dist2Roof) && (prefetch_readIdx < prefetch_writeIdx)
          }.otherwise {
            haveRoomToAlloc := (prefetch_readIdx > prefetch_writeIdx + slots_per_alloc.U) || (prefetch_readIdx <= prefetch_writeIdx)
          }
        }
        tl_reg.io.enq.valid := sourceAvailable && haveRoomToAlloc
        tl_reg.io.enq.bits := tledge.Get(
          fromSource = chosenSource,
          toAddress = addr,
          lgSize = lgLargestRead.U
        )._2
        when(tl_reg.io.enq.fire) {
          addr := addr + largestRead.U
          len := len - largestRead.U
          prefetch_writeIdx := Mux(prefetch_writeIdx === (prefetchRows - slots_per_alloc).U, 0.U, prefetch_writeIdx + slots_per_alloc.U)
          beatsRemaining(chosenSource) := (largeTxNBeats - 1).U
        }
      }

      when(tl_reg.io.enq.fire) {
        state := s_read_memory
        sourceToIdx(chosenSource) := prefetch_writeIdx
        sourceIdleBits(chosenSource) := false.B
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
  val channel_buffer_depth = prefetch_blatency + 2
  val channel_buffer_q = Module(new Queue(UInt(storedDataWidth.W), channel_buffer_depth))
  val channels = VecInit(
    (0 until channelsPerStorage) map { ch_buffer_idx =>
      val high = (ch_buffer_idx + 1) * channelWidthBits - 1
      val low = channelWidthBits * ch_buffer_idx
      channel_buffer_q.io.deq.bits(high, low)
    })

  channel_buffer := VecInit((0 until vlen) map { vidx =>
    val high = (vidx + 1) * dataBytes * 8 - 1
    val low = vidx * dataBytes * 8
    channels(data_channel_read_idx)(high, low)
  })


  val read_enable_pipeline = ShiftReg(prefetch_buffers.chip_select(0) && read_ready, prefetch_blatency)
  val reads_in_flight = RegInit(0.U(3.W))
  val queue_occupancy = RegInit(0.U(log2Up(channel_buffer_depth + 1).W))

  when(prefetch_buffers_valid(prefetch_readIdx) && reads_in_flight + queue_occupancy < channel_buffer_depth.U && read_ready) {
    prefetch_buffers.chip_select(0) := true.B
    prefetch_buffers_valid(prefetch_readIdx) := false.B
    prefetch_readIdx := prefetch_readIdx + 1.U
    when (prefetch_readIdx === (prefetchRows - 1).U) {
      prefetch_readIdx := 0.U
    }
  }

  // maintain reads in flight
  when(prefetch_buffers.chip_select(0) && read_enable_pipeline) {}.elsewhen(prefetch_buffers.chip_select(0)) {
    reads_in_flight := reads_in_flight + 1.U
  }.elsewhen(read_enable_pipeline) {
    reads_in_flight := reads_in_flight - 1.U
  }

  // maintain queue occupancy
  when(channel_buffer_q.io.deq.fire && channel_buffer_q.io.enq.fire) {}.elsewhen(channel_buffer_q.io.deq.fire) {
    queue_occupancy := queue_occupancy - 1.U
  }.elsewhen(channel_buffer_q.io.enq.fire) {
    queue_occupancy := queue_occupancy + 1.U
  }

  channel_buffer_q.io.enq.valid := read_enable_pipeline
  channel_buffer_q.io.enq.bits := prefetch_buffers.data_out(0)
  channel_buffer_q.io.deq.ready := false.B
  channel_buffer_valid := channel_buffer_q.io.deq.valid
  when(io.channel.data.fire) {
    data_channel_read_idx := data_channel_read_idx + 1.U
    when(data_channel_read_idx === (channelsPerStorage - 1).U) {
      data_channel_read_idx := 0.U
      channel_buffer_q.io.deq.ready := true.B
    }
  }
  io.req.ready := !(state =/= s_idle || reads_in_flight > 0.U || (prefetch_readIdx =/= prefetch_writeIdx) || queue_occupancy =/= 0.U)

  if (p(BuildModeKey).isInstanceOf[BuildMode.Tuning]) {
    val tx_cycles = PerfCounter(Reg(UInt(64.W)), designObjective = new MinimizeObjective)
    val tx_counter = Reg(UInt(64.W))
    tx_counter := tx_counter + 1.U
    when (io.req.fire) {
      tx_counter := 0.U
    }
    when (io.req.ready && !RegNext(io.req.ready)) {
      tx_cycles := tx_counter
    }
  }
}