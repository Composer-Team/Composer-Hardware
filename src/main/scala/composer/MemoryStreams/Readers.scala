package composer.MemoryStreams

import chipsalliance.rocketchip.config._
import chisel3.{Reg, _}
import chisel3.util._
import composer.{PlatformType, PlatformTypeKey, PrefetchSourceMultiplicity}
import composer.Systems.DataChannelIO
import composer.common.{CLog2Up, ShiftReg}
import freechips.rocketchip.diplomacy.ValName
import freechips.rocketchip.tilelink._

class ReadChannelIO(dataBytes: Int, vlen: Int)(implicit p: Parameters) extends Bundle {
  val req = Flipped(Decoupled(new ChannelTransactionBundle))
  val channel = new DataChannelIO(dataBytes, vlen)
  val busy = Output(Bool())
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
  println("largest read is " + largestRead)


  val prefetch_readIdx, prefetch_writeIdx = Reg(UInt(log2Up(prefetchRows).W))

  val prefetch_blatency = p(PlatformTypeKey) match {
    case PlatformType.FPGA => 3
    case PlatformType.ASIC => (prefetchRows.toFloat / 512).ceil.toInt
  }
  val prefetch_buffers = CMemory(prefetch_blatency, storedDataWidth, prefetchRows, debugName = Some(debugName.getOrElse("") + "_prefetchBuffer"))(p.alterPartial({
    case SimpleDRAMHintKey => true
  }), valName = ValName.apply("prefetch_buffers"))
  prefetch_buffers.CE1 := clock.asBool
  prefetch_buffers.CE2 := clock.asBool

  prefetch_buffers.A1 := prefetch_readIdx
  prefetch_buffers.I1 := DontCare
  prefetch_buffers.WEB1 := false.B
  prefetch_buffers.OEB1 := true.B
  prefetch_buffers.CSB1 := false.B

  prefetch_buffers.CSB2 := false.B
  prefetch_buffers.OEB2 := false.B
  prefetch_buffers.WEB2 := true.B
  prefetch_buffers.I2 := DontCare
  prefetch_buffers.A2 := DontCare

  val prefetch_buffers_valid = Reg(Vec(prefetchRows, Bool()))

  when(reset.asBool) {
    prefetch_buffers_valid.foreach(_ := false.B)
  }

  tl_out.d.ready := atLeastOneSourceActive

  val sourceToIdx = Reg(Vec(nSources, UInt(log2Up(prefetchRows).W)))
  val beatsRemaining = Reg(Vec(nSources, UInt(log2Up(largeTxNBeats).W)))
  val slots_per_alloc =  if (beatBytes >= maxBytes) {
    when(tl_out.d.fire) {
      val dSource = tl_out.d.bits.source
      val prefetchIdx = sourceToIdx(dSource)

      prefetch_buffers.I2 := tl_out.d.bits.data
      prefetch_buffers.A2 := prefetchIdx
      prefetch_buffers.CSB2 := true.B

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
      when (beatsRemaining(src) === 0.U) {
        sourceIdleBits(src) := true.B
      }

      perSourceBeatProgress(src) := perSourceBeatProgress(src) + 1.U
      when(perSourceBeatProgress(src) === (beatsPerLine - 1).U) {
        perSourceBeatProgress(src) := 0.U

        prefetch_buffers_valid(sourceToIdx(src)) := true.B
        sourceToIdx(src) := sourceToIdx(src) + 1.U

        prefetch_buffers.A2 := sourceToIdx(src)
        prefetch_buffers.I2 := Cat(tl_out.d.bits.data, Cat(beatBufferPerSource(src).reverse))
        prefetch_buffers.CSB2 := true.B

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
        when (dist2Roof < slots_per_alloc.U) {
          haveRoomToAlloc := prefetch_readIdx > (slots_per_alloc.U - dist2Roof) && (prefetch_readIdx < prefetch_writeIdx)
        }.otherwise {
          haveRoomToAlloc := (prefetch_readIdx > prefetch_writeIdx + slots_per_alloc.U) || (prefetch_readIdx <= prefetch_writeIdx)
        }
        tl_reg.io.enq.valid := sourceAvailable && haveRoomToAlloc
        println("largeTxNBeats: " + largeTxNBeats)
        tl_reg.io.enq.bits := tledge.Get(
          fromSource = chosenSource,
          toAddress = addr,
          lgSize = lgLargestRead.U
        )._2
        when(tl_reg.io.enq.fire) {
          addr := addr + largestRead.U
          len := len - largestRead.U
          prefetch_writeIdx := prefetch_writeIdx + slots_per_alloc.U
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


  val read_enable_pipeline = ShiftReg(prefetch_buffers.CSB1, prefetch_blatency)
  val reads_in_flight = RegInit(0.U(3.W))
  val queue_occupancy = RegInit(0.U(log2Up(channel_buffer_depth + 1).W))

  dontTouch(prefetch_readIdx)
  when(prefetch_buffers_valid(prefetch_readIdx) && reads_in_flight + queue_occupancy < channel_buffer_depth.U) {
    prefetch_buffers.CSB1 := true.B
    prefetch_buffers_valid(prefetch_readIdx) := false.B
    prefetch_readIdx := prefetch_readIdx + 1.U
  }

  // maintain reads in flight
  when(prefetch_buffers.CSB1 && read_enable_pipeline) {}.elsewhen(prefetch_buffers.CSB1) {
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
  channel_buffer_q.io.enq.bits := prefetch_buffers.O1
  channel_buffer_q.io.deq.ready := false.B
  channel_buffer_valid := channel_buffer_q.io.deq.valid
  when(io.channel.data.fire) {
    data_channel_read_idx := data_channel_read_idx + 1.U
    when(data_channel_read_idx === (channelsPerStorage - 1).U) {
      data_channel_read_idx := 0.U
      channel_buffer_q.io.deq.ready := true.B
    }
  }
}