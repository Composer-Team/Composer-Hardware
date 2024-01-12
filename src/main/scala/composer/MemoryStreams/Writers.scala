package composer.MemoryStreams

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import composer.PrefetchSourceMultiplicity
import composer.common.Stack
import freechips.rocketchip.tilelink._

class WriterDataChannelIO(val dWidth: Int) extends Bundle {
  val data = Flipped(Decoupled(UInt(dWidth.W)))
  val isFlushed = Output(Bool())
}

class SequentialWriteChannelIO(maxBytes: Int)(implicit p: Parameters) extends Bundle {
  val req = Flipped(Decoupled(new ChannelTransactionBundle))
  val channel = new WriterDataChannelIO(maxBytes * 8)
  val busy = Output(Bool())
}

/**
 * Writes out a set number of fixed-size items sequentially to memory.
 *
 * @param nBytes the number of bytes in a single item
 */
class SequentialWriter(nBytes: Int,
                       val tl_outer: TLBundle,
                       edge: TLEdgeOut,
                       minSizeBytes: Option[Int] = None,
                       backwards: Boolean = false)
                      (implicit p: Parameters) extends Module {
  require(isPow2(nBytes))
  private val beatBytes = tl_outer.params.dataBits / 8
  private val addressBits = tl_outer.params.addressBits
  private val addressBitsChop = addressBits - log2Up(beatBytes)
  private val nSources = edge.master.endSourceId
  val pfsm = p(PrefetchSourceMultiplicity)
  require(isPow2(pfsm) && pfsm > 1)

  val io = IO(new SequentialWriteChannelIO(nBytes))
  val tl_out = IO(new TLBundle(tl_outer.params))
  io.req.ready := false.B
  io.channel.data.ready := false.B
  tl_out.a.valid := false.B
  tl_out.a.bits := DontCare

  val q_size = Math.max(
    minSizeBytes.getOrElse(0) / beatBytes,
    pfsm * nSources)

  val burst_storage_io = if (backwards) {
    Module(new Stack[UInt](UInt(tl_outer.params.dataBits.W), q_size)).io
  } else {
    Module(new Queue(
      UInt(tl_outer.params.dataBits.W),
      q_size,
      pipe = true,
      useSyncReadMem = true // hopefully this gives us BRAM in FPGA. Worry about ASIC later ugh
    )).io
  }

  burst_storage_io.deq.ready := tl_out.a.fire

  // keep two different counts so that we can keep enqueueing while bursting
  val burst_progress_count = RegInit(0.U(log2Up(pfsm).W))
  val burst_occupancy = RegInit(0.U(log2Up(q_size + 1).W))

  val req_len, req_addr = RegInit(0.U(addressBitsChop.W))

  val sourceBusyBits = Reg(Vec(edge.master.endSourceId, Bool()))
  when(reset.asBool) {
    sourceBusyBits.foreach(_ := false.B)
  }
  val sourcesInProgress = sourceBusyBits.fold(false.B)(_ || _)
  val hasAvailableSource = (~sourceBusyBits.asUInt).asBools.fold(false.B)(_ || _)
  val nextSource = PriorityEncoder(~sourceBusyBits.asUInt)

  val burst_inProgress = RegInit(false.B)
  val sourceInProgress = Reg(UInt(tl_out.params.sourceBits.W))
  val addrInProgress = Reg(UInt(addressBits.W))

  val blockage = WireInit(false.B)

  io.busy := true.B
  io.channel.isFlushed := sourceBusyBits.asUInt === 0.U
  when(req_len === 0.U) {
    when(!sourcesInProgress) {
      io.busy := false.B
      io.req.ready := true.B
      when(io.req.fire) {
        val l = (io.req.bits.len >> log2Up(beatBytes)).asUInt
        req_len := l
        val choppedAddr = (io.req.bits.addr >> log2Up(beatBytes)).asUInt
        if (backwards) {
          req_addr := choppedAddr + l
        } else {
          req_addr := choppedAddr
        }
      }
    }
  }
  when(burst_inProgress) {
    tl_out.a.valid := true.B
    assert(burst_storage_io.deq.valid)
    tl_out.a.bits := edge.Put(
      sourceInProgress,
      addrInProgress,
      log2Up(p(PrefetchSourceMultiplicity) * beatBytes).U,
      burst_storage_io.deq.bits)._2
    when(tl_out.a.fire) {
      burst_progress_count := burst_progress_count + 1.U
      when(burst_progress_count === (pfsm - 1).U) {
        // try to allocate
        burst_inProgress := false.B
      }
    }
  }.otherwise {
    val isSmall = req_len < pfsm.U
    val burstSize = Mux(isSmall, 1.U, pfsm.U)
    if (backwards) {
      // we drain the enqueue buffer under 3 circumstances
      // 1. We have a short transaction that needs to proceed immediately in order to reach alignment
      // 2. We have all of the data that we need to complete the rest of the transaction
      // 3. The enqueue buffer is full
      // do not drain under any other circumstance
      val isTransactionComplete = req_len === burst_occupancy
      val enqueueBufferFull = burst_occupancy === q_size.U
      val ongoingDrain = RegInit(false.B)
      blockage := ongoingDrain
      val total_increment = Reg(burst_storage_io.count.cloneType)
      when(((isSmall && burst_occupancy > 0.U && req_len > q_size.U) || isTransactionComplete || enqueueBufferFull) && req_len > 0.U && !ongoingDrain) {
        ongoingDrain := true.B
        total_increment := burst_storage_io.count
      }
      when(ongoingDrain) {
        when(burst_storage_io.count === 1.U && burst_storage_io.deq.fire) {
          ongoingDrain := false.B
          req_addr := req_addr - total_increment
        }
      }
      tl_out.a.valid := hasAvailableSource && ongoingDrain
    } else {
      tl_out.a.valid := hasAvailableSource && burst_occupancy >= burstSize && req_len > 0.U
    }
    val nextAddr = Cat(if (backwards) req_addr - burst_storage_io.count else req_addr,
      0.U(log2Up(beatBytes).W))

    tl_out.a.bits := edge.Put(
      nextSource,
      nextAddr,
      Mux(isSmall, log2Up(beatBytes).U, log2Up(beatBytes * pfsm).U),
      burst_storage_io.deq.bits)._2
    when(tl_out.a.fire) {
      sourceBusyBits(nextSource) := true.B
      sourceInProgress := nextSource
      addrInProgress := nextAddr
      burst_progress_count := burst_progress_count + 1.U
      if (!backwards) {
        req_addr := req_addr + burstSize
      }
      req_len := req_len - burstSize
      when(!isSmall) {
        burst_inProgress := true.B
      }
    }
  }

  when(burst_storage_io.enq.fire) {
    when(!burst_storage_io.deq.fire) {
      burst_occupancy := burst_occupancy + 1.U
    }
  }.otherwise {
    when(burst_storage_io.deq.fire) {
      burst_occupancy := burst_occupancy - 1.U
    }
  }

  if (nBytes == beatBytes) {
    io.channel.data <> burst_storage_io.enq
  } else if (nBytes < beatBytes) {
    val beatLim = beatBytes / nBytes
    val beatBuffer = Reg(Vec(beatLim - 1, UInt((nBytes * 8).W)))
    val beatCounter = RegInit(0.U(log2Up(beatLim).W))
    io.channel.data.ready := burst_storage_io.enq.ready && req_len > 0.U && !blockage
    burst_storage_io.enq.valid := false.B
    burst_storage_io.enq.bits := DontCare
    when(io.channel.data.fire) {
      val bytesGrouped = (0 until nBytes).map(i => io.channel.data.bits((i + 1) * 8 - 1, i * 8))
      beatBuffer(beatCounter) := Cat(bytesGrouped.reverse)
      beatCounter := beatCounter + 1.U
      when(beatCounter === (beatLim - 1).U) {
        burst_storage_io.enq.valid := true.B
        val bgc = Cat(bytesGrouped.reverse)
        burst_storage_io.enq.bits := (if (backwards) Cat(Cat(beatBuffer), bgc) else Cat(bgc, Cat(beatBuffer.reverse)))
        beatCounter := 0.U
      }
    }
  } else {
    require(requirement = false, "Unexpected")
  }

  tl_out.d.ready := true.B
  when(tl_out.d.fire) {
    sourceBusyBits(tl_out.d.bits.source) := false.B
  }
}

