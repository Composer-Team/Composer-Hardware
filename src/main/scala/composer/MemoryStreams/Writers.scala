package composer.MemoryStreams

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import composer.PrefetchSourceMultiplicity
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
                       edge: TLEdgeOut)
                      (implicit p: Parameters) extends Module {
  require(isPow2(nBytes))
  private val beatBytes = tl_outer.params.dataBits / 8
  private val addressBits = tl_outer.params.addressBits
  private val addressBitsChop = addressBits - log2Up(beatBytes)
  private val nSources = edge.master.endSourceId
  val io = IO(new SequentialWriteChannelIO(nBytes))
  val tl_out = IO(new TLBundle(tl_outer.params))
  io.req.ready := false.B
  io.channel.data.ready := false.B
  tl_out.a.valid := false.B
  tl_out.a.bits := DontCare

  val burst_queue = Module(new Queue(
    UInt(tl_outer.params.dataBits.W),
    p(PrefetchSourceMultiplicity) * nSources,
    pipe = true))
  burst_queue.io.deq.ready := tl_out.a.fire

  // keep two different counts so that we can keep enqueueing while bursting
  val burst_progress_count = RegInit(0.U(log2Up(p(PrefetchSourceMultiplicity)).W))
  val burst_occupancy = RegInit(0.U((log2Up(p(PrefetchSourceMultiplicity) * edge.master.endSourceId) + 1).W))

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
  require(isPow2(p(PrefetchSourceMultiplicity)))

  io.busy := true.B
  io.channel.isFlushed := sourceBusyBits.asUInt === 0.U
  when(req_len === 0.U) {
    when(!sourcesInProgress) {
      io.busy := false.B
      io.req.ready := true.B
      when(io.req.fire) {
        req_len := io.req.bits.len >> log2Up(beatBytes)
        req_addr := io.req.bits.addr >> log2Up(beatBytes)
      }
    }
  }
  when(burst_inProgress) {
    tl_out.a.valid := true.B
    assert(burst_queue.io.deq.valid)
    tl_out.a.bits := edge.Put(
      sourceInProgress,
      addrInProgress,
      log2Up(p(PrefetchSourceMultiplicity) * beatBytes).U,
      burst_queue.io.deq.bits)._2
    when(tl_out.a.fire) {
      burst_progress_count := burst_progress_count + 1.U
      when(burst_progress_count === (p(PrefetchSourceMultiplicity) - 1).U) {
        // try to allocate
        burst_inProgress := false.B
      }
    }
  }.otherwise {
    val isSmall = req_len < p(PrefetchSourceMultiplicity).U
    val burstSize = Mux(isSmall,
      1.U,
      p(PrefetchSourceMultiplicity).U)
    tl_out.a.valid := hasAvailableSource && burst_occupancy >= burstSize && req_len > 0.U
    tl_out.a.bits := edge.Put(
      nextSource,
      Cat(req_addr, 0.U(log2Up(beatBytes).W)),
      Mux(isSmall, log2Up(beatBytes).U, log2Up(beatBytes * p(PrefetchSourceMultiplicity)).U),
      burst_queue.io.deq.bits)._2
    when(tl_out.a.fire) {
      sourceBusyBits(nextSource) := true.B
      sourceInProgress := nextSource
      addrInProgress := Cat(req_addr, 0.U(log2Up(beatBytes).W))
      burst_progress_count := burst_progress_count + 1.U
      req_len := req_len - burstSize
      req_addr := req_addr + burstSize
      when(!isSmall) {
        burst_inProgress := true.B

      }
    }
  }

  when(burst_queue.io.enq.fire) {
    when(!burst_queue.io.deq.fire) {
      burst_occupancy := burst_occupancy + 1.U
    }
  }.otherwise {
    when(burst_queue.io.deq.fire) {
      burst_occupancy := burst_occupancy - 1.U
    }
  }

  if (nBytes == beatBytes) {
    io.channel.data <> burst_queue.io.enq
  } else if (nBytes < beatBytes) {
    val beatLim = beatBytes / nBytes
    val beatBuffer = Reg(Vec(beatLim-1, UInt((nBytes * 8).W)))
    val beatCounter = RegInit(0.U(log2Up(beatLim).W))
    io.channel.data.ready := burst_queue.io.enq.ready
    burst_queue.io.enq.valid := false.B
    burst_queue.io.enq.bits := DontCare
    when(io.channel.data.fire) {
      val bytesGrouped = (0 until nBytes).map(i => io.channel.data.bits((i + 1) * 8 - 1, i * 8))
      beatBuffer(beatCounter) := Cat(bytesGrouped.reverse)
      beatCounter := beatCounter + 1.U
      when(beatCounter === (beatLim - 1).U) {
        burst_queue.io.enq.valid := true.B
        val beatBufferCat = Cat(beatBuffer.reverse)
        val bgc = Cat(bytesGrouped.reverse)
        burst_queue.io.enq.bits := Cat(bgc, beatBufferCat)
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

