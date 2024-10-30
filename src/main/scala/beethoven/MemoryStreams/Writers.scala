package beethoven.MemoryStreams

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import beethoven.Generation.BeethovenBuild
import beethoven.platform
import beethoven.common.{CLog2Up, Stack, splitIntoChunks}
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
 * @param userBytes the number of bytes in a single item
 */
class SequentialWriter(userBytes: Int,
                       val tl_outer: TLBundle,
                       edge: TLEdgeOut,
                       minSizeBytes: Option[Int] = None)
                      (implicit p: Parameters) extends Module {
  override val desiredName = s"SequentialWriter_w${userBytes * 8}"
  require(isPow2(userBytes))
  private val fabricBeatBytes = tl_outer.params.dataBits / 8
  private val addressBits = tl_outer.params.addressBits
  private val addressBitsChop = addressBits - log2Up(fabricBeatBytes)
  private val nSources = edge.master.endSourceId
  val pfsm = platform.prefetchSourceMultiplicity
  val userBeatsPerLargeTx = fabricBeatBytes * pfsm / userBytes
  require(isPow2(pfsm) && pfsm > 1)

  val io = IO(new SequentialWriteChannelIO(userBytes))
  val tl_out = IO(new TLBundle(tl_outer.params))
  io.req.ready := false.B
  io.channel.data.ready := false.B
  tl_out.a.valid := false.B
  tl_out.a.bits := DontCare

  val q_size = Math.max(
    minSizeBytes.getOrElse(0) / fabricBeatBytes,
    pfsm * nSources)

  val burst_storage_io = Module(new Queue(
    UInt(tl_outer.params.dataBits.W),
    platform.prefetchSourceMultiplicity,
    pipe = true,
    useSyncReadMem = true // hopefully this gives us BRAM in FPGA. Worry about ASIC later ugh
  )).io

  // logical operation "a -> b"
  def implies(a: Bool, b: Bool): Bool = (a && b) || !a

  val memory_latency = 3
  val write_buffer_io = Wire(Output(Decoupled(UInt(tl_outer.params.dataBits.W))))
  val write_buffer = Memory(memory_latency, tl_outer.params.dataBits, q_size, 1, 1, 0)
  val write_buffer_occupancy = RegInit(0.U(log2Up(q_size + 1).W))
  val write_buffer_read_shift = RegInit(0.U(memory_latency.W))
  val burst_storage_occupancy = RegInit(0.U(log2Up(platform.prefetchSourceMultiplicity + 1).W))
  val raddr = RegInit(0.U(log2Up(q_size).W))
  val waddr = RegInit(0.U(log2Up(q_size).W))
  val (wb_widx, wb_ridx) = if (write_buffer.nWritePorts == 0) (0, 1) else (write_buffer.getWritePortIdx(0), write_buffer.getReadPortIdx(0))

  write_buffer_read_shift := Cat(write_buffer.chip_select(wb_ridx), write_buffer_read_shift >> 1)
  write_buffer_io.valid := false.B
  write_buffer_io.bits := DontCare
  write_buffer_io.ready := write_buffer_occupancy < q_size.U

  write_buffer.initLow(clock)
  write_buffer.addr(wb_widx) := waddr
  write_buffer.write_enable(wb_widx) := true.B
  write_buffer.data_in(wb_widx) := write_buffer_io.bits
  write_buffer.chip_select(wb_widx) := write_buffer_io.fire
  waddr := waddr + write_buffer.chip_select(wb_widx)

  write_buffer.addr(wb_ridx) := raddr
  write_buffer.read_enable(wb_ridx) := true.B
  write_buffer.chip_select(wb_ridx) := burst_storage_occupancy < platform.prefetchSourceMultiplicity.U && write_buffer_occupancy > 0.U
  burst_storage_io.enq.valid := write_buffer_read_shift(0)
  burst_storage_io.enq.bits := write_buffer.data_out(wb_ridx)
  assert(implies(burst_storage_io.enq.valid, burst_storage_io.enq.ready))
  raddr := raddr + write_buffer.chip_select(wb_ridx)

  when (write_buffer_io.fire) {
    when (!write_buffer.chip_select(wb_ridx)) {
      write_buffer_occupancy := write_buffer_occupancy + 1.U
    }
  }.otherwise {
    when(write_buffer.chip_select(wb_ridx)) {
      write_buffer_occupancy := write_buffer_occupancy - 1.U
    }
  }

  when (write_buffer.chip_select(wb_ridx)) {
    when (!burst_storage_io.deq.fire) {
      burst_storage_occupancy := burst_storage_occupancy + 1.U
    }
  }.otherwise {
    when (burst_storage_io.deq.fire) {
      burst_storage_occupancy := burst_storage_occupancy - 1.U
    }
  }

  burst_storage_io.deq.ready := tl_out.a.fire

  // keep two different counts so that we can keep enqueueing while bursting
  val burst_progress_count = RegInit(0.U(log2Up(pfsm).W))

  val req_addr = RegInit(0.U(addressBitsChop.W))

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

  io.busy := true.B
  io.channel.isFlushed := sourceBusyBits.asUInt === 0.U

  val expectedNumBeats = RegInit(0.U((addressBits - log2Up(userBytes)).W))
  when(expectedNumBeats === 0.U) {
    when(!sourcesInProgress) {
      io.busy := false.B
      io.req.ready := true.B
      when(io.req.fire) {
        val choppedAddr = (io.req.bits.addr >> log2Up(fabricBeatBytes)).asUInt
        expectedNumBeats := io.req.bits.len >> CLog2Up(userBytes)
        req_addr := choppedAddr
        burst_progress_count := 0.U
      }
    }
  }
  when(burst_inProgress) {
    tl_out.a.valid := true.B
    assert(burst_storage_io.deq.valid)
    tl_out.a.bits.opcode := TLMessages.PutFullData
    tl_out.a.bits.address := addrInProgress
    tl_out.a.bits.size := log2Up(platform.prefetchSourceMultiplicity * fabricBeatBytes).U
    tl_out.a.bits.data := burst_storage_io.deq.bits
    tl_out.a.bits.mask := BigInt("1" * fabricBeatBytes, radix=2).U
    when(tl_out.a.fire) {
      burst_progress_count := burst_progress_count + 1.U
      when(burst_progress_count === (pfsm - 1).U) {
        // try to allocate
        burst_inProgress := false.B
      }
    }
  }.otherwise {
    val isSmall = expectedNumBeats < userBeatsPerLargeTx.U
    val burstSize = Mux(isSmall, 1.U, pfsm.U)
    tl_out.a.valid := hasAvailableSource && burst_storage_occupancy >= burstSize && burst_storage_io.deq.valid
    require(platform.prefetchSourceMultiplicity >= memory_latency,
    """
        |If the valid signal is high, there is _at least_ one thing in the burst queue. For burst storage
        |occupancy = bso and memory latency = ml and bso >= ml, we know that bso can be greater than the real
        |occupancy of the queue. However, if bso >= burst length, then within ml cycles, there will have been
        |at least bso elements within the queue, guaranteed by bso >= ml.
        |""".stripMargin)
    val nextAddr = Cat(req_addr,
      0.U(log2Up(fabricBeatBytes).W))

    tl_out.a.bits.data := burst_storage_io.deq.bits
    tl_out.a.bits.size := Mux(isSmall, log2Up(fabricBeatBytes).U, log2Up(fabricBeatBytes * pfsm).U)
    tl_out.a.bits.address := nextAddr
    tl_out.a.bits.source := nextSource
    tl_out.a.bits.opcode := TLMessages.PutFullData
    when(tl_out.a.fire) {
      sourceBusyBits(nextSource) := true.B
      sourceInProgress := nextSource
      addrInProgress := nextAddr
      req_addr := req_addr + burstSize
      when(!isSmall) {
        burst_inProgress := true.B
        burst_progress_count := 1.U
      }
    }
  }

  if (userBytes == fabricBeatBytes) {
    io.channel.data <> write_buffer_io
  } else if (userBytes < fabricBeatBytes) {
    val beatLim = fabricBeatBytes / userBytes
    val beatBuffer = Reg(Vec(beatLim - 1, UInt((userBytes * 8).W)))
    val beatCounter = Reg(UInt(log2Up(beatLim).W))
    io.channel.data.ready := write_buffer_io.ready && expectedNumBeats > 0.U
    write_buffer_io.valid := false.B
    write_buffer_io.bits := DontCare
    when(io.req.fire) {
      beatCounter := 0.U
    }
    when(io.channel.data.fire) {
      expectedNumBeats := expectedNumBeats - 1.U
      val bytesGrouped = (0 until userBytes).map(i => io.channel.data.bits((i + 1) * 8 - 1, i * 8))
      beatBuffer(beatCounter) := Cat(bytesGrouped.reverse)
      beatCounter := beatCounter + 1.U
      when(beatCounter === (beatLim - 1).U || expectedNumBeats === 1.U) {
        write_buffer_io.valid := true.B
        val bgc = Cat(bytesGrouped.reverse)
        write_buffer_io.bits := Cat(bgc, Cat(beatBuffer.reverse))
        beatCounter := 0.U
      }
    }
  } else {
    val dsplit = splitIntoChunks(io.channel.data.bits, fabricBeatBytes*8)
    val inProgressPushing = RegInit(false.B)
    val channelReg = Reg(Vec(userBytes / fabricBeatBytes, UInt((fabricBeatBytes*8).W))) // bottom chunk will get optimized away
    val dsplitCount = Reg(UInt(log2Up(userBytes / fabricBeatBytes + 1).W))
    when (!inProgressPushing) {
      io.channel.data.ready := expectedNumBeats > 0.U && write_buffer_io.ready
      write_buffer_io.bits := dsplit(0)
      write_buffer_io.valid := expectedNumBeats > 0.U && io.channel.data.valid
      when (io.channel.data.fire) {
        inProgressPushing := true.B
        channelReg := dsplit
        dsplitCount := 1.U
      }
    }.otherwise {
      io.channel.data.ready := false.B
      write_buffer_io.valid := true.B
      write_buffer_io.bits := channelReg(dsplitCount)
      when (write_buffer_io.fire) {
        dsplitCount := dsplitCount + 1.U
        when (dsplitCount === (userBytes / fabricBeatBytes - 1).U) {
          inProgressPushing := false.B
        }
      }
    }
  }

  tl_out.d.ready := true.B
  when(tl_out.d.fire) {
    sourceBusyBits(tl_out.d.bits.source) := false.B
  }
}

