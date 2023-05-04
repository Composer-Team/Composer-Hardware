package composer.MemoryStreams

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import composer.DRAMBankBytes
import composer.MemoryStreams.Loaders.CScratchpadPackedSubwordLoader
import composer.common.CLog2Up
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.{CacheBlockBytes, ExtMem}
import freechips.rocketchip.tilelink._

import scala.annotation.tailrec

class CScratchpadAccessBundle(scReqBits: Int, dataWidthBits: Int) extends Bundle {
  // note the flipped
  val readReq = Flipped(Decoupled(UInt(scReqBits.W)))
  val readRes = ValidIO(UInt(dataWidthBits.W))
  val writeReq = Flipped(Decoupled(new Bundle() {
      val addr = UInt(scReqBits.W)
      val data = UInt(dataWidthBits.W)
    }))
}

class CScratchpadInitReqIO(mem_out: TLBundle, nDatas: Int) extends Bundle {
  val progress = Output(UInt((log2Up(nDatas) + 1).W))
  val request, writeback = Flipped(Decoupled(new Bundle() {
    val memAddr = UInt(mem_out.params.addressBits.W)
    val scAddr = UInt(log2Up(nDatas).W)
    val len = UInt(mem_out.params.addressBits.W)
  }))
}


/**
  * Parameters that all scratchpad subtypes should support
  *
  * @param dataWidthBits  the granularity of a single scratchpad read/write in bits
  * @param nDatas         number of data items in the scratchpad. Non-zero
  * @param latency        latency of a scratchpad access from the user interface. Current implementation only supports 1 or 2.
  * @param specialization How data is loaded from memory. Choose a specialization from CScratchpadSpecialization
  */
class CScratchpad(supportWriteback: Boolean,
                  dataWidthBits: Int,
                  nDatas: Int,
                  latency: Int,
                  specialization: CScratchpadSpecialization)(implicit p: Parameters) extends LazyModule {
  require(dataWidthBits > 0)
  require(nDatas > 0)

  val channelWidthBytes = p(ExtMem).get.master.beatBytes

  val mem = TLClientNode(Seq(TLMasterPortParameters.v2(
    masters = Seq(TLMasterParameters.v1(
      name = "ScratchpadToMemory",
      sourceId = IdRange(0, 8),
      supportsProbe = TransferSizes(1, channelWidthBytes),
      supportsGet = TransferSizes(1, channelWidthBytes),
      supportsPutFull = TransferSizes(1, channelWidthBytes)
    )),
    channelBytes = TLChannelBeatBytes(channelWidthBytes))))
  lazy val module = new CScratchpadImp(supportWriteback, dataWidthBits, nDatas, latency, specialization, this)
}

class CScratchpadImp(supportWriteback: Boolean,
                     dataWidthBits: Int,
                     nDatas: Int,
                     latency: Int,
                     specialization: CScratchpadSpecialization,
                     outer: CScratchpad) extends LazyModuleImp(outer) {
  private val (mem_out, mem_edge) = outer.mem.out(0)
  private val scReqBits = log2Up(nDatas)

  private val maxTxLength = outer.channelWidthBytes
  private val lgMaxTxLength = log2Up(maxTxLength)

  private val pageLength = p(DRAMBankBytes)

  @tailrec
  private def nestPipeline[T <: Data](a: T, depth: Int): T = {
    if (depth == 0) a
    else nestPipeline(RegNext(a), depth - 1)
  }

  val access = IO(new CScratchpadAccessBundle(scReqBits, dataWidthBits))
  val req = IO(new CScratchpadInitReqIO(mem_out, nDatas))

  req.request.ready := true.B

//  val mem = SyncReadMem(nDatas, UInt(dataWidthBits.W))
  private val memory = Module(new CMemory(latency-2, dataWidth = dataWidthBits, nRows = nDatas, ports = 1))

  private val loader = Module(specialization match {
    case psw: PackedSubwordScratchpadParams =>
      new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, psw.wordSizeBits, psw.datsPerSubword, maxTxLength)
    case _: FlatPackScratchpadParams =>
      new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, dataWidthBits, 1, maxTxLength)
  })

  private val read_idle :: read_send :: read_process :: Nil = Enum(3)
  private val mem_tx_state = RegInit(read_idle)

  access.readRes.valid := nestPipeline(access.readReq.valid, latency)

  memory.io.clk := clock
  memory.io.rst := reset

  memory.io.mem_en(0) := access.readReq.valid || access.writeReq.valid
  assert(!(access.readReq.valid && access.writeReq.valid), "Currently only support single port BRAMs")
  memory.io.addr(0) := Mux(access.writeReq.valid, access.writeReq.bits.addr, access.readReq.bits)
  memory.io.we(0) := access.writeReq.valid
  memory.io.din(0) := access.writeReq.bits.data
  memory.io.regce(0) := true.B

//  memory.io.addr(0) := access.writeReq.bits.addr
//  memory.io.we(0) := true.B
//  memory.io.din(0) := access.writeReq.bits.data
//  memory.io.mem_en(0) := access.writeReq.valid
//  memory.io.regce(WRITE_PORT) := false.B

  val output_enable = nestPipeline(access.readReq.valid, latency)
  access.readRes.valid := output_enable
  access.readRes.bits := memory.io.dout(0)
  // in-flight read tx busy bits

  req.request.ready := mem_tx_state === read_idle

  private val reqIdleBits = RegInit(VecInit(Seq.fill(mem_edge.master.endSourceId)(true.B)))
  private val reqAvailable = reqIdleBits.reduce(_ || _)
  private val reqChosen = PriorityEncoder(reqIdleBits)
  // only consider non-busy transactions to be the
  private val req_cache = SyncReadMem(mem_edge.master.endSourceId, new Bundle() {
    val scratchpadAddress = UInt(req.request.bits.scAddr.getWidth.W)
    val memoryLength = UInt(mem_out.params.addressBits.W)
    val memAddr = UInt(mem_out.params.addressBits.W)
  })
  /* handle progress monitoring */

  class ProgressStat extends Bundle {
    val progress = UInt((1 + scReqBits).W)
    val valid = Bool()
    val complete = Bool()
  }

  private val perSourceProgress = Reg(Vec(mem_edge.master.endSourceId, new ProgressStat()))
  private val loaderSource = Reg(UInt(mem_out.params.sourceBits.W))

  private val totalTx = Reg(new Bundle {
    val memoryAddress = UInt(req.request.bits.memAddr.getWidth.W)
    val scratchpadAddress = UInt(req.request.bits.scAddr.getWidth.W)
    val memoryLength = UInt(mem_out.params.addressBits.W)
  })

  private val txEmitLengthLg = Wire(UInt(4.W))
  txEmitLengthLg := 0.U

  // relaunch transactions at bank granularity
  val rsource = mem_out.d.bits.source
  val prevProcessed = Reg(rsource.cloneType)
  val prevDone = Reg(Bool())
  val relaunch_queue = Module(new Queue[UInt](reqChosen.cloneType, mem_edge.master.endSourceId))
  relaunch_queue.io.enq.valid := false.B
  relaunch_queue.io.enq.bits := rsource
  relaunch_queue.io.deq.ready := false.B
  when(relaunch_queue.io.deq.valid) {
    mem_out.a.valid := true.B
    val rl_id = relaunch_queue.io.deq.bits
    val mem_length = req_cache(rl_id).memoryLength
    val tx_size = Mux(mem_length < maxTxLength.U, OHToUInt(mem_length), lgMaxTxLength.U)
    mem_out.a.bits := mem_edge.Get(
      fromSource = rl_id,
      toAddress = req_cache(rl_id).memAddr,
      lgSize = tx_size
    )._2
    relaunch_queue.io.deq.ready := mem_out.a.ready
  }.otherwise {
    mem_out.a.valid := mem_tx_state === read_send
    mem_out.a.bits := mem_edge.Get(
      fromSource = reqChosen,
      toAddress = totalTx.memoryAddress,
      lgSize = txEmitLengthLg)._2
  }


  when (mem_tx_state === read_idle) {
    when(req.request.fire) {
      totalTx.memoryLength := req.request.bits.len
      totalTx.scratchpadAddress := req.request.bits.scAddr
      totalTx.memoryAddress := req.request.bits.memAddr
      mem_tx_state := read_send
      prevDone := false.B
      perSourceProgress foreach { pss =>
        pss.valid := false.B
        pss.complete := false.B
      }

      // sanity check
      val low_order = req.request.bits.len(lgMaxTxLength - 1, 0)
      val low_order_is_pow_2 = !(low_order & (low_order - 1.U)).orR
      assert(low_order_is_pow_2,
        "Transaction length must be approximately a multiple of the cache block size.\n" +
          "Length detected: %d\n" +
          "Length %% " + maxTxLength + " must be a power of two\n" +
          "Low order bits found: %x\n", req.request.bits.len, low_order)

      val alignment = Mux(req.request.bits.len < maxTxLength.U, req.request.bits.len, maxTxLength.U)
      val alignment_is_power_2 = ((alignment - 1.U) & alignment) === 0.U
      assert(alignment_is_power_2, "Alignment must be a power of two. Found %d\n", alignment)
      val alignmentSubsets = VecInit((0 until req.request.bits.memAddr.getWidth).map(width => req.request.bits.memAddr(width, 0)))
      assert(alignmentSubsets(OHToUInt(alignment)-1.U) === 0.U, "\nAlignment not met!\n Alignment required: %x, Low order bits of address(%x): %x\n",
        alignment,
        req.request.bits.memAddr,
        alignmentSubsets(OHToUInt(alignment)))

    }
  }.elsewhen (mem_tx_state === read_send) {
    when (!relaunch_queue.io.deq.valid) { // whenever relaunch is ongoing, we need to launch txs from elsewhere.
                                          // They have precedence
      val isBelowLimit = totalTx.memoryLength <= maxTxLength.U
      txEmitLengthLg := Mux(isBelowLimit, OHToUInt(totalTx.memoryLength), lgMaxTxLength.U)
      mem_out.a.valid := reqAvailable
      val isBelowPage = totalTx.memoryLength <= pageLength.U
      when(mem_out.a.fire) {
        reqIdleBits(reqChosen) := false.B
        req_cache(reqChosen).scratchpadAddress := totalTx.scratchpadAddress
        req_cache(reqChosen).memoryLength := Mux(isBelowPage, totalTx.memoryLength, pageLength.U)
        req_cache(reqChosen).memAddr := totalTx.memoryAddress

        totalTx.memoryLength := Mux(isBelowPage, 0.U, totalTx.memoryLength - pageLength.U)
        totalTx.scratchpadAddress := totalTx.scratchpadAddress + (loader.spEntriesPerBeat * pageLength / maxTxLength).U
        totalTx.memoryAddress := totalTx.memoryAddress + pageLength.U

        perSourceProgress(reqChosen).progress := totalTx.scratchpadAddress
        perSourceProgress(reqChosen).complete := false.B
        perSourceProgress(reqChosen).valid := true.B

        when(isBelowPage) {
          mem_tx_state := read_process
        }
      }
    }
  }.otherwise {
    when(reqIdleBits.reduce(_ && _)) {
      mem_tx_state := read_idle
    }
  }

  loader.io.cache_block_in.valid := mem_out.d.valid
  loader.io.cache_block_in.bits.dat := mem_out.d.bits.data
  when(req_cache(rsource).memoryLength >= maxTxLength.U) {
    loader.io.cache_block_in.bits.len := maxTxLength.U
  }.otherwise {
    loader.io.cache_block_in.bits.len := req_cache(rsource).memoryLength
  }
  loader.io.cache_block_in.bits.idxBase := req_cache(rsource).scratchpadAddress
  mem_out.d.ready := loader.io.cache_block_in.ready
  loader.io.sp_write_out.ready := true.B


  val mergedStat = {
    // collate progress statistics
    // lower ps has precedence
    def collapseStat(ps1: ProgressStat, ps2: ProgressStat): ProgressStat = {
      val stat = Wire(new ProgressStat)
      stat.valid := false.B
      stat.complete := DontCare
      stat.progress := DontCare
      when(ps1.valid && ps2.valid) {
        val ps1less = ps1.progress < ps2.progress
        when(ps1less && !ps1.complete) {
          stat := ps1
        }.elsewhen(!ps2.complete) {
          stat := ps2
        }.elsewhen(ps1less) {
          stat := ps2
        }.otherwise {
          stat := ps1
        }
      }.elsewhen(ps1.valid) {
        stat := ps1
      }.elsewhen(ps2.valid) {
        stat := ps2
      }

      val r = Reg(new ProgressStat())
      when (req.request.fire) {
        r.valid := false.B
        r.progress := 0.U
        r.complete := false.B
      }.otherwise {
        r := stat
      }
      r

    }

    @tailrec
    def recurseStat(stats: Seq[ProgressStat]): ProgressStat = {
      if (stats.length == 1) stats(0)
      else {
        val groups = stats.grouped(2).map(a => collapseStat(a(0), a(1))).toSeq
        recurseStat(groups)
      }
    }
    val ms = recurseStat(perSourceProgress)
    Mux(ms.valid, ms.progress, 0.U)
  }
  req.progress := mergedStat

  when(mem_out.d.fire) {
    req_cache(rsource).memoryLength := req_cache(rsource).memoryLength - maxTxLength.U
    req_cache(rsource).scratchpadAddress := req_cache(rsource).scratchpadAddress + loader.spEntriesPerBeat.U
    req_cache(rsource).memAddr := req_cache(rsource).memAddr + maxTxLength.U
    loaderSource := mem_out.d.bits.source
    prevProcessed := rsource
    when(req_cache(rsource).memoryLength <= maxTxLength.U) {
      reqIdleBits(rsource) := true.B
      prevDone := true.B
    }.otherwise {
      // transaction has to keep going, emit another segment
      relaunch_queue.io.enq.valid := true.B
      assert(relaunch_queue.io.enq.ready)
      prevDone := false.B
    }
  }

  when(loader.io.sp_write_out.valid) {
    memory.io.mem_en(0) := true.B
    memory.io.we(0) := true.B
    memory.io.din(0) := loader.io.sp_write_out.bits.dat
    memory.io.addr(0) := loader.io.sp_write_out.bits.idx
    access.writeReq.ready := false.B
    perSourceProgress(loaderSource).progress := loader.io.sp_write_out.bits.idx +& 1.U
  }

  // processing a new cache block
  when (loader.io.cache_block_in.ready && !RegNext(loader.io.cache_block_in.ready) && prevDone && reqIdleBits(prevProcessed)) {
    perSourceProgress(prevProcessed).complete := true.B
  }
  if (supportWriteback) {
    require(specialization.isInstanceOf[FlatPackScratchpadParams] && dataWidthBits % 8 == 0)
    val writer = Module(new SequentialWriter(nBytes = dataWidthBits / 8,
      TLClientNode = outer.mem))
    writer.tl_out.a.ready := false.B
    writer.tl_out.d.valid := false.B
    writer.tl_out.d.bits := DontCare
    val wb_idle :: wb_read :: wb_rewind :: wb_write :: Nil = Enum(4)
    val wb_state = RegInit(wb_idle)

    when (wb_state =/= wb_idle) {
      req.request.ready := false.B
      writer.tl_out <> mem_out
    }
    writer.io.req.valid := req.writeback.valid
    req.writeback.ready := writer.io.req.ready
    writer.io.req.bits.len := req.writeback.bits.len
    writer.io.req.bits.addr := req.writeback.bits.memAddr
    val channel = writer.io.channel
    val writebackIdx, written = Reg(UInt(log2Up(nDatas).W))

    channel.data.valid := output_enable && wb_state === wb_read
    channel.data.bits := memory.io.dout(0)
    switch(wb_state) {
      is (wb_idle) {
        when (req.writeback.fire) {
          writebackIdx := req.writeback.bits.scAddr
          written := req.writeback.bits.scAddr
          wb_state := wb_read
        }
      }
      is (wb_read) {
        req.request.ready := false.B
        memory.io.mem_en(0) := true.B
        memory.io.addr := writebackIdx

        when (channel.data.fire) {
          written := written + 1.U
        }
        writebackIdx := writebackIdx + 1.U
        when (!channel.data.ready) {
          wb_state := wb_rewind
        }
        when (writer.io.req.ready) {
          wb_state := wb_write
        }
      }
      is (wb_rewind) {
        req.request.ready := false.B
        when (channel.data.ready) {
          wb_state := wb_read
          writebackIdx := written
        }
        when(writer.io.req.ready) {
          wb_state := wb_write
        }
      }
      is (wb_write) {
        when (channel.channelIdle) {
          wb_state := wb_idle
        }
      }
    }
  } else {
    req.writeback.ready := false.B
    assert(req.writeback.valid === false.B || reset.asBool === true.B,
      "Writeback is disabled in the configuration but the valid signal\n" +
        "is high. Either ensure that the writeback valid signal is never\n" +
        "asserted or that the config promises writeback support.")
  }
}
