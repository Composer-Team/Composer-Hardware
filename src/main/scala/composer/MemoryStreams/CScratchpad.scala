package composer.MemoryStreams

import chipsalliance.rocketchip.config._


import chisel3._
import chisel3.util._
import composer.MemoryStreams.Loaders.CScratchpadPackedSubwordLoader
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tilelink._

import scala.annotation.tailrec

class CScratchpadAccessBundle(scReqBits: Int, dataWidthBits: Int) extends Bundle {
  // note the flipped
  val readReq = Flipped(Decoupled(UInt(scReqBits.W)))
  val readRes = ValidIO(UInt(dataWidthBits.W))
  val writeReq = Flipped(ValidIO(new Bundle() {
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
  require(1 <= latency && latency <= 3)
  require(dataWidthBits > 0)
  require(nDatas > 0)

  val mem = TLClientNode(Seq(TLMasterPortParameters.v2(
    masters = Seq(TLMasterParameters.v1(
      name = "ScratchpadToMemory",
      sourceId = IdRange(0, 16),
      supportsProbe = TransferSizes(1, p(CacheBlockBytes)),
      supportsGet = TransferSizes(1, p(CacheBlockBytes)),
      supportsPutFull = TransferSizes(1, p(CacheBlockBytes))
    )),
    channelBytes = TLChannelBeatBytes(p(CacheBlockBytes)))))
  val writerNode = if (supportWriteback) Some(TLClientNode(Seq(TLMasterPortParameters.v2(
    masters = Seq(TLMasterParameters.v1(
      name = "ScratchpadWriteback",
      sourceId = IdRange(0, 4),
      supportsPutFull = TransferSizes(1, p(CacheBlockBytes)),
      supportsProbe = TransferSizes(1, p(CacheBlockBytes))
    ))
  )))) else None
  lazy val module = new CScratchpadImp(supportWriteback, dataWidthBits, nDatas, latency, specialization, this)
}

class CScratchpadImp(supportWriteback: Boolean,
                     dataWidthBits: Int,
                     nDatas: Int,
                     latency: Int,
                     specialization: CScratchpadSpecialization,
                     outer: CScratchpad) extends LazyModuleImp(outer) {
  val (mem_out, mem_edge) = outer.mem.out(0)
  val beatSize = mem_edge.manager.beatBytes
  val scReqBits = log2Up(nDatas)
  val lgMaxTxLength = log2Up(p(CacheBlockBytes))

  @tailrec
  private def nestPipeline[T <: Data](a: T, depth: Int): T = {
    if (depth == 0) a
    else nestPipeline(RegNext(a), depth - 1)
  }

  val access = IO(new CScratchpadAccessBundle(scReqBits, dataWidthBits))
  val req = IO(new CScratchpadInitReqIO(mem_out, nDatas))
  req.request.ready := true.B

  val mem = SyncReadMem(nDatas, UInt(dataWidthBits.W))

  private val loader = Module(specialization match {
    case psw: PackedSubwordScratchpadParams =>
      new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, psw.wordSizeBits, psw.datsPerSubword, beatSize)
    case _: FlatPackScratchpadParams =>
      new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, dataWidthBits, 1, beatSize)
  })

  private val read_idle :: read_send :: read_process :: Nil = Enum(3)
  private val mem_tx_state = RegInit(read_idle)
  access.readRes.valid := nestPipeline(access.readReq.valid, latency)
  private val rval = mem.read(access.readReq.bits, access.readReq.valid)
  access.readRes.bits := nestPipeline(rval, latency - 1)
  // in-flight read tx busy bits

  req.request.ready := mem_tx_state === read_idle

  private val reqIdleBits = RegInit(VecInit(Seq.fill(mem_edge.master.endSourceId)(true.B)))
  private val reqAvailable = reqIdleBits.reduce(_ || _)
  private val reqChosen = PriorityEncoder(reqIdleBits)
  // only consider non-busy transactions to be the
  private val req_cache = Reg(Vec(mem_edge.master.endSourceId, new Bundle() {
    val scratchpadAddress = UInt(req.request.bits.scAddr.getWidth.W)
    val memoryLength = UInt(16.W)
  }))

  when(req.request.valid && ((req.request.bits.len & (req.request.bits.len - 1.U)) =/= 0.U)) {
    printf("Len is not pow2: %d\n", req.request.bits.len)
    assert(false.B)
  }

  private val totalTx = Reg(new Bundle {
    val memoryAddress = UInt(req.request.bits.memAddr.getWidth.W)
    val scratchpadAddress = UInt(req.request.bits.scAddr.getWidth.W)
    val memoryLength = UInt(mem_out.params.addressBits.W)
  })

  private val txEmitLengthLg = Wire(UInt(4.W))
  txEmitLengthLg := 0.U
  mem_out.a.bits := mem_edge.Get(
    fromSource = reqChosen,
    toAddress = totalTx.memoryAddress,
    lgSize = txEmitLengthLg)._2
  mem_out.a.valid := mem_tx_state === read_send

  switch(mem_tx_state) {
    is(read_idle) {
      when(req.request.fire) {
        totalTx.memoryLength := req.request.bits.len
        totalTx.scratchpadAddress := req.request.bits.scAddr
        totalTx.memoryAddress := req.request.bits.memAddr
        mem_tx_state := read_send
      }
    }

    is(read_send) {
      val isBelowLimit = totalTx.memoryLength <= p(CacheBlockBytes).U
      txEmitLengthLg := Mux(isBelowLimit, OHToUInt(totalTx.memoryLength), lgMaxTxLength.U)
      mem_out.a.valid := reqAvailable
      when(mem_out.a.fire) {
        reqIdleBits(reqChosen) := false.B
        req_cache(reqChosen).scratchpadAddress := totalTx.scratchpadAddress
        req_cache(reqChosen).memoryLength := Mux(isBelowLimit, totalTx.memoryLength, (1 << lgMaxTxLength).U)
        totalTx.memoryLength := totalTx.memoryLength - (1 << lgMaxTxLength).U
        totalTx.scratchpadAddress := totalTx.scratchpadAddress + (loader.spEntriesPerBeat * (1 << lgMaxTxLength) / beatSize).U
        totalTx.memoryAddress := totalTx.memoryAddress + p(CacheBlockBytes).U
        when(isBelowLimit) {
          mem_tx_state := read_process
        }
      }
    }

    is(read_process) {
      when(reqIdleBits.reduce(_ && _)) {
        mem_tx_state := read_idle
      }
    }
  }
  loader.io.cache_block_in.valid := mem_out.d.valid
  loader.io.cache_block_in.bits.dat := mem_out.d.bits.data
  val rsource = mem_out.d.bits.source
  when(req_cache(rsource).memoryLength >= beatSize.U) {
    loader.io.cache_block_in.bits.len := beatSize.U
  }.otherwise {
    loader.io.cache_block_in.bits.len := req_cache(rsource).memoryLength
  }
  loader.io.cache_block_in.bits.idxBase := req_cache(rsource).scratchpadAddress
  mem_out.d.ready := loader.io.cache_block_in.ready
  loader.io.sp_write_out.ready := true.B
  when(loader.io.cache_block_in.fire) {
    req_cache(rsource).scratchpadAddress := req_cache(rsource).scratchpadAddress + loader.spEntriesPerBeat.U
  }

  /* handle progress monitoring */
  val progressReg = RegInit(0.U((loader.io.sp_write_out.bits.idx.getWidth + 1).W))
  val perSourceProgress = Reg(Vec(mem_edge.master.endSourceId, UInt((1 + scReqBits).W)))
  val loadersActive = Reg(Vec(mem_edge.master.endSourceId, Bool()))
  when (req.request.fire) {
    loadersActive.foreach(_ := false.B)
  }
  when (mem_out.a.fire) {
    loadersActive(reqChosen) := true.B
    perSourceProgress(reqChosen) := totalTx.scratchpadAddress
  }
  val loaderSource = Reg(UInt(mem_out.params.sourceBits.W))
  val lowestActive = PriorityEncoder(loadersActive)
  val haveActive = loadersActive.reduce(_ || _) || !loader.io.cache_block_in.ready

  when (loader.io.cache_block_in.ready && !RegNext(loader.io.cache_block_in.ready)) {
    loadersActive(loaderSource) := !reqIdleBits(loaderSource)
  }

  when (RegNext(haveActive)) {
    progressReg := perSourceProgress(RegNext(lowestActive))
  }

  when(mem_out.d.fire) {
    req_cache(rsource).memoryLength := req_cache(rsource).memoryLength - beatSize.U
    loaderSource := mem_out.d.bits.source
    when(req_cache(rsource).memoryLength <= beatSize.U) {
      reqIdleBits(rsource) := true.B
    }
  }
  req.progress := progressReg
  when(loader.io.sp_write_out.valid) {
    mem.write(loader.io.sp_write_out.bits.idx, loader.io.sp_write_out.bits.dat)
    perSourceProgress(loaderSource) := loader.io.sp_write_out.bits.idx +& 1.U
  }

  when(req.request.fire) {
    progressReg := 0.U
    perSourceProgress.foreach(_ := 0.U)
  }

  when(access.writeReq.valid) {
    mem.write(access.writeReq.bits.addr, access.writeReq.bits.data)
  }
  if (supportWriteback) {
    require(specialization.isInstanceOf[FlatPackScratchpadParams] && dataWidthBits % 8 == 0)
    val writer = Module(new SequentialWriter(nBytes = dataWidthBits / 8,
      TLClientNode = outer.writerNode.get))
    writer.tl_out <> outer.writerNode.get.out(0)._1
    writer.io.req.valid := req.writeback.valid
    req.writeback.ready := writer.io.req.ready
    writer.io.req.bits.len := req.writeback.bits.len
    writer.io.req.bits.addr := req.writeback.bits.memAddr
    val channel = writer.io.channel
    val writebackIdx, written = Reg(UInt(log2Up(nDatas).W))

    val wb_idle :: wb_read :: wb_rewind :: wb_write :: Nil = Enum(4)
    val wb_state = RegInit(wb_idle)
    val read_valid = nestPipeline(wb_state === wb_read, latency)
    channel.data.valid := read_valid
    channel.data.bits := DontCare
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
        val read = nestPipeline(mem.read(writebackIdx), latency - 1)
        channel.data.bits := read

        when (read_valid && channel.data.ready) {
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
  }
}
