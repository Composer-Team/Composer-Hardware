package composer.MemoryStreams

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._

abstract class CScratchpadLoader(datOutWidth: Int, idxWidth: Int)(implicit p: Parameters) extends Module {
  val blockSize = p(CacheBlockBytes)
  val io = IO(new Bundle() {
    val cache_block_in = Flipped(Decoupled(new Bundle() {
      val dat = UInt((blockSize * 8).W)
      val len = UInt(idxWidth.W)
      val idxBase = UInt(idxWidth.W)
    }))
    val sp_write_out = Decoupled(new Bundle() {
      val dat = UInt(datOutWidth.W)
      val idx = UInt(idxWidth.W)
    })
  })

  val lengthIncPerCB: Int
}

class CScratchpadPackedSubwordLoader(datOutWidth: Int, idxWidth: Int, wordSizeBits: Int, datsPerSubword: Int)(implicit p: Parameters)
  extends CScratchpadLoader(datOutWidth, idxWidth) {
  require(blockSize * 8 % wordSizeBits == 0)
  val subwordsPerBlock = blockSize * 8 / wordSizeBits

  val subwordCounter = Counter(subwordsPerBlock)
  val datCounter = Counter(datsPerSubword)

  val cacheblock = Reg(UInt((blockSize * 8).W))
  val idxBase = Reg(UInt(idxWidth.W))
  val lenRemainingFromReq = Reg(UInt(idxWidth.W))

  val s_idle :: s_loading :: Nil = Enum(2)
  val state = RegInit(s_idle)
  io.cache_block_in.ready := state === s_idle
  io.sp_write_out.bits := DontCare
  io.sp_write_out.valid := false.B

  override val lengthIncPerCB: Int = datsPerSubword * subwordsPerBlock

  val datSelection = VecInit((0 until datsPerSubword) map { sw_idx =>
    val start = sw_idx * datOutWidth
    val end = (sw_idx + 1) * datOutWidth - 1
    cacheblock(end, start)
  })

  switch(state) {
    is(s_idle) {
      when(io.cache_block_in.fire) {
        state := s_loading
        cacheblock := io.cache_block_in.bits.dat
        idxBase := io.cache_block_in.bits.idxBase
        lenRemainingFromReq := io.cache_block_in.bits.len
        datCounter.reset()
        subwordCounter.reset()
      }
    }

    is(s_loading) {
      io.sp_write_out.valid := true.B
      io.sp_write_out.bits.dat := datSelection(datCounter.value)
      io.sp_write_out.bits.idx := idxBase
      when(io.sp_write_out.fire) {
        lenRemainingFromReq := lenRemainingFromReq - 1.U
        datCounter.inc()
        idxBase := idxBase + 1.U
        when(lenRemainingFromReq === 0.U) {
          state := s_idle
        }
        when(datCounter.value === (datsPerSubword - 1).U) {
          subwordCounter.inc()
          cacheblock := cacheblock >> wordSizeBits
        }
      }
    }
  }
}

class CScratchpadAccessBundle(supportMemRead: Boolean, supportWriteback: Boolean,
                              scReqBits: Int, dataWidthBits: Int) extends Bundle {
  // note the flipped
  val readReq = if (supportMemRead) Some(Flipped(ValidIO(UInt(scReqBits.W)))) else None
  val readRes = if (supportMemRead) Some(ValidIO(UInt(dataWidthBits.W))) else None
  val writeReq = if (supportWriteback) Some(Flipped(ValidIO(new Bundle() {
    val addr = UInt(scReqBits.W)
    val data = UInt(dataWidthBits.W)
  }))) else None
}

class CScratchpadInitReq(mem_out: TLBundle, nDatas: Int) extends Bundle {
  val memAddr = UInt(mem_out.params.addressBits.W)
  val scAddr = UInt(log2Up(nDatas).W)
  val len = UInt(log2Up(nDatas).W)
}


/**
  * Parameters that all scratchpad subtypes should support
  *
  * @param supportMemRead   support initialization from memory. If false, an implementation must tie off the TL interface
  * @param supportWriteback support memory writeback. If false, an implementation must tie off the TL interface
  * @param dataWidthBits    the granularity of a single scratchpad read/write in bits
  * @param nDatas           number of data items in the scratchpad. Non-zero
  * @param latency          latency of a scratchpad access from the user interface. Current implementation only supports 1 or 2.
  * @param maxInFlightTxs   Initialization for different sublocks of the scratchpad may come from far away segments in memory. Multiple in-flight txs may be issued to utilize DDR concurrency.
  * @param specialization   How data is loaded from memory. Choose a specialization from CScratchpadSpecialization
  */
class CScratchpad(supportMemRead: Boolean,
                  supportWriteback: Boolean,
                  dataWidthBits: Int,
                  nDatas: Int,
                  maxInFlightTxs: Int,
                  latency: Int,
                  specialization: CScratchpadSpecialization)(implicit p: Parameters) extends LazyModule {
  require(latency == 1 || latency == 2)
  require(supportMemRead || supportWriteback)
  require(dataWidthBits > 0)
  require(nDatas > 0)
  val blockBytes = p(CacheBlockBytes)
  val mem = TLClientNode(Seq(TLMasterPortParameters.v2(
    masters = Seq(TLMasterParameters.v1(
      name = "ScratchpadToMemory",
      sourceId = IdRange(0, maxInFlightTxs),
      supportsProbe = TransferSizes(1, blockBytes),
      supportsGet = TransferSizes(1, blockBytes),
      supportsPutFull = TransferSizes(1, blockBytes)
    )),
    channelBytes = TLChannelBeatBytes(blockBytes))))
  lazy val module = new CScratchpadImp(supportMemRead, supportWriteback, dataWidthBits, nDatas, maxInFlightTxs, latency, specialization, this)
}

class CScratchpadImp(supportMemRead: Boolean,
                     supportWrite: Boolean,
                     dataWidthBits: Int,
                     nDatas: Int,
                     maxInFlightTxs: Int,
                     latency: Int,
                     specialization: CScratchpadSpecialization,
                     outer: CScratchpad) extends LazyModuleImp(outer) {
  val (mem_out, mem_edge) = outer.mem.out(0)

  val scReqBits = log2Up(nDatas)
  val scratchpad_access_io = IO(new CScratchpadAccessBundle(supportMemRead, supportWrite, scReqBits, dataWidthBits))

  val scratchpad_req_io = IO(Flipped(Decoupled(new CScratchpadInitReq(mem_out, nDatas))))

  val scratchpad_req_idle_io = IO(Output(Bool()))
  scratchpad_req_idle_io := true.B

  val mem = SyncReadMem(nDatas, UInt(dataWidthBits.W))

  if (supportMemRead) {
    val loader = Module(specialization match {
      case psw: PackedSubwordScratchpadParams =>
        new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, psw.wordSizeBits, psw.datsPerSubword)
      case _: FlatPackScratchpadParams =>
        new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, dataWidthBits, 1)
    })

    val read_idle :: read_allocate :: Nil = Enum(2)
    val read_state = RegInit(read_idle)
    scratchpad_access_io.readRes.get.valid := (if (latency == 1) RegNext(scratchpad_access_io.readReq.get.valid) else
      RegNext(RegNext(scratchpad_access_io.readReq.get.valid)))
    val rval = mem.read(scratchpad_access_io.readReq.get.bits, scratchpad_access_io.readReq.get.valid)
    scratchpad_access_io.readRes.get.bits := (if (latency == 1) rval else RegNext(rval))
    // in-flight read tx busy bits
    val busyBits = Reg(Vec(maxInFlightTxs, Bool()))

    scratchpad_req_idle_io := !busyBits.reduce(_ || _)

    val srcToTxInfo = Reg(Vec(maxInFlightTxs, new Bundle() {
      val baseIdx = UInt(scReqBits.W)
      val len = UInt(scReqBits.W)
    }))
    when(reset.asBool) {
      busyBits foreach (_ := false.B)
    }
    // only consider non-busy transactions to be the
    val next_tx = PriorityEncoderOH(busyBits map (!_))
    val tx_id = OHToUInt(next_tx)
    val have_available_tx_slot = busyBits.map(!_).reduce(_ || _)

    val req_cache = Reg(Output(scratchpad_req_io.cloneType))

    scratchpad_req_io.ready := read_state === read_idle && mem_out.a.ready
    mem_out.a.bits := mem_edge.Get(tx_id, req_cache.bits.memAddr, 6.U)._2
    mem_out.a.valid := false.B

    switch(read_state) {
      is(read_idle) {
        when(scratchpad_req_io.fire) {
          req_cache := scratchpad_req_io
          read_state := read_allocate
        }
      }

      is(read_allocate) {
        mem_out.a.valid := have_available_tx_slot
        when(have_available_tx_slot && mem_out.a.ready) {
          busyBits(tx_id) := true.B
          val itemsPerTx = loader.lengthIncPerCB

          srcToTxInfo(tx_id).baseIdx := req_cache.bits.scAddr
          when(req_cache.bits.len < itemsPerTx.U) {
            srcToTxInfo(tx_id).len := (itemsPerTx - 1).U
            read_state := read_idle
          }.otherwise {
            srcToTxInfo(tx_id).len := req_cache.bits.len
            // otherwise we need to allocate more txs because we have a very long read (>1 CB)
            req_cache.bits.scAddr := req_cache.bits.scAddr + itemsPerTx.U
            req_cache.bits.len := req_cache.bits.len - itemsPerTx.U
          }
        }
      }
    }
    loader.io.cache_block_in.valid := mem_out.d.valid
    loader.io.cache_block_in.bits.dat := mem_out.d.bits.data
    loader.io.cache_block_in.bits.idxBase := srcToTxInfo(mem_out.d.bits.source).baseIdx
    loader.io.cache_block_in.bits.len := srcToTxInfo(mem_out.d.bits.source).len
    mem_out.d.ready := loader.io.cache_block_in.ready
    loader.io.sp_write_out.ready := true.B
    when (mem_out.d.fire) {
      busyBits(mem_out.d.bits.source) := false.B
    }
    when(loader.io.sp_write_out.valid) {
      mem.write(loader.io.sp_write_out.bits.idx, loader.io.sp_write_out.bits.dat)
    }
  }
  if (supportWrite) {
    when(scratchpad_access_io.writeReq.get.valid) {
      mem.write(scratchpad_access_io.writeReq.get.bits.addr, scratchpad_access_io.writeReq.get.bits.data)
    }
  }

}