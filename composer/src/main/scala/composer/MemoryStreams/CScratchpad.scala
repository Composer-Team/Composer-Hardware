package composer.MemoryStreams

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.MaximumTransactionLength
import composer.MemoryStreams.Loaders.CScratchpadPackedSubwordLoader
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

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

class CScratchpadInitReq(mem_out: TLBundle, nDatas: Int, upperboundBytesPerData: Int) extends Bundle {
  val memAddr = UInt(mem_out.params.addressBits.W)
  val scAddr = UInt(log2Up(nDatas).W)
  val len = UInt(log2Up(nDatas * upperboundBytesPerData).W)
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
  val maxTxSize = p(MaximumTransactionLength)
  val mem = TLClientNode(Seq(TLMasterPortParameters.v2(
    masters = Seq(TLMasterParameters.v1(
      name = "ScratchpadToMemory",
      sourceId = IdRange(0, 1),
      supportsProbe = TransferSizes(1, maxTxSize),
      supportsGet = TransferSizes(1, maxTxSize),
      supportsPutFull = TransferSizes(1, maxTxSize)
    )),
    channelBytes = TLChannelBeatBytes(maxTxSize))))
  lazy val module = new CScratchpadImp(supportMemRead, supportWriteback, dataWidthBits, nDatas, latency, specialization, this)
}

class CScratchpadImp(supportMemRead: Boolean,
                     supportWrite: Boolean,
                     dataWidthBits: Int,
                     nDatas: Int,
                     latency: Int,
                     specialization: CScratchpadSpecialization,
                     outer: CScratchpad) extends LazyModuleImp(outer) {
  val (mem_out, mem_edge) = outer.mem.out(0)
  val beatSize = mem_edge.manager.beatBytes
  val scReqBits = log2Up(nDatas)

  val scratchpad_access_io = IO(new CScratchpadAccessBundle(supportMemRead, supportWrite, scReqBits, dataWidthBits))
  val scratchpad_req_io = IO(Flipped(Decoupled(new CScratchpadInitReq(mem_out, nDatas,
    specialization match {
      case psw: PackedSubwordScratchpadParams =>
        val c = Math.ceil(psw.wordSizeBits.toFloat / psw.datsPerSubword).toInt
        (c + c % 8) / 8
      case _: FlatPackScratchpadParams =>
        (dataWidthBits + (dataWidthBits % 8)) / 8
    }))))


  val mem = SyncReadMem(nDatas, UInt(dataWidthBits.W))

  if (supportMemRead) {
    val loader = Module(specialization match {
      case psw: PackedSubwordScratchpadParams =>
        new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, psw.wordSizeBits, psw.datsPerSubword, beatSize)
      case _: FlatPackScratchpadParams =>
        new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, dataWidthBits, 1, beatSize)
    })

    val read_idle :: read_send :: read_process :: Nil = Enum(3)
    val read_state = RegInit(read_idle)
    scratchpad_access_io.readRes.get.valid := (if (latency == 1) RegNext(scratchpad_access_io.readReq.get.valid) else
      RegNext(RegNext(scratchpad_access_io.readReq.get.valid)))
    val rval = mem.read(scratchpad_access_io.readReq.get.bits, scratchpad_access_io.readReq.get.valid)
    scratchpad_access_io.readRes.get.bits := (if (latency == 1) rval else RegNext(rval))
    // in-flight read tx busy bits

    scratchpad_req_io.ready := read_state === read_idle

    // only consider non-busy transactions to be the
    val req_cache = Reg(Output(scratchpad_req_io.cloneType))

    when ((scratchpad_req_io.bits.len & (scratchpad_req_io.bits.len - 1.U)) =/= 0.U) {
      printf("Len is not pow2: %d\n", scratchpad_req_io.bits.len)
      assert(false.B)
    }
    mem_out.a.bits := mem_edge.Get(0.U,
      req_cache.bits.memAddr,
      OHToUInt(req_cache.bits.len))._2
    mem_out.a.valid := read_state === read_send

    switch(read_state) {
      is (read_idle) {
        when(scratchpad_req_io.fire) {
          req_cache := scratchpad_req_io
          read_state := read_send
        }
      }

      is(read_send) {
        when (mem_out.a.fire) {
          read_state := read_process
        }
      }

      is (read_process) {
        // see mem_out.d.fire condition for transition out of read_process
      }
    }
    loader.io.cache_block_in.valid := mem_out.d.valid
    loader.io.cache_block_in.bits.dat := mem_out.d.bits.data
    loader.io.cache_block_in.bits.idxBase := req_cache.bits.scAddr
    mem_out.d.ready := loader.io.cache_block_in.ready
    loader.io.sp_write_out.ready := true.B
    when (loader.io.cache_block_in.fire) {
      req_cache.bits.scAddr := req_cache.bits.scAddr + loader.spEntriesPerBeat.U
    }
    when(mem_out.d.fire) {
      req_cache.bits.len := req_cache.bits.len - beatSize.U
      when (req_cache.bits.len <= beatSize.U) {
        read_state := read_idle
      }
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