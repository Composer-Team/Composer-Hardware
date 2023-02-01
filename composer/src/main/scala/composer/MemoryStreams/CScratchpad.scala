package composer.MemoryStreams

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.MaximumTransactionLength
import composer.MemoryStreams.Loaders.CScratchpadPackedSubwordLoader
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class CScratchpadAccessBundle(supportWriteback: Boolean,
                              scReqBits: Int, dataWidthBits: Int) extends Bundle {
  // note the flipped
  val readReq = Flipped(ValidIO(UInt(scReqBits.W)))
  val readRes = ValidIO(UInt(dataWidthBits.W))
  val writeReq = if (supportWriteback) Some(Flipped(ValidIO(new Bundle() {
    val addr = UInt(scReqBits.W)
    val data = UInt(dataWidthBits.W)
  }))) else None
}

class CScratchpadInitReqIO(mem_out: TLBundle, nDatas: Int, upperboundBytesPerData: Int) extends Bundle {
  val progress = Output(UInt((log2Up(nDatas)+1).W))
  val request = Flipped(Decoupled(new Bundle() {
    val memAddr = UInt(mem_out.params.addressBits.W)
    val scAddr = UInt(log2Up(nDatas).W)
    val len = UInt((log2Up(nDatas * upperboundBytesPerData)+1).W)
  }))
}


/**
  * Parameters that all scratchpad subtypes should support
  *
  * @param supportMemRead   support initialization from memory. If false, an implementation must tie off the TL interface
  * @param supportWrite support memory writeback. If false, an implementation must tie off the TL interface
  * @param dataWidthBits    the granularity of a single scratchpad read/write in bits
  * @param nDatas           number of data items in the scratchpad. Non-zero
  * @param latency          latency of a scratchpad access from the user interface. Current implementation only supports 1 or 2.
  * @param specialization   How data is loaded from memory. Choose a specialization from CScratchpadSpecialization
  */
class CScratchpad(supportMemRead: Boolean,
                  supportWrite: Boolean,
                  dataWidthBits: Int,
                  nDatas: Int,
                  latency: Int,
                  specialization: CScratchpadSpecialization)(implicit p: Parameters) extends LazyModule {
  require(1 <= latency && latency <= 3)
  require(supportMemRead || supportWrite)
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
  lazy val module = new CScratchpadImp(supportMemRead, supportWrite, dataWidthBits, nDatas, latency, specialization, this)
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

  def nestPipeline[T <: Data](a: T, depth: Int): T = {
    if (depth == 0) a
    else RegNext(a)
  }

  val access = IO(new CScratchpadAccessBundle(supportWrite, scReqBits, dataWidthBits))
  val req = if (supportMemRead) {
    Some(IO(new CScratchpadInitReqIO(mem_out, nDatas,
      specialization match {
        case psw: PackedSubwordScratchpadParams =>
          val c = Math.ceil(psw.wordSizeBits.toFloat / psw.datsPerSubword).toInt
          (c + c % 8) / 8
        case _: FlatPackScratchpadParams =>
          (dataWidthBits + (dataWidthBits % 8)) / 8
      })))
  } else None


  val mem = SyncReadMem(nDatas, UInt(dataWidthBits.W))

  if (supportMemRead) {
    val scratchpad_req_io = req.get
    val loader = Module(specialization match {
      case psw: PackedSubwordScratchpadParams =>
        new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, psw.wordSizeBits, psw.datsPerSubword, beatSize)
      case _: FlatPackScratchpadParams =>
        new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, dataWidthBits, 1, beatSize)
    })

    val read_idle :: read_send :: read_process :: Nil = Enum(3)
    val read_state = RegInit(read_idle)
    access.readRes.valid := nestPipeline(access.readReq.valid, latency)
    val rval = mem.read(access.readReq.bits, access.readReq.valid)
    access.readRes.bits := nestPipeline(rval, latency-1)
    // in-flight read tx busy bits

    scratchpad_req_io.request.ready := read_state === read_idle

    // only consider non-busy transactions to be the
    val req_cache = Reg(new Bundle() {
      val memoryAddress = UInt(scratchpad_req_io.request.bits.memAddr.getWidth.W)
      val scratchpadAddress = UInt(scratchpad_req_io.request.bits.scAddr.getWidth.W)
      val memoryLength = UInt(scratchpad_req_io.request.bits.len.getWidth.W)
    })

    when (scratchpad_req_io.request.valid && ((scratchpad_req_io.request.bits.len & (scratchpad_req_io.request.bits.len - 1.U)) =/= 0.U)) {
      printf("Len is not pow2: %d\n", scratchpad_req_io.request.bits.len)
      assert(false.B)
    }
    mem_out.a.bits := mem_edge.Get(0.U,
      req_cache.memoryAddress,
      OHToUInt(req_cache.memoryLength))._2
    mem_out.a.valid := read_state === read_send

    switch(read_state) {
      is (read_idle) {
        when(scratchpad_req_io.request.fire) {
          req_cache.memoryLength := scratchpad_req_io.request.bits.len
          req_cache.scratchpadAddress := scratchpad_req_io.request.bits.scAddr
          req_cache.memoryAddress := scratchpad_req_io.request.bits.memAddr
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
    when (req_cache.memoryLength >= beatSize.U) {
      loader.io.cache_block_in.bits.len := beatSize.U
    }.otherwise {
      loader.io.cache_block_in.bits.len := req_cache.memoryLength
    }
    loader.io.cache_block_in.bits.idxBase := req_cache.scratchpadAddress
    mem_out.d.ready := loader.io.cache_block_in.ready
    loader.io.sp_write_out.ready := true.B
    when (loader.io.cache_block_in.fire) {
      req_cache.scratchpadAddress := req_cache.scratchpadAddress + loader.spEntriesPerBeat.U
    }
    when(mem_out.d.fire) {
      req_cache.memoryLength := req_cache.memoryLength - beatSize.U
      when (req_cache.memoryLength <= beatSize.U) {
        read_state := read_idle
      }
    }
    when(loader.io.sp_write_out.valid) {
      mem.write(loader.io.sp_write_out.bits.idx, loader.io.sp_write_out.bits.dat)
    }
    val progressReg = RegInit(0.U((loader.io.sp_write_out.bits.idx.getWidth + 1).W))
    req.get.progress := progressReg
    when (loader.io.sp_write_out.fire) {
      progressReg := loader.io.sp_write_out.bits.idx +& 1.U
    }
    when (req.get.request.valid) {
      progressReg := req.get.request.bits.scAddr
    }
  }
  if (supportWrite) {
    when(access.writeReq.get.valid) {
      mem.write(access.writeReq.get.bits.addr, access.writeReq.get.bits.data)
    }
  }
}
