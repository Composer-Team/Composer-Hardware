package composer.MemoryStreams

import chipsalliance.rocketchip.config._


import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import chisel3._
import chisel3.util._
import composer.MemoryStreams.TLCacheImpl.{getBitSubsetAsUInt, make_mask}
import freechips.rocketchip.subsystem.{CacheBlockBytes, ExtMem}

import scala.annotation.tailrec

object TLCache {
  def apply(cacheSize: Int,
            nClients: Int,
            associativity: Int = 1,
            idxMask: Option[Long] = None)(implicit p: Parameters): TLCache = {
    val tlcache = LazyModule(new TLCache(cacheSize, associativity = associativity, nClients = nClients, idxMask = idxMask))
    tlcache
  }
}

class TLCache(cacheSize: Int, associativity: Int, nClients: Int,
              idxMask: Option[Long])(implicit p: Parameters) extends LazyModule {

  val mbase = p(ExtMem).get.master.base
  val mmask = p(ExtMem).get.master.size * p(ExtMem).get.nMemoryChannels - 1
  val blockBytes = p(CacheBlockBytes)

  val mem_reqs = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLSlaveParameters.v2(
    Seq(AddressSet(mbase, mmask)),
    regionType = RegionType.UNCACHED,
    supports = TLMasterToSlaveTransferSizes(
      get = TransferSizes(blockBytes),
      putFull = TransferSizes(blockBytes)
    ))),
    beatBytes = blockBytes)))

  val mem_out = TLClientNode(Seq(TLMasterPortParameters.v2(
    masters = Seq(TLMasterParameters.v1("TLCache_mem_req_out_port",
      supportsProbe = TransferSizes(1, blockBytes),
      supportsGet = TransferSizes(1, blockBytes),
      supportsPutFull = TransferSizes(1, blockBytes),
      sourceId = IdRange(0, nClients))),
    channelBytes = TLChannelBeatBytes(blockBytes))))

  lazy val module = new TLCacheImpl(cacheSize, associativity, this, idxMask)
}

object TLCacheImpl {
  @tailrec
  def make_mask(i: Int, acc: Long = 0): Long = {
    if (i == 0) acc
    else make_mask(i - 1, (acc << 1) | 1)
  }

  def getBitSubsetAsUInt(q: UInt, mask: Long): UInt = {
    require(q.getWidth <= 64)
    VecInit((0 until q.getWidth) map { idx => if (((mask >> idx) & 1) == 1) Some(q(idx)) else None } filter (_.isDefined) map (_.get)).asUInt
  }
}

class TLCacheImpl(cacheSize: Int, associativity: Int, outer: TLCache, idxMaskP: Option[Long] = None) extends LazyModuleImp(outer) {
  @tailrec
  private def popCount(a: Long, acc: Int = 0): Int = {
    if (a == 0) acc
    else if ((a & 1) == 1) popCount((a >> 1) & 0x7FFFFFFFFFFFFFFFL, acc + 1)
    else popCount((a >> 1) & 0x7FFFFFFFFFFFFFFFL, acc)
  }

  require(isPow2(associativity))
  val (io_req_out, _) = outer.mem_out.out(0)
  val (io_req_in, _) = outer.mem_reqs.in(0)

  val io_invalidate = IO(Input(Bool()))

  val s_idle :: s_cache_search :: Nil = Enum(2)
  val state = RegInit(s_idle)

  val blockSizeBits = io_req_in.params.dataBits
  val blockSizeBytes = blockSizeBits / 8
  val nRows = cacheSize / blockSizeBytes / associativity
  val cache = Seq.fill(associativity)(SyncReadMem(nRows, UInt(blockSizeBits.W)))
  val addr_bits = io_req_in.a.bits.address.getWidth
  val offset_bits = log2Up(blockSizeBytes)
  val idx_bits = log2Up(nRows)
  val tagbits = addr_bits - idx_bits - offset_bits
  val tags = Reg(Vec(nRows, Vec(associativity, UInt(tagbits.W))))
  val valids = Reg(Vec(nRows, Vec(associativity, Bool())))

  when(reset.asBool) {
    valids foreach (_ foreach (_ := false.B))
  }

  // only do reads. Don't support caching writes
  assert(!(io_req_in.a.valid && io_req_in.a.bits.opcode =/= 4.U))
  // must only recieve aligned reads
  assert(!(io_req_in.a.valid && io_req_in.a.bits.address(log2Up(blockSizeBytes) - 1, 0) =/= 0.U))
  assert(!(io_req_in.a.valid && io_req_in.a.bits.size =/= log2Up(io_req_in.a.bits.data.getWidth >> 3).U),
    "We can only handle single-beat transactions")


  val req_in_cache = Reg(new TLBundleA(io_req_in.a.bits.params))
  val idx_mask = idxMaskP match {
    case Some(a) =>
      require((make_mask(offset_bits) & a) == 0, "Offset bits are statically chosen and cannot overlap with Index")
      require(popCount(a) == idx_bits)
      a
    case _ => make_mask(idx_bits) << offset_bits
  }
  val tag_mask = ~idx_mask & (~make_mask(offset_bits))

  //  println("idx_mask: " + idx_mask.toHexString)
  //  println("tag_mask: " + tag_mask.toHexString)

  val tag = getBitSubsetAsUInt(req_in_cache.address, tag_mask)
  val idx = getBitSubsetAsUInt(req_in_cache.address, idx_mask)
  val tag_hits = tags(idx).map(_ === tag)
  val valid_hits = valids(idx)
  val cache_hits = tag_hits.zip(valid_hits) map { case (th, vh) => th && vh }
  val cache_hit = cache_hits reduce (_ || _)

  val write_counters = if (associativity == 1) VecInit(Seq.fill(nRows)(0.U))
    else Reg(Vec(nRows, UInt(log2Up(associativity).W)))
  if (associativity > 1) {
    when (reset.asBool) {
      write_counters foreach (_ := 0.U)
    }
  }

  val ongoing_txs = Reg(Vec(1 << io_req_in.a.bits.source.getWidth, UInt(idx_bits.W)))
  val cache_hit_ongoing = RegInit(false.B)
  val cache_read_en = WireInit(VecInit(Seq.fill(associativity)(false.B)))
  val cache_read_dat = Reg(UInt(io_req_in.d.bits.data.getWidth.W))
  val cache_read_valid = RegInit(false.B)
  val cache_read = VecInit(cache zip cache_read_en map { case (ch, en) => RegNext(ch.read(idx, en)) })

  // we're ready for another D when master is ready for another Dtx
  io_req_out.d.ready := io_req_in.d.ready
  // we're ready for another A when slave interface is ready and when we're not currently addressing a cached tx
  // not overlapping cache txs is important because if we hit in the cache, then we don't have resources for servicing
  // multiple cache txs
  io_req_in.a.ready := io_req_out.a.ready && state === s_idle && !cache_hit_ongoing

  io_req_out.a.valid := false.B
  io_req_in.d.valid := false.B

  when(RegNext(RegNext(cache_read_en.reduce(_ || _)))) {
    cache_read_valid := true.B
    cache_read_dat := Mux1H(cache_hits, cache_read)
  }
  // always take long-latency access first
  when(io_req_out.d.valid) {
    io_req_in.d.bits := io_req_out.d.bits
    io_req_in.d.valid := true.B
    when(io_req_in.d.ready) {
      val assoc_choose = write_counters(idx)
      tags(idx)(assoc_choose) := tag
      valids(idx)(assoc_choose) := true.B
      if (associativity > 1) write_counters(idx) := write_counters(idx) + 1.U
      (0 until associativity) foreach { assoc =>
        when(assoc_choose === assoc.U) {
          cache(assoc).write(idx, io_req_out.d.bits.data)
        }
      }
    }
  }

  when(io_invalidate) {
    valids.flatten foreach (_ := false.B)
  }

  when(cache_read_valid && !io_req_out.d.valid) {
    io_req_in.d.valid := true.B
    io_req_in.d.bits <> req_in_cache
    io_req_in.d.bits.data := cache_read_dat
    when(io_req_in.d.fire) {
      cache_read_valid := false.B
      cache_hit_ongoing := false.B
    }
  }

  switch(state) {
    is(s_idle) {
      when(io_req_in.a.fire) {
        req_in_cache := io_req_in.a.bits
        state := s_cache_search
      }
    }

    is(s_cache_search) {
      when(cache_hit) {
        state := s_idle
        cache_read_en(OHToUInt(cache_hits)) := true.B
        cache_hit_ongoing := true.B
      }.otherwise {
        io_req_out.a.bits := req_in_cache
        io_req_out.a.valid := true.B
        ongoing_txs(req_in_cache.source) := idx
        when(io_req_out.a.fire) {
          state := s_idle
        }
      }
    }
  }
}
