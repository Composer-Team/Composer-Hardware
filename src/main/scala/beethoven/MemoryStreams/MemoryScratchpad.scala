package beethoven.MemoryStreams

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import beethoven.{ScratchpadConfig, _}
import beethoven.MemoryStreams.Loaders.ScratchpadPackedSubwordLoader
import beethoven.common.{Address, CLog2Up, ShiftReg, splitIntoChunks}
import beethoven.MemoryStreams.MemoryScratchpad.has_warned
import beethoven.MemoryStreams.Readers.{LightweightReader, LightweightReader_small, SequentialReader}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._

sealed abstract class ScratchpadPort extends Bundle {}

class ScratchpadDataPort(val scReqBits: Int, val dataWidthBits: Int) extends ScratchpadPort {
  val req = Flipped(Decoupled(new Bundle() {
    val addr = UInt(scReqBits.W)
    val data = UInt(dataWidthBits.W)
    val write_enable = Bool()
  }))
  val res = ValidIO(UInt(dataWidthBits.W))

  def write(toAddr: UInt, withData: UInt): Bool = {
    this.req.bits.addr := toAddr
    this.req.bits.data := withData
    this.req.bits.write_enable := true.B
    this.req.valid := true.B
    this.req.fire
  }

  def read(toAddr: UInt): Bool = {
    this.req.bits.addr := toAddr
    this.req.bits.data := DontCare
    this.req.bits.write_enable := false.B
    this.req.valid := true.B
    this.req.fire
  }

  def read(toAddr: UInt, enable: Bool): Bool = {
    when(enable) {
      this.read(toAddr)
    }
    this.req.fire
  }
}

object ScratchpadDataPort {
  def apply(other: ScratchpadDataPort): ScratchpadDataPort = {
    new ScratchpadDataPort(other.scReqBits, other.dataWidthBits)
  }
}

class ScratchpadMemReqPort(nDatas: Int)(implicit p: Parameters) extends Bundle {
  val init, writeback = Flipped(Decoupled(new Bundle() {
    val memAddr = Address()
    val scAddr = UInt(log2Up(nDatas).W)
    val len = UInt(Address.addrBits().W)
  }))
}

object MemoryScratchpad {
  private[beethoven] var has_warned = false
}

/**
 * Parameters that all scratchpad subtypes should support
 *
 * @param dataWidthBits  the granularity of a single scratchpad read/write in bits
 * @param nDatas         number of data items in the scratchpad. Non-zero
 * @param latency        latency of a scratchpad access from the user interface. Current implementation only supports 1 or 2.
 * @param specialization How data is loaded from memory. Choose a specialization from ScratchpadSpecialization
 */
class MemoryScratchpad(csp: ScratchpadConfig)(implicit p: Parameters) extends LazyModule {
  require(csp.dataWidthBits.intValue() > 0)
  require(csp.nDatas.intValue() > 0)
  val channelWidthBytes = platform.extMem.master.beatBytes

  val blockBytes = p(CacheBlockBytes)
  lazy val module = new ScratchpadImpl(csp, this)
  val useLowResourceReader = csp.dataWidthBits.intValue() > channelWidthBytes * 8 * platform.prefetchSourceMultiplicity
  if (useLowResourceReader && !has_warned) {
    has_warned = true
    System.err.println(f"The ${csp.name} scratchpad is using a _very_ wide data bus, preventing use of the high" +
      f" throughput reader modules. To use these, either increase platform prefetch length to ${csp.dataWidthBits.intValue() / channelWidthBytes / 8}" +
      f" or decrease bus width to at most ${channelWidthBytes * 8 * platform.prefetchSourceMultiplicity}.")
  }
  val very_small_sp = (csp.dataWidthBits.intValue() * csp.nDatas.intValue()) <= (channelWidthBytes * 8 * platform.prefetchSourceMultiplicity)
  val mem_reader = if (csp.features.supportMemRequest) {
    Some(TLClientNode(Seq(TLMasterPortParameters.v2(
      masters = Seq(TLMasterParameters.v1(
        name = "ScratchpadRead",
        sourceId = IdRange(0, if (useLowResourceReader || very_small_sp) 1 else platform.defaultReadTXConcurrency),
        supportsProbe = TransferSizes(channelWidthBytes, channelWidthBytes * platform.prefetchSourceMultiplicity),
        supportsGet = TransferSizes(channelWidthBytes, channelWidthBytes * platform.prefetchSourceMultiplicity),
      )),
      channelBytes = TLChannelBeatBytes(channelWidthBytes)))))
  } else None
  val mem_writer = if (csp.features.supportWriteback) {
    Some(TLClientNode(Seq(TLMasterPortParameters.v2(
      masters = Seq(TLMasterParameters.v1(
        name = "ScratchpadWriteback",
        sourceId = IdRange(0, if (!very_small_sp) platform.defaultWriteTXConcurrency else 1),
        supportsProbe = TransferSizes(channelWidthBytes, channelWidthBytes * platform.prefetchSourceMultiplicity),
        supportsPutFull = TransferSizes(channelWidthBytes, channelWidthBytes * platform.prefetchSourceMultiplicity)
      )),
      channelBytes = TLChannelBeatBytes(channelWidthBytes)))))
  } else None
}

class ScratchpadImpl(csp: ScratchpadConfig,
                     outer: MemoryScratchpad) extends LazyModuleImp(outer) {
  val nDatas = csp.nDatas.intValue()
  val datasPerCacheLine = csp.features.nBanks
  val dataWidthBits = csp.dataWidthBits.intValue()
  val nPorts = csp.nPorts
  val latency = csp.latency.intValue()
  val specialization = csp.features.specialization
  val supportWriteback = csp.features.supportWriteback

  private val realNRows = Math.max((nDatas.toFloat / datasPerCacheLine).ceil.toInt,
    outer.channelWidthBytes * 8 / dataWidthBits)
  val memoryLengthBits = log2Up(realNRows * dataWidthBits) + 1

  private val maxTxLength = outer.channelWidthBytes

  private val scReqBits = log2Up(nDatas)
  val IOs = Seq.fill(nPorts)(IO(new ScratchpadDataPort(scReqBits, dataWidthBits)))
  val req = IO(new ScratchpadMemReqPort(nDatas))
  private val memory = Seq.fill(datasPerCacheLine)(Memory(latency,
    dataWidth = dataWidthBits,
    nRows = realNRows,
    debugName = Some(outer.name),
    nReadPorts = if (csp.features.readOnly) nPorts - 1 else 0,
    nWritePorts = 0,
    nReadWritePorts = if (csp.features.readOnly) 1 else nPorts))
  if (csp.features.readOnly) {
    IOs.foreach { io =>
      assert(!io.req.valid || !io.req.bits.write_enable,
        "Read-only scratchpad cannot write from user side. If you require this behavior, do not enable readOnly in scratchpad configuration")
    }
  }
  memory.foreach { mem =>
    mem.clock := clock.asBool
    mem.chip_select.foreach(_ := false.B)
  }

  // datasPerCacheLine stripes a row across multiple BRAMs. This causes the overall depth of the memory to decrease,
  // but increases the width. We can individually access each BRAM so we scrape the lower and higher order access
  // address bits and use them to index into the BRAMs.
  def getHighOrderAddr(addr: UInt): UInt = {
    val highOrderBits = CLog2Up(datasPerCacheLine)
    addr(addr.getWidth - 1, highOrderBits).asUInt
  }

  def getLowOrderAddr(addr: UInt): UInt = {
    val lowOrderBits = CLog2Up(datasPerCacheLine)
    if (lowOrderBits == 0) 0.U else
      addr(lowOrderBits - 1, 0)
  }

  IOs.zipWithIndex.foreach { case (io, portIdx) =>
    val memIdx = getLowOrderAddr(io.req.bits.addr)
    val memIdxDelay = ShiftReg(memIdx, latency)
    memory.zipWithIndex foreach { case (mem, mem_idx) =>
      mem.addr(portIdx) := getHighOrderAddr(io.req.bits.addr)
      mem.chip_select(portIdx) := io.req.valid && memIdx === mem_idx.U
      mem.read_enable(portIdx) := !io.req.bits.write_enable
      mem.write_enable(portIdx) := io.req.bits.write_enable
      mem.data_in(portIdx) := io.req.bits.data
    }
    val datsOut = VecInit(memory.map(_.data_out(portIdx)))
    io.req.ready := true.B
    io.res.bits := datsOut(memIdxDelay)
    io.res.valid := ShiftReg(io.req.valid && !io.req.bits.write_enable, latency)
  }

  require(isPow2(datasPerCacheLine))
  if (outer.mem_reader.isDefined) {
    val loader = Module(specialization match {
      case psw: PackedSubwordScratchpadConfig =>
        require(datasPerCacheLine == 1)
        new ScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, psw.wordSizeBits, psw.datsPerSubword)
      case _: FlatPackScratchpadConfig =>
        new ScratchpadPackedSubwordLoader(dataWidthBits * datasPerCacheLine, scReqBits - CLog2Up(datasPerCacheLine), dataWidthBits * datasPerCacheLine, 1)
    })

    val swWordSize = specialization match {
      case psw: PackedSubwordScratchpadConfig =>
        psw.wordSizeBits
      case _ => dataWidthBits
    }

    loader.io.sp_write_out.ready := true.B
    when(loader.io.sp_write_out.valid) {
      val dataSplit = splitIntoChunks(loader.io.sp_write_out.bits.dat, dataWidthBits)
      memory.zip(dataSplit) foreach { case (mem, dat) =>
        val rwp = mem.getReadWritePortIdx(0)
        mem.addr(rwp) := loader.io.sp_write_out.bits.idx
        mem.chip_select(rwp) := true.B
        mem.read_enable(rwp) := false.B
        mem.write_enable(rwp) := true.B
        mem.data_in(rwp) := dat
      }
    }

    loader.io.cache_block_in.bits.len := maxTxLength.U
    val idxCounter = Reg(UInt(log2Up(realNRows).W))

    val tx_ready = {
      val reader = if (outer.useLowResourceReader)
        Module(new LightweightReader(
          swWordSize * datasPerCacheLine,
          tl_edge = outer.mem_reader.get.out(0)._2,
          tl_bundle = outer.mem_reader.get.out(0)._1))
      else if (outer.very_small_sp)
        Module(new LightweightReader_small(
          dWidth = swWordSize * datasPerCacheLine,
          tl_bundle = outer.mem_reader.get.out(0)._1,
          tl_edge = outer.mem_reader.get.out(0)._2,
          sp_sz_bytes = csp.nDatas.intValue() * csp.dataWidthBits.intValue() / 8
        ))
      else
        Module(new SequentialReader(
          swWordSize * datasPerCacheLine,
          tl_edge = outer.mem_reader.get.out(0)._2,
          tl_bundle = outer.mem_reader.get.out(0)._1))

      reader.tl_out <> outer.mem_reader.get.out(0)._1
      reader.io.req.valid := req.init.valid
      reader.io.req.bits.addr := req.init.bits.memAddr
      reader.io.req.bits.len := req.init.bits.len
      when(req.init.fire) {
        idxCounter := req.init.bits.scAddr
      }
      loader.io.cache_block_in.valid := reader.io.channel.data.valid
      loader.io.cache_block_in.bits.dat := reader.io.channel.data.bits
      loader.io.cache_block_in.bits.idxBase := idxCounter
      reader.io.channel.data.ready := loader.io.cache_block_in.ready
      reader.io.req.ready
    }
    when(loader.io.cache_block_in.fire) {
      idxCounter := idxCounter + loader.spEntriesPerBeat.U
    }

    req.init.ready := tx_ready && loader.io.cache_block_in.ready


    if (supportWriteback) {
      require(specialization.isInstanceOf[FlatPackScratchpadConfig] && dataWidthBits % 8 == 0)
      val writer = Module(new SequentialWriter(userBytes = dataWidthBits / 8,
        tl_outer = outer.mem_writer.get.out(0)._1,
        edge = outer.mem_writer.get.out(0)._2))
      writer.tl_out.a.ready := false.B
      writer.tl_out.d.valid := false.B
      writer.tl_out.d.bits := DontCare
      val wb_idle :: wb_read :: wb_rewind :: Nil = Enum(3)
      val wb_state = RegInit(wb_idle)

      when(wb_state =/= wb_idle) {
        req.init.ready := false.B
        writer.tl_out <> outer.mem_writer.get.out(0)._1
      }
      writer.io.req.valid := req.writeback.valid
      writer.io.req.bits.len := req.writeback.bits.len
      writer.io.req.bits.addr := req.writeback.bits.memAddr
      val channel = writer.io.channel
      val writebackIdx, written = Reg(UInt(log2Up(realNRows).W))

      val mem_valid = ShiftReg(memory(0).read_enable(0) && wb_state === wb_read, latency)
      channel.data.valid := mem_valid && wb_state === wb_read
      channel.data.bits := memory(0).data_out(0)
      when(wb_state =/= wb_idle) {
        IOs foreach (port => port.req.ready := false.B)
      }
      req.writeback.ready := false.B

      switch(wb_state) {
        is(wb_idle) {
          req.writeback.ready := writer.io.req.ready
          when(req.writeback.fire) {
            writebackIdx := req.writeback.bits.scAddr
            written := req.writeback.bits.scAddr
            wb_state := wb_read
          }
        }
        is(wb_read) {
          req.init.ready := false.B
          memory(0).chip_select(0) := true.B
          memory(0).read_enable(0) := true.B
          memory(0).write_enable(0) := false.B
          memory(0).addr(0) := writebackIdx

          when(channel.data.fire) {
            written := written + 1.U
          }
          writebackIdx := writebackIdx + 1.U
          when(!channel.data.ready) {
            wb_state := wb_rewind
          }
        }
        is(wb_rewind) {
          when(writer.io.req.ready && channel.isFlushed) {
            wb_state := wb_idle
          }
          when(channel.data.ready && !mem_valid) {
            wb_state := wb_read
            writebackIdx := written
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
}

