package composer.MemoryStreams

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import composer._
import composer.Generation._
import composer.MemoryStreams.Loaders.CScratchpadPackedSubwordLoader
import composer.common.{splitIntoChunks, CLog2Up, ShiftReg}
import composer.Generation.Tune._
import composer.Platforms.{PlatformType, PlatformTypeKey}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._

sealed abstract class CScratchpadPort extends Bundle {}

class ScratchpadDataPort(val scReqBits: Int, val dataWidthBits: Int) extends CScratchpadPort {
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
    when (enable) {
      this.read(toAddr)
    }
    this.req.fire
  }
}

class ScratchpadMemReqPort(mem_out: Option[TLBundle], nDatas: Int, memLenBits: Int) extends Bundle {
  val init, writeback = Flipped(Decoupled(new Bundle() {
    val memAddr = UInt(if (mem_out.isDefined) mem_out.get.params.addressBits.W else 1.W)
    val scAddr = UInt(log2Up(nDatas).W)
    val len = UInt(memLenBits.W)
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
class MemoryScratchpad(csp: CScratchpadParams)(implicit p: Parameters) extends LazyModule {
  require(csp.dataWidthBits.intValue() > 0)
  require(csp.nDatas.intValue() > 0)
  val channelWidthBytes = p(ExtMem).get.master.beatBytes

  val blockBytes = p(CacheBlockBytes)
  lazy val module = new ScratchpadImpl(csp, this)
  val mem_master_node = if (csp.features.supportMemRequest) {
    require(csp.dataWidthBits.intValue() <= channelWidthBytes * 8 * p(PrefetchSourceMultiplicity))
    Some(TLClientNode(Seq(TLMasterPortParameters.v2(
      masters = Seq(TLMasterParameters.v1(
        name = "ScratchpadToMemory",
        sourceId = IdRange(0, InstanceTunable(2, (1, 4), Some(csp.name + "ScratchpadReaderSources"))),
        supportsProbe = TransferSizes(1, channelWidthBytes * p(PrefetchSourceMultiplicity)),
        supportsGet = TransferSizes(1, channelWidthBytes * p(PrefetchSourceMultiplicity)),
        supportsPutFull = TransferSizes(1, channelWidthBytes * p(PrefetchSourceMultiplicity))
      )),
      channelBytes = TLChannelBeatBytes(channelWidthBytes)))))
  } else None

  csp.dataWidthBits match {
    case a: Tunable if !a.isIdentity =>
      println(s"dataWidthBits is base tunable. Range is ${a.range._1}, ${a.range._2}")
    case _ => ;
  }
}

class ScratchpadImpl(csp: CScratchpadParams,
                     outer: MemoryScratchpad) extends LazyModuleImp(outer) {
  val nDatas = csp.nDatas.intValue()
  val datasPerCacheLine = csp.features.nBanks
  val dataWidthBits = csp.dataWidthBits.intValue()
  val nPorts = csp.nPorts
  val latency = csp.latency.intValue()
  val specialization = csp.features.specialization
  val supportWriteback = csp.features.supportWriteback

  private val realNRows = Math.max((nDatas.toFloat / datasPerCacheLine).ceil.toInt, outer.channelWidthBytes * 8 / dataWidthBits)
  val memoryLengthBits = log2Up(realNRows * dataWidthBits) + 1

  private val maxTxLength = outer.channelWidthBytes

  private val scReqBits = log2Up(nDatas)
  val IOs = Seq.fill(nPorts)(IO(new ScratchpadDataPort(scReqBits, dataWidthBits)))
  val req = IO(new ScratchpadMemReqPort(if (outer.mem_master_node.isDefined) Some(outer.mem_master_node.get.out(0)._1) else None, nDatas, memoryLengthBits))
  private val memory = Seq.fill(datasPerCacheLine)(CMemory(latency,
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
    addr(addr.getWidth-1, highOrderBits).asUInt
  }

  def getLowOrderAddr(addr: UInt): UInt = {
    val lowOrderBits = CLog2Up(datasPerCacheLine)
    if (lowOrderBits == 0) 0.U else
      addr(lowOrderBits - 1, 0)
  }

  IOs.zipWithIndex.foreach { case(io, portIdx) =>
    val memIdx = getLowOrderAddr(io.req.bits.addr)
    val memIdxDelay = ShiftReg(memIdx, latency)
    memory.zipWithIndex foreach { case(mem, mem_idx) =>
      mem.addr(portIdx) := getHighOrderAddr(io.req.bits.addr)
      mem.chip_select(portIdx) := io.req.valid && memIdx === mem_idx.U
      mem.read_enable(portIdx) := !io.req.bits.write_enable
      mem.write_enable(portIdx) := io.req.bits.write_enable
      mem.data_in(portIdx) := io.req.bits.data
    }
    val datsOut = VecInit(memory.map(_.data_out(portIdx)))
    io.res.bits := datsOut(memIdxDelay)
    io.res.valid := ShiftReg(io.req.valid && !io.req.bits.write_enable, latency)
  }

  require(isPow2(datasPerCacheLine))
  if (outer.mem_master_node.isDefined) {
    val loader = Module(specialization match {
      case psw: PackedSubwordScratchpadParams =>
        require(datasPerCacheLine == 1)
        new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, psw.wordSizeBits, psw.datsPerSubword)
      case _: FlatPackScratchpadParams =>
        new CScratchpadPackedSubwordLoader(dataWidthBits * datasPerCacheLine, scReqBits - CLog2Up(datasPerCacheLine), dataWidthBits * datasPerCacheLine, 1)
    })

    val swWordSize = specialization match {
      case psw: PackedSubwordScratchpadParams =>
        psw.wordSizeBits
      case _ => dataWidthBits
    }

    loader.io.sp_write_out.ready := true.B
    when (loader.io.sp_write_out.valid) {
      val dataSplit = splitIntoChunks(loader.io.sp_write_out.bits.dat, dataWidthBits)
      memory.zip(dataSplit) foreach { case(mem, dat) =>
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

    val tx_ready = p(PlatformTypeKey) match {
      case PlatformType.FPGA =>
        val reader = Module(new CReader(swWordSize * datasPerCacheLine / 8, 1, tlclient = outer.mem_master_node.get, debugName = Some(s"Scratchpad${csp.name}"))(p.alterPartial {
          case PrefetchSourceMultiplicity => 32
        }))
        reader.tl_out <> outer.mem_master_node.get.out(0)._1
        reader.io.req.valid := req.init.valid
        reader.io.req.bits.addr := req.init.bits.memAddr
        reader.io.req.bits.len := req.init.bits.len
        when(req.init.fire) {
          idxCounter := req.init.bits.scAddr
        }
        loader.io.cache_block_in.valid := reader.io.channel.data.valid
        loader.io.cache_block_in.bits.dat := reader.io.channel.data.bits(0)
        loader.io.cache_block_in.bits.idxBase := idxCounter
        reader.io.channel.data.ready := loader.io.cache_block_in.ready
        reader.io.req.ready
      case PlatformType.ASIC =>
        val (port, edge) = outer.mem_master_node.get.out(0)
        val tx_len_remaining = Reg(UInt(req.init.bits.len.getWidth.W))
        val tx_base_addr = Reg(UInt(req.init.bits.memAddr.getWidth.W))
        val tx_init_index = Reg(UInt(req.init.bits.scAddr.getWidth.W))
        val n_sources = edge.master.endSourceId
        val big_tx_size = 1 << 10
        val small_tx_size = edge.master.channelBytes.d.get
        val tx_max_beats = big_tx_size / small_tx_size
        val scratchpad_indices_per_beat: Int = small_tx_size / csp.dataWidthBits.intValue()
        require(small_tx_size >= csp.dataWidthBits.intValue() / 8, "ASIC platform currently only supports scratchpads up to width (" + small_tx_size + "), ")

        class PerSourceTx extends Bundle {
          val active = Bool()
          val base_addr = UInt(req.init.bits.memAddr.getWidth.W)
          val init_index = UInt(req.init.bits.scAddr.getWidth.W)
          val beats_remaining_minus_one = UInt(log2Up(tx_max_beats).W) // only handle 1KB txs at a time
        }
        val source_txs = Reg(Vec(n_sources, new PerSourceTx()))

        when(req.init.fire) {
          tx_len_remaining := req.init.bits.len
          tx_base_addr := req.init.bits.memAddr
          tx_init_index := req.init.bits.scAddr
        }

        val is_tx_available = source_txs.map(!_.active).reduce(_ || _)
        val chosen_tx = PriorityEncoder(source_txs.map(!_.active))
        val is_big = tx_len_remaining >= (1 << 10).U
        val tx_alloc_sz = Mux(is_big, big_tx_size.U, small_tx_size.U)
        loader.io.cache_block_in.bits.idxBase := DontCare
        when(is_tx_available) {
          val tx = source_txs(chosen_tx)
          tx.base_addr := tx_base_addr
          tx.beats_remaining_minus_one := Mux(is_big, (tx_max_beats - 1).U, 0.U)
          tx.init_index := tx_init_index
          port.a.bits := edge.Get(chosen_tx,
            tx_base_addr,
            Mux(is_big, log2Up(big_tx_size).U, log2Up(small_tx_size).U)
          )._2
          when(tx_len_remaining > 0.U) {
            port.a.valid := true.B
            when(port.a.ready) {
              tx_base_addr := tx_base_addr + tx_alloc_sz
              tx_init_index := tx_init_index + Mux(is_big, (tx_max_beats * scratchpad_indices_per_beat).U, scratchpad_indices_per_beat.U)
              tx_len_remaining := tx_len_remaining - tx_alloc_sz
              tx.active := true.B
            }
          }
        }
        port.d.ready := loader.io.cache_block_in.ready
        loader.io.cache_block_in.valid := port.d.valid
        loader.io.cache_block_in.bits.dat := port.d.bits.data
        when (port.d.fire) {
          val tx = source_txs(port.d.bits.source)
          loader.io.cache_block_in.bits.idxBase := tx.init_index
          tx.init_index := tx.init_index + scratchpad_indices_per_beat.U
          tx.beats_remaining_minus_one := tx.beats_remaining_minus_one - 1.U
          when (tx.beats_remaining_minus_one === 0.U) {
            tx.active := false.B
          }
        }

        when(reset.asBool) {
          source_txs.foreach(_.active := false.B)
          tx_len_remaining := 0.U
        }

        RegNext(tx_len_remaining === 0.U && source_txs.map(!_.active).reduce(_ && _))
    }
    when(loader.io.cache_block_in.fire) {
      idxCounter := idxCounter + loader.spEntriesPerBeat.U
    }

    req.init.ready := tx_ready && loader.io.cache_block_in.ready


    if (supportWriteback) {
      require(specialization.isInstanceOf[FlatPackScratchpadParams] && dataWidthBits % 8 == 0)
      val writer = Module(new SequentialWriter(nBytes = dataWidthBits / 8,
        TLClientNode = outer.mem_master_node.get))
      writer.tl_out.a.ready := false.B
      writer.tl_out.d.valid := false.B
      writer.tl_out.d.bits := DontCare
      val wb_idle :: wb_read :: wb_rewind :: Nil = Enum(3)
      val wb_state = RegInit(wb_idle)

      when(wb_state =/= wb_idle) {
        req.init.ready := false.B
        writer.tl_out <> outer.mem_master_node.get.out(0)._1
      }
      writer.io.req.valid := req.writeback.valid
      writer.io.req.bits.len := req.writeback.bits.len
      writer.io.req.bits.addr := req.writeback.bits.memAddr
      val channel = writer.io.channel
      val writebackIdx, written = Reg(UInt(log2Up(realNRows).W))

      val mem_valid = ShiftReg(memory(0).read_enable(0) && wb_state === wb_read, latency)
      channel.data.valid := mem_valid && wb_state === wb_read
      channel.data.bits := memory(0).data_out(0)
      when (wb_state =/= wb_idle) {
        IOs foreach (port => port.req.ready := false.B )
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
          when(writer.io.req.ready && channel.channelIdle) {
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

