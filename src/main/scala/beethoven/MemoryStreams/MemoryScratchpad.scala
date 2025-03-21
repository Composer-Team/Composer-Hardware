package beethoven.MemoryStreams

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import beethoven.{ScratchpadConfig, _}
import beethoven.MemoryStreams.Loaders.ScratchpadPackedSubwordLoader
import beethoven.common.{Address, CLog2Up, ShiftReg, splitIntoChunks}
import beethoven.MemoryStreams.MemoryScratchpad.has_warned
import beethoven.MemoryStreams.Readers.{LightweightReader, LightweightReader_small, ReadChannelIO, ReaderModuleIO, SequentialReader}
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
  require(csp.dataWidthBits.intValue() > 0, "The Scratchpad datawidth must be greater than 0.")
  require(csp.nDatas.intValue() > 0, "The scratchpad depth (number of rows) must be greater than 0.")
  val channelWidthBytes = platform.extMem.master.beatBytes

  val blockBytes = p(CacheBlockBytes)
  lazy val module = new ScratchpadImpl(csp, this)
  val forceCustom = true
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
  val dWidth = csp.dataWidthBits.intValue()
  val nPorts = csp.nPorts
  val latency = csp.latency.intValue()
  val specialization = csp.features.specialization
  val supportWriteback = csp.features.supportWriteback

  private val realNRows = Math.max((nDatas.toFloat / datasPerCacheLine).ceil.toInt,
    outer.channelWidthBytes * 8 / dWidth)
  val memoryLengthBits = log2Up(realNRows * dWidth) + 1

  private val maxTxLength = outer.channelWidthBytes

  private val scReqBits = log2Up(nDatas)
  val IOs = Seq.fill(nPorts)(IO(new ScratchpadDataPort(scReqBits, dWidth)))
  val req = IO(new ScratchpadMemReqPort(nDatas))
  private val memory = Seq.fill(datasPerCacheLine)(Memory(latency,
    dataWidth = dWidth,
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
    val memIdxDelay = ShiftReg(memIdx, latency, clock)
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
    io.res.valid := ShiftReg(io.req.valid && !io.req.bits.write_enable, latency, clock)
  }

  require(isPow2(datasPerCacheLine), "The Scratchpad nBanks/datas-per-cache-line parameter must be a power of 2.")
  if (outer.mem_reader.isDefined) {

    val swWordSize = specialization match {
      case psw: PackedSubwordScratchpadConfig =>
        psw.wordSizeBits
      case _ => dWidth
    }
    val loader = Module(specialization match {
      case psw: PackedSubwordScratchpadConfig =>
        require(datasPerCacheLine == 1, "Using packed subword, only assumes nbanks=1 in the Scratchpad")
        new ScratchpadPackedSubwordLoader(dWidth, scReqBits, psw.wordSizeBits, psw.datsPerSubword)
      case _: FlatPackScratchpadConfig =>
        new ScratchpadPackedSubwordLoader(dWidth * datasPerCacheLine,
          scReqBits - CLog2Up(datasPerCacheLine),
          outer.mem_reader.get.out(0)._1.params.dataBits,
          outer.mem_reader.get.out(0)._1.params.dataBits / (dWidth * datasPerCacheLine))
    })

    loader.io.sp_write_out.ready := true.B
    when(loader.io.sp_write_out.valid) {
      val dataSplit = splitIntoChunks(loader.io.sp_write_out.bits.dat, dWidth)
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

    def linearReadInc(): Unit = {
      when(loader.io.cache_block_in.fire) {
        idxCounter := idxCounter + loader.spEntriesPerBeat.U
      }
      when(req.init.fire) {
        idxCounter := req.init.bits.scAddr
      }
      loader.io.cache_block_in.bits.idxBase := idxCounter
    }

    val tx_ready = {
      val reader = if (outer.useLowResourceReader) {
        println("use low resource")
        val reader = Module(new LightweightReader(
          swWordSize * datasPerCacheLine,
          tl_edge = outer.mem_reader.get.out(0)._2,
          tl_bundle = outer.mem_reader.get.out(0)._1))
        reader.tl_out <> outer.mem_reader.get.out(0)._1
        linearReadInc()
        reader.io
      } else if (outer.very_small_sp) {
        println("very small")
        val reader = Module(new LightweightReader_small(
          dWidth = swWordSize * datasPerCacheLine,
          tl_bundle = outer.mem_reader.get.out(0)._1,
          tl_edge = outer.mem_reader.get.out(0)._2,
          sp_sz_bytes = csp.nDatas.intValue() * csp.dataWidthBits.intValue() / 8
        ))
        reader.tl_out <> outer.mem_reader.get.out(0)._1
        linearReadInc()
        reader.io
      } else if (!outer.forceCustom) {
        println("not small")
        val reader = Module(new SequentialReader(
          swWordSize * datasPerCacheLine,
          tl_edge = outer.mem_reader.get.out(0)._2,
          tl_bundle = outer.mem_reader.get.out(0)._1))
        reader.tl_out <> outer.mem_reader.get.out(0)._1
        linearReadInc()
        reader.io
      } else {
        println("else")
        val reader = Wire(Output(new ReadChannelIO(outer.mem_reader.get.out(0)._1.params.dataBits)))
        val (tl_out, tl_edge) = outer.mem_reader.get.out(0)
        val beatBytes = tl_out.params.dataBits / 8
        val beatBytesWidth = CLog2Up(beatBytes)

        val addr = Reg(UInt((tl_out.params.addressBits - beatBytesWidth).W))
        val expectedBeatsLeft = RegInit(0.U((tl_out.params.addressBits - beatBytesWidth).W))

        val bigTxBytes = platform.prefetchSourceMultiplicity * beatBytes

        val sourceIdle = RegInit(VecInit(Seq.fill(tl_edge.client.endSourceId)(true.B)))
        val hasIdle = sourceIdle.asUInt =/= 0.U
        val nextSource = PriorityEncoder(sourceIdle)

        val offsetAcc = Reg(UInt(log2Up(csp.nDatas).W))
        val spadAddrOffsetPerSource = Reg(Vec(tl_edge.client.endSourceId, UInt(log2Up(csp.nDatas).W)))
        val beatsLeftPerSourcee = Reg(Vec(tl_edge.client.endSourceId, UInt(log2Up(platform.prefetchSourceMultiplicity).W)))



        val s_idle :: s_emit :: s_wait :: Nil = Enum(3)
        val state = RegInit(s_idle)

        reader.channel.data.valid := tl_out.d.valid
        reader.channel.data.bits := tl_out.d.bits.data
        tl_out.d.ready := reader.channel.data.ready
        tl_out.a.valid := false.B

        val src = tl_out.d.bits.source
        loader.io.cache_block_in.bits.idxBase := spadAddrOffsetPerSource(src)
        when (tl_out.d.fire) {
          val progress = beatsLeftPerSourcee(src)
          beatsLeftPerSourcee(src) := progress - 1.U
          when (progress === 0.U) {
            sourceIdle(src) := true.B
          }
          spadAddrOffsetPerSource(src) := spadAddrOffsetPerSource(src) + loader.spEntriesPerBeat.U
        }

        when (state === s_idle) {
          when (reader.req.fire) {
            expectedBeatsLeft := reader.req.bits.len >> beatBytesWidth
            offsetAcc := req.init.bits.scAddr
            state := s_emit
            addr := req.init.bits.memAddr.address >> beatBytesWidth
          }
        }.elsewhen(state === s_emit) {
          when (hasIdle && expectedBeatsLeft =/= 0.U) {
            val isBig = expectedBeatsLeft >= platform.prefetchSourceMultiplicity.U
            tl_out.a.valid := true.B
            tl_out.a.bits.opcode := TLMessages.Get
            tl_out.a.bits.address := Cat(addr, 0.U(beatBytesWidth.W))
            tl_out.a.bits.data := DontCare
            tl_out.a.bits.source := nextSource
            tl_out.a.bits.mask := BigInt("1" * beatBytes, radix=2).U
            tl_out.a.bits.size := Mux(isBig, CLog2Up(bigTxBytes).U, CLog2Up(beatBytes).U)
            when (tl_out.a.fire) {

              sourceIdle(nextSource) := false.B
              spadAddrOffsetPerSource(nextSource) := offsetAcc
              beatsLeftPerSourcee(nextSource) := Mux(isBig, (platform.prefetchSourceMultiplicity-1).U, 0.U)

              addr := addr + Mux(isBig, platform.prefetchSourceMultiplicity.U, 1.U)
              offsetAcc := offsetAcc + Mux(isBig, (loader.spEntriesPerBeat * platform.prefetchSourceMultiplicity).U, loader.spEntriesPerBeat.U)
              expectedBeatsLeft := expectedBeatsLeft - Mux(isBig, platform.prefetchSourceMultiplicity.U, 1.U)
            }
          }
          when (expectedBeatsLeft === 0.U) {
            state := s_wait
          }
        }.elsewhen(state === s_wait) {
          when (loader.io.cache_block_in.ready && sourceIdle.asUInt === BigInt("1" * tl_edge.client.endSourceId, radix=2).U) {
            state := s_idle
          }
        }
        reader.req.ready := state === s_idle
        reader.channel.in_progress := state =/= s_idle
        reader
      }

      reader.req.valid := req.init.valid
      reader.req.bits.addr := req.init.bits.memAddr
      reader.req.bits.len := req.init.bits.len
      loader.io.cache_block_in.valid := reader.channel.data.valid
      loader.io.cache_block_in.bits.dat := reader.channel.data.bits
      reader.channel.data.ready := loader.io.cache_block_in.ready

      reader.req.ready
    }

    req.init.ready := tx_ready && loader.io.cache_block_in.ready


    if (supportWriteback) {
      require(specialization.isInstanceOf[FlatPackScratchpadConfig] && dWidth % 8 == 0,
      "Using the assisted writeback in the Scratchpad requires the data to resemble the same restrictions we place on Writers." +
        "The data must be a power-of-two number of bytes and the data layout must be flat.")
      val writer = Module(new SequentialWriter(userBytes = dWidth / 8,
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

      val mem_valid = ShiftReg(memory(0).read_enable(0) && wb_state === wb_read, latency, clock)
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

