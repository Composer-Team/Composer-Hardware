package composer.MemoryStreams

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import composer._
import composer.Generation._
import composer.MemoryStreams.Loaders.CScratchpadPackedSubwordLoader
import composer.common.ShiftReg
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._

import scala.annotation.tailrec

sealed abstract class CScratchpadPort extends Bundle {}

class CScratchpadAccessPort(val scReqBits: Int, val dataWidthBits: Int) extends CScratchpadPort {
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

class MemWritePort(addrBits: Int, dataBits: Int) extends DecoupledIO(new Bundle() {
  val data = UInt(dataBits.W)
  val addr = UInt(addrBits.W)
})

class CScratchpadInitReqIO(mem_out: Option[TLBundle], nDatas: Int, memLenBits: Int) extends Bundle {
  val progress = Output(UInt((log2Up(nDatas) + 1).W))
  val request, writeback = Flipped(Decoupled(new Bundle() {
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
class CScratchpad(supportWriteback: Boolean,
                  asMemorySlave: Option[TLSlavePortParameters],
                  dataWidthBits: Number,
                  nDatas: Number,
                  latency: Number,
                  nPorts: Int,
                  specialization: CScratchpadSpecialization)(implicit p: Parameters) extends LazyModule {
  require(dataWidthBits.intValue() > 0)
  require(nDatas.intValue() > 0)
  val channelWidthBytes = p(ExtMem).get.master.beatBytes

  private val biggestTxBytes = dataWidthBits.intValue() / 8 * nDatas.intValue()
  private val concurrentPages = Math.min(16, Math.max(biggestTxBytes / 4096, 1))

  val mem_slave_node = if (asMemorySlave.isDefined) Some(TLManagerNode(Seq(asMemorySlave.get))) else None
  val mem_master_node = if (asMemorySlave.isEmpty) {
    require(dataWidthBits.intValue() <= channelWidthBytes*8)
    Some(TLClientNode(Seq(TLMasterPortParameters.v2(
      masters = Seq(TLMasterParameters.v1(
        name = "ScratchpadToMemory",
        sourceId = IdRange(0, concurrentPages),
        supportsProbe = TransferSizes(channelWidthBytes),
        supportsGet = TransferSizes(channelWidthBytes),
        supportsPutFull = TransferSizes(channelWidthBytes)
      )),
      channelBytes = TLChannelBeatBytes(channelWidthBytes)))))
  } else None

  lazy val module = new CScratchpadImp(supportWriteback, asMemorySlave = asMemorySlave.isDefined, dataWidthBits.intValue(), nDatas.intValue(), latency.intValue(), nPorts, specialization, this)

  dataWidthBits match {
    case a: BaseTunable if !a.isIdentity =>
      println(s"dataWidthBits is base tunable. Range is ${a.range._1}, ${a.range._2}")
    case _ => ;
  }
}

class CScratchpadImp(supportWriteback: Boolean,
                     asMemorySlave: Boolean,
                     dataWidthBits: Int,
                     nDatas: Int,
                     latency: Int,
                     nPorts: Int,
                     specialization: CScratchpadSpecialization,
                     outer: CScratchpad) extends LazyModuleImp(outer) {
  private val scReqBits = log2Up(nDatas)

  private val maxTxLength = outer.channelWidthBytes
  private val lgMaxTxLength = log2Up(maxTxLength)

  private val pageLength = p(DRAMBankBytes)

  val mostPortsSupported = p(PlatformTypeKey) match {
    case PlatformType.FPGA => 2
    case PlatformType.ASIC => p(ASICMemoryCompilerKey).mems.keys.max
  }

  val nDuplicates = (nPorts.toFloat / mostPortsSupported).ceil.toInt

  private val memory = Seq.fill(nDuplicates)(CMemory(latency, dataWidth = dataWidthBits, nRows = nDatas, debugName = Some(outer.name), nPorts = mostPortsSupported))

  val IOs = Seq.fill(nPorts)(IO(new CScratchpadAccessPort(scReqBits, dataWidthBits)))

  val memoryLengthBits = log2Up(nDatas * outer.channelWidthBytes) + 1

  val req = IO(new CScratchpadInitReqIO(if (outer.mem_master_node.isDefined) Some(outer.mem_master_node.get.out(0)._1) else None, nDatas, memoryLengthBits))

  IOs.grouped(mostPortsSupported) zip memory foreach{ case (access_group, mem) =>
    mem.clock := clock
    
    val port1 = access_group(0)
    mem.addr(0) := port1.req.bits.addr
    mem.chip_select(0) := port1.req.valid
    mem.read_enable(0) := !port1.req.bits.write_enable
    mem.write_enable(0) := port1.req.bits.write_enable
    mem.data_in(0) := port1.req.bits.data
    port1.res.valid := ShiftReg(port1.req.valid && !port1.req.bits.write_enable, latency)
    port1.res.bits := mem.data_out(0)

    if (access_group.length > 1) {
      val port2 = access_group(1)
      mem.addr(1) := port2.req.bits.addr
      mem.chip_select(1) := port2.req.valid
      mem.read_enable(1) := !port2.req.bits.write_enable
      mem.write_enable(1) := port2.req.bits.write_enable
      mem.data_in(1) := port2.req.bits.data
      port2.res.valid := ShiftReg(port2.req.valid && !port2.req.bits.write_enable, latency)
      port2.res.bits := mem.data_out(1)
    }
  }

  if (nPorts > mostPortsSupported) {
    IOs foreach { acc =>
      when (acc.req.fire) {
        assert(!acc.req.bits.write_enable, "currently don't support writeback into >2 ported memories")
      }
    }
  }

  if (outer.mem_master_node.isDefined) {

    val loader = Module(specialization match {
      case psw: PackedSubwordScratchpadParams =>
        new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, psw.wordSizeBits, psw.datsPerSubword, maxTxLength)
      case _: FlatPackScratchpadParams =>
        new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, dataWidthBits, 1, maxTxLength)
    })


    val (mem_out, mem_edge) = outer.mem_master_node.get.out(0)

    // given alignment, all bottom bits should be zeroed. Don't need to store them
    val (memoryAddrBitsFAlign, memoryAddrShAmt) = {
      val defaultBits = mem_out.params.addressBits
      val alignment = log2Up(outer.channelWidthBytes) - 1
      (defaultBits - alignment, alignment)
    }

    def chopAddr(a: UInt): UInt = {
      require(a.getWidth == mem_out.params.addressBits)
      a(mem_out.params.addressBits - 1, memoryAddrShAmt)
    }

    def realignAddr(a: UInt): UInt = {
      require(a.getWidth == memoryAddrBitsFAlign)
      Cat(a, 0.U(memoryAddrShAmt.W))
    }

    val read_idle :: read_send :: read_process :: Nil = Enum(3)
    val mem_tx_state = RegInit(read_idle)

    // in-flight read tx busy bits

    req.request.ready := mem_tx_state === read_idle

    val reqIdleBits = RegInit(VecInit(Seq.fill(mem_edge.master.endSourceId)(true.B)))
    val reqAvailable = reqIdleBits.reduce(_ || _)
    val reqChosen = PriorityEncoder(reqIdleBits)
    // only consider non-busy transactions to be the
    val req_cache = Reg(Vec(mem_edge.master.endSourceId, new Bundle() {
      val scratchpadAddress = UInt(req.request.bits.scAddr.getWidth.W)
      val memoryLength = UInt(memoryLengthBits.W)
      val memAddr = UInt(memoryAddrBitsFAlign.W)
    }))
    /* handle progress monitoring */

    class ProgressStat extends Bundle {
      val progress = UInt((1 + scReqBits).W)
      val valid = Bool()
      val complete = Bool()
    }

    val perSourceProgress = Reg(Vec(mem_edge.master.endSourceId, new ProgressStat()))
    val loaderSource = Reg(UInt(mem_out.params.sourceBits.W))

    val totalTx = Reg(new Bundle {
      val memoryAddress = UInt(memoryAddrBitsFAlign.W)
      val scratchpadAddress = UInt(req.request.bits.scAddr.getWidth.W)
      val memoryLength = UInt(memoryLengthBits.W)
    })

    val txEmitLengthLg = Wire(UInt(4.W))
    txEmitLengthLg := 0.U

    // relaunch transactions at bank granularity
    val rsource = mem_out.d.bits.source
    val prevProcessed = Reg(rsource.cloneType)
    val prevDone = Reg(Bool())
    val relaunch_queue = Module(new Queue[UInt](reqChosen.cloneType, mem_edge.master.endSourceId, flow = false, useSyncReadMem = true))
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
        toAddress = realignAddr(req_cache(rl_id).memAddr),
        lgSize = tx_size
      )._2
      relaunch_queue.io.deq.ready := mem_out.a.ready
    }.otherwise {
      mem_out.a.valid := mem_tx_state === read_send
      mem_out.a.bits := mem_edge.Get(
        fromSource = reqChosen,
        toAddress = realignAddr(totalTx.memoryAddress),
        lgSize = txEmitLengthLg)._2
    }


    when(mem_tx_state === read_idle) {
      when(req.request.fire) {
        totalTx.memoryLength := req.request.bits.len
        totalTx.scratchpadAddress := req.request.bits.scAddr
        totalTx.memoryAddress := chopAddr(req.request.bits.memAddr)
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
        assert(alignmentSubsets(OHToUInt(alignment) - 1.U) === 0.U, "\nAlignment not met!\n Alignment required: %x, Low order bits of address(%x): %x, len: %x\n",
          alignment,
          req.request.bits.memAddr,
          alignmentSubsets(OHToUInt(alignment)),
          req.request.bits.len)

      }
    }.elsewhen(mem_tx_state === read_send) {
      when(!relaunch_queue.io.deq.valid) { // whenever relaunch is ongoing, we need to launch txs from elsewhere.
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
          totalTx.memoryAddress := chopAddr(realignAddr(totalTx.memoryAddress) + pageLength.U)

          perSourceProgress(reqChosen).progress := totalTx.scratchpadAddress
          perSourceProgress(reqChosen).complete := false.B
          perSourceProgress(reqChosen).valid := true.B

          when(isBelowPage) {
            mem_tx_state := read_process
          }
        }
      }
    }.otherwise {
      when(reqIdleBits.reduce(_ && _) && loader.io.cache_block_in.ready) {
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
        when(req.request.fire) {
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
          val groups = stats.grouped(2).map(a => {
            if (a.length == 1) a(0)
            else collapseStat(a(0), a(1))
          }).toSeq
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
      req_cache(rsource).memAddr := chopAddr(realignAddr(req_cache(rsource).memAddr) + maxTxLength.U)
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
      memory.foreach { mem =>
        mem.chip_select(0) := true.B
        mem.write_enable(0) := true.B
        mem.read_enable(0) := false.B
        mem.data_in(0) := loader.io.sp_write_out.bits.dat
        mem.addr(0) := loader.io.sp_write_out.bits.idx
      }
      IOs.foreach { _.req.ready := false.B }
      perSourceProgress(loaderSource).progress := loader.io.sp_write_out.bits.idx +& 1.U
    }

    // processing a new cache block
    when(loader.io.cache_block_in.ready && !RegNext(loader.io.cache_block_in.ready) && prevDone && reqIdleBits(prevProcessed)) {
      perSourceProgress(prevProcessed).complete := true.B
    }
    if (supportWriteback) {
      require(specialization.isInstanceOf[FlatPackScratchpadParams] && dataWidthBits % 8 == 0)
      val writer = Module(new SequentialWriter(nBytes = dataWidthBits / 8,
        TLClientNode = outer.mem_master_node.get))
      writer.tl_out.a.ready := false.B
      writer.tl_out.d.valid := false.B
      writer.tl_out.d.bits := DontCare
      val wb_idle :: wb_read :: wb_rewind :: wb_write :: Nil = Enum(4)
      val wb_state = RegInit(wb_idle)

      when(wb_state =/= wb_idle) {
        req.request.ready := false.B
        writer.tl_out <> mem_out
      }
      writer.io.req.valid := req.writeback.valid
      req.writeback.ready := writer.io.req.ready
      writer.io.req.bits.len := req.writeback.bits.len
      writer.io.req.bits.addr := req.writeback.bits.memAddr
      val channel = writer.io.channel
      val writebackIdx, written = Reg(UInt(log2Up(nDatas).W))

      channel.data.valid := ShiftReg(memory(0).read_enable(0), latency) && wb_state === wb_read
      channel.data.bits := memory(0).data_out(0)
      when (wb_state =/= wb_idle) {
        IOs foreach (port => port.req.ready := false.B )
      }

      switch(wb_state) {
        is(wb_idle) {
          when(req.writeback.fire) {
            writebackIdx := req.writeback.bits.scAddr
            written := req.writeback.bits.scAddr
            wb_state := wb_read
          }
        }
        is(wb_read) {
          req.request.ready := false.B
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
          when(writer.io.req.ready) {
            wb_state := wb_write
          }
        }
        is(wb_rewind) {
          req.request.ready := false.B
          when(channel.data.ready) {
            wb_state := wb_read
            writebackIdx := written
          }
          when(writer.io.req.ready) {
            wb_state := wb_write
          }
        }
        is(wb_write) {
          when(channel.channelIdle) {
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
  } else {
    require(outer.mem_slave_node.isDefined)
    val (in, edge) = outer.mem_slave_node.get.in(0)

    val responseQ = Queue(in.a.map(_.source), entries = 4)
    in.d <> responseQ.map { source => edge.AccessAck(source, log2Up(in.params.dataBits / 8).U) }

    when(in.a.valid) {
      IOs.foreach { _.req.ready := false.B }
      val off = log2Up(dataWidthBits / 8)
      val bot_addr = in.a.bits.address(log2Up(nDatas) - 1 + off, off)

      memory foreach { mem =>
        mem.write_enable(0) := true.B
        mem.read_enable(0) := false.B
        mem.chip_select(0) := true.B
        // trim off the bottom bits that need to be removed due to alignment.
        mem.addr(0) := bot_addr
        mem.data_in(0) := in.a.bits.data
      }
    }
  }
}

