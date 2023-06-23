package composer.MemoryStreams

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import composer._
import composer.Generation._
import composer.MemoryStreams.Loaders.CScratchpadPackedSubwordLoader
import composer.common.{splitIntoChunks, CLog2Up, ShiftReg}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._

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

class CScratchpadInitReqIO(mem_out: Option[TLBundle], nDatas: Int, memLenBits: Int) extends Bundle {
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
class MemoryScratchpad(supportWriteback: Boolean,
                       supportMemRequest: Boolean,
                       dataWidthBits: Number,
                       nDatas: Number,
                       latency: Number,
                       nPorts: Int,
                       specialization: CScratchpadSpecialization,
                       datasPerCacheLine: Int)(implicit p: Parameters) extends LazyModule {
  require(dataWidthBits.intValue() > 0)
  require(nDatas.intValue() > 0)
  val channelWidthBytes = p(ExtMem).get.master.beatBytes

  val blockBytes = p(CacheBlockBytes)
  lazy val module = new CScratchpadImp(
    supportWriteback,
    dataWidthBits.intValue(),
    nDatas.intValue(),
    latency.intValue(),
    nPorts,
    specialization,
    datasPerCacheLine,
    this)
  val mem_master_node = if (supportMemRequest) {
    require(dataWidthBits.intValue() <= channelWidthBytes * 8 * p(PrefetchSourceMultiplicity))
    Some(TLClientNode(Seq(TLMasterPortParameters.v2(
      masters = Seq(TLMasterParameters.v1(
        name = "ScratchpadToMemory",
        sourceId = IdRange(0, 4),
        supportsProbe = TransferSizes(1, channelWidthBytes * p(PrefetchSourceMultiplicity)),
        supportsGet = TransferSizes(1, channelWidthBytes * p(PrefetchSourceMultiplicity)),
        supportsPutFull = TransferSizes(1, channelWidthBytes * p(PrefetchSourceMultiplicity))
      )),
      channelBytes = TLChannelBeatBytes(channelWidthBytes)))))
  } else None

  dataWidthBits match {
    case a: BaseTunable if !a.isIdentity =>
      println(s"dataWidthBits is base tunable. Range is ${a.range._1}, ${a.range._2}")
    case _ => ;
  }
}

class CScratchpadImp(supportWriteback: Boolean,
                     dataWidthBits: Int,
                     nDatas: Int,
                     latency: Int,
                     nPorts: Int,
                     specialization: CScratchpadSpecialization,
                     datasPerCacheLine: Int,
                     outer: MemoryScratchpad) extends LazyModuleImp(outer) {
  private val realNRows = Math.max((nDatas.toFloat / datasPerCacheLine).ceil.toInt, outer.channelWidthBytes * 8 / dataWidthBits)
  val memoryLengthBits = log2Up(realNRows * dataWidthBits) + 1

  private val maxTxLength = outer.channelWidthBytes

  private val scReqBits = log2Up(nDatas)
  val IOs = Seq.fill(nPorts)(IO(new CScratchpadAccessPort(scReqBits, dataWidthBits)))
  val req = IO(new CScratchpadInitReqIO(if (outer.mem_master_node.isDefined) Some(outer.mem_master_node.get.out(0)._1) else None, nDatas, memoryLengthBits))
  private val memory = Seq.fill(datasPerCacheLine)(CMemory(latency, dataWidth = dataWidthBits, nRows = realNRows, debugName = Some(outer.name), nPorts = nPorts))
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
    dontTouch(io.res.bits)
    dontTouch(datsOut)
    dontTouch(memIdxDelay)
    memory.foreach(dontTouch(_))
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
        mem.addr(0) := loader.io.sp_write_out.bits.idx
        mem.data_in(0) := dat
        mem.chip_select(0) := true.B
        mem.read_enable(0) := false.B
        mem.write_enable(0) := true.B
      }
    }

    loader.io.cache_block_in.bits.len := maxTxLength.U
    val reader = Module(new CReader(swWordSize * datasPerCacheLine / 8, 1, tlclient = outer.mem_master_node.get, debugName = Some(s"ScratchpadReader")))
    reader.tl_out <> outer.mem_master_node.get.out(0)._1
    val idxCounter = Reg(UInt(log2Up(realNRows).W))
    reader.io.req.valid := req.request.valid
    reader.io.req.bits.addr := req.request.bits.memAddr
    reader.io.req.bits.len := req.request.bits.len
    when (req.request.fire) {
      printf("Request info: addr(%x) len(%x)\n", req.request.bits.memAddr, req.request.bits.len)
      idxCounter := req.request.bits.scAddr
    }
    loader.io.cache_block_in.valid := reader.io.channel.data.valid
    loader.io.cache_block_in.bits.dat := reader.io.channel.data.bits(0)
    loader.io.cache_block_in.bits.idxBase := idxCounter
    reader.io.channel.data.ready := loader.io.cache_block_in.ready
    when (loader.io.cache_block_in.fire) {
      idxCounter := idxCounter + loader.spEntriesPerBeat.U
    }

    req.request.ready := reader.io.req.ready && loader.io.cache_block_in.ready


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
        writer.tl_out <> outer.mem_master_node.get.out(0)._1
      }
      writer.io.req.valid := req.writeback.valid
      req.writeback.ready := writer.io.req.ready
      writer.io.req.bits.len := req.writeback.bits.len
      writer.io.req.bits.addr := req.writeback.bits.memAddr
      val channel = writer.io.channel
      val writebackIdx, written = Reg(UInt(log2Up(realNRows).W))

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
  }
}

