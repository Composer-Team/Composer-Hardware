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

  val blockBytes = p(CacheBlockBytes)
//  val unCachedReaders = coreParams.memoryChannelParams.filter(_.isInstanceOf[CReadChannelParams]).map { para =>
//    val param: CReadChannelParams = para.asInstanceOf[CReadChannelParams]
//    (param.name, List.tabulate(para.nChannels) { i =>
//      TLClientNode(List(TLMasterPortParameters.v1(
//        clients = List(TLMasterParameters.v1(
//          name = s"ReadChannel_sys${system_id}_core${core_id}_${para.name}$i",
//          supportsGet = TransferSizes(1, blockBytes * p(PrefetchSourceMultiplicity)),
//          supportsProbe = TransferSizes(1, blockBytes * p(PrefetchSourceMultiplicity)),
//          sourceId = IdRange(0, param.maxInFlightTxs)
//        )))))
//    })

  val mem_slave_node = if (asMemorySlave.isDefined) Some(TLManagerNode(Seq(asMemorySlave.get))) else None
  val mem_master_node = if (asMemorySlave.isEmpty) {
    require(dataWidthBits.intValue() <= channelWidthBytes*8)
    Some(TLClientNode(Seq(TLMasterPortParameters.v2(
      masters = Seq(TLMasterParameters.v1(
        name = "ScratchpadToMemory",
        sourceId = IdRange(0, 4),
        supportsProbe = TransferSizes(1, blockBytes * p(PrefetchSourceMultiplicity)),
        supportsGet = TransferSizes(1, blockBytes * p(PrefetchSourceMultiplicity)),
        supportsPutFull = TransferSizes(1, blockBytes * p(PrefetchSourceMultiplicity))
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
  private val realNRows = Math.max(nDatas, outer.channelWidthBytes*8/dataWidthBits)
  private val scReqBits = log2Up(realNRows)

  private val maxTxLength = outer.channelWidthBytes
//  private val lgMaxTxLength = log2Up(maxTxLength)
//
//  private val pageLength = p(DRAMBankBytes)

  val mostPortsSupported = p(PlatformTypeKey) match {
    case PlatformType.FPGA => 2
    case PlatformType.ASIC => p(ASICMemoryCompilerKey).mems.keys.max
  }

  val nDuplicates = (nPorts.toFloat / mostPortsSupported).ceil.toInt

  private val memory = Seq.fill(nDuplicates)(CMemory(latency, dataWidth = dataWidthBits, nRows = realNRows, debugName = Some(outer.name), nPorts = mostPortsSupported))

  val IOs = Seq.fill(nPorts)(IO(new CScratchpadAccessPort(scReqBits, dataWidthBits)))

  val memoryLengthBits = log2Up(nDatas * outer.channelWidthBytes) + 1

  val req = IO(new CScratchpadInitReqIO(if (outer.mem_master_node.isDefined) Some(outer.mem_master_node.get.out(0)._1) else None, nDatas, memoryLengthBits))

  IOs.grouped(mostPortsSupported) zip memory foreach{ case (access_group, mem) =>
    mem.clock := clock.asBool
    access_group.indices.foreach { port_idx =>
      val port = access_group(port_idx)
      mem.addr(port_idx) := port.req.bits.addr
      mem.chip_select(port_idx) := port.req.valid
      mem.read_enable(port_idx) := !port.req.bits.write_enable
      mem.write_enable(port_idx) := port.req.bits.write_enable
      mem.data_in(port_idx) := port.req.bits.data
      port.res.valid := ShiftReg(port.req.valid && !port.req.bits.write_enable, latency)
      port.res.bits := mem.data_out(port_idx)
    }
    (access_group.length until mem.nPorts) foreach { port_idx =>
      mem.addr(port_idx) := DontCare
      mem.chip_select(port_idx) := false.B
      mem.read_enable(port_idx) := DontCare
      mem.write_enable(port_idx) := DontCare
      mem.data_in(port_idx) := DontCare
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
        new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, psw.wordSizeBits, psw.datsPerSubword)
      case _: FlatPackScratchpadParams =>
        new CScratchpadPackedSubwordLoader(dataWidthBits, scReqBits, dataWidthBits, 1)
    })

    val swWordSize = specialization match {
      case psw: PackedSubwordScratchpadParams =>
        psw.wordSizeBits
      case _ => dataWidthBits
    }

    loader.io.sp_write_out.ready := true.B
    when (loader.io.sp_write_out.valid) {
      memory.foreach { mem =>
        mem.addr(0) := loader.io.sp_write_out.bits.idx
        mem.data_in(0) := loader.io.sp_write_out.bits.dat
        mem.chip_select(0) := true.B
        mem.read_enable(0) := false.B
        mem.write_enable(0) := true.B
      }
    }

    loader.io.cache_block_in.bits.len := maxTxLength.U
    val reader = Module(new CReader(swWordSize / 8, 1, tlclient = outer.mem_master_node.get, debugName = Some(s"ScratchpadReader")))
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

    val lenRemainingInReq = RegInit(0.U(req.request.bits.len.getWidth.W))
    when (req.request.fire) {
      lenRemainingInReq := req.request.bits.len
    }
    if (loader.spEntriesPerBeat == 1) {
      when (loader.io.sp_write_out.fire) {
        lenRemainingInReq := lenRemainingInReq - (swWordSize / 8).U
      }
    } else {
      val beatCount = RegInit(0.U(log2Up(loader.spEntriesPerBeat).W))
      when (loader.io.sp_write_out.fire) {
        beatCount := beatCount + 1.U
        when (beatCount === (loader.spEntriesPerBeat - 1).U) {
          beatCount := 0.U
          lenRemainingInReq := lenRemainingInReq - (swWordSize / 8).U
        }
      }
    }
    req.request.ready := lenRemainingInReq === 0.U


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
  } else {
    require(outer.mem_slave_node.isDefined)
    val (in, edge) = outer.mem_slave_node.get.in(0)

    val responseQ = Queue(in.a.map(_.source), entries = 4)
    in.d <> responseQ.map { source => edge.AccessAck(source, log2Up(in.params.dataBits / 8).U) }

    when(in.a.valid) {
      IOs.foreach { _.req.ready := false.B }
      val off = log2Up(dataWidthBits / 8)
      val bot_addr = in.a.bits.address(log2Up(nDatas) - 1 + off, off)
      val mem_idx = mostPortsSupported - 1
      memory foreach { mem =>
        mem.write_enable(mem_idx) := true.B
        mem.read_enable(mem_idx) := false.B
        mem.chip_select(mem_idx) := true.B
        // trim off the bottom bits that need to be removed due to alignment.
        mem.addr(mem_idx) := bot_addr
        mem.data_in(mem_idx) := in.a.bits.data
      }
    }
  }
}

