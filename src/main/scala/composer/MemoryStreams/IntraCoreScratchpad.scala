package composer.MemoryStreams

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import composer._
import composer.Generation._
import composer.common.ShiftReg
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._

class MemWritePort(addrBits: Int, dataBits: Int) extends DecoupledIO(new Bundle() {
  val data = UInt(dataBits.W)
  val addr = UInt(addrBits.W)
})

/**
 * Parameters that all scratchpad subtypes should support
 *
 * @param dataWidthBits  the granularity of a single scratchpad read/write in bits
 * @param nDatas         number of data items in the scratchpad. Non-zero
 * @param latency        latency of a scratchpad access from the user interface. Current implementation only supports 1 or 2.
 * @param specialization How data is loaded from memory. Choose a specialization from CScratchpadSpecialization
 */
class IntraCoreScratchpad(asMemorySlave: TLSlavePortParameters,
                          dataWidthBits: Number,
                          nDatas: Number,
                          latency: Number,
                          nPorts: Int)(implicit p: Parameters) extends LazyModule {
  require(dataWidthBits.intValue() > 0)
  require(nDatas.intValue() > 0)
  lazy val module = new IntraCoreScratchpadImp(
    dataWidthBits.intValue(),
    nDatas.intValue(),
    latency.intValue(),
    nPorts,
    this)
  val channelWidthBytes = p(ExtMem).get.master.beatBytes
  val blockBytes = p(CacheBlockBytes)
  val mem_slave_node = TLManagerNode(Seq(asMemorySlave))

  dataWidthBits match {
    case a: BaseTunable if !a.isIdentity =>
      println(s"dataWidthBits is base tunable. Range is ${a.range._1}, ${a.range._2}")
    case _ => ;
  }
}

class IntraCoreScratchpadImp(dataWidthBits: Int,
                             nDatas: Int,
                             latency: Int,
                             nPorts: Int,
                             outer: IntraCoreScratchpad) extends LazyModuleImp(outer) {
  val mostPortsSupported = p(PlatformTypeKey) match {
    case PlatformType.FPGA => 2
    case PlatformType.ASIC => p(ASICMemoryCompilerKey).mems.keys.max
  }
  val nDuplicates = (nPorts.toFloat / mostPortsSupported).ceil.toInt
  val IOs = Seq.fill(nPorts)(IO(new CScratchpadAccessPort(scReqBits, dataWidthBits)))
  val (in, edge) = outer.mem_slave_node.in(0)
  val responseQ = Queue(in.a.map(_.source), entries = 4)
  private val realNRows = nDatas

  IOs.grouped(mostPortsSupported) zip memory foreach { case (access_group, mem) =>
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
      when(acc.req.fire) {
        assert(!acc.req.bits.write_enable, "currently don't support writeback into >2 ported memories")
      }
    }
  }
  private val scReqBits = log2Up(nDatas)
  private val memory = Seq.fill(nDuplicates)(CMemory(latency, dataWidth = dataWidthBits, nRows = realNRows, debugName = Some(outer.name), nPorts = mostPortsSupported))
  in.d <> responseQ.map { source => edge.AccessAck(source, log2Up(in.params.dataBits / 8).U) }

  when(in.a.valid) {
    IOs.foreach {
      _.req.ready := false.B
    }
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

