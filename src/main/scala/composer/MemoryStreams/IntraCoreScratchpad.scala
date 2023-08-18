package composer.MemoryStreams

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import composer.Generation._
import composer.Platforms.{ASICMemoryCompilerKey, PlatformType, PlatformTypeKey}
import composer.common.ShiftReg
import composer.Generation.Tune.Tunable
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
                          readOnly: Boolean,
                          nPorts: Int)(implicit p: Parameters) extends LazyModule {
  require(dataWidthBits.intValue() > 0)
  require(nDatas.intValue() > 0)
  lazy val module = new IntraCoreScratchpadImp(
    dataWidthBits.intValue(),
    nDatas.intValue(),
    latency.intValue(),
    nPorts,
    readOnly,
    this)
  val channelWidthBytes = p(ExtMem).get.master.beatBytes
  val blockBytes = p(CacheBlockBytes)
  val mem_slave_node = TLManagerNode(Seq(asMemorySlave))

  dataWidthBits match {
    case a: Tunable if !a.isIdentity =>
      println(s"dataWidthBits is base tunable. Range is ${a.range._1}, ${a.range._2}")
    case _ => ;
  }
}

class IntraCoreScratchpadImp(dataWidthBits: Int,
                             nDatas: Int,
                             latency: Int,
                             nPorts: Int,
                             readOnly: Boolean,
                             outer: IntraCoreScratchpad) extends LazyModuleImp(outer) {
  private val scReqBits = log2Up(nDatas)
  val IOs = Seq.fill(nPorts)(IO(new ScratchpadDataPort(scReqBits, dataWidthBits)))
  val (in, edge) = outer.mem_slave_node.in(0)
  val responseQ = Queue(in.a.map(_.source), entries = 4)
  private val realNRows = nDatas
  private val memory = Memory(latency, dataWidth = dataWidthBits, nRows = realNRows, debugName = Some(outer.name),
    nReadPorts = if (readOnly) nPorts - 1 else 0,
    nWritePorts = 0,
    nReadWritePorts = if (readOnly) 1 else nPorts + 1)

  if (readOnly) {
    IOs.foreach { io =>
      assert(!io.req.valid || (io.req.valid && !io.req.bits.write_enable), "read only scratchpad. turn off readonly if you need to write to the scratchpad")
    }
  }


  val write_port = memory.getReadWritePortIdx(0)

  memory.clock := clock.asBool
  if (readOnly) {
    IOs.zipWithIndex.foreach { case (io, idx) =>
      val port = idx
      memory.addr(port) := io.req.bits.addr
      memory.chip_select(port) := io.req.valid
      memory.read_enable(port) := true.B
      memory.write_enable(port) := false.B
      memory.data_in(port) := DontCare
      io.res.valid := ShiftReg(io.req.valid, latency)
      io.res.bits := memory.data_out(port)
    }
  } else {
    memory.addr(write_port) := DontCare
    memory.chip_select(write_port) := false.B
    memory.read_enable(write_port) := false.B
    memory.write_enable(write_port) := false.B
    memory.data_in(write_port) := DontCare

    IOs.zipWithIndex.foreach { case(io, idx) =>
      val port = memory.getReadWritePortIdx(idx + 1)
      memory.addr(port) := io.req.bits.addr
      memory.chip_select(port) := io.req.valid
      memory.read_enable(port) := !io.req.bits.write_enable
      memory.write_enable(port) := io.req.bits.write_enable
      memory.data_in(port) := io.req.bits.data
      io.res.valid := ShiftReg(io.req.valid && !io.req.bits.write_enable, latency)
      io.res.bits := memory.data_out(port)
    }
  }

  in.d <> responseQ.map { source => edge.AccessAck(source, log2Up(in.params.dataBits / 8).U) }

  when(in.a.valid) {
    IOs.foreach {
      _.req.ready := false.B
    }
    val off = log2Up(dataWidthBits / 8)
    val bot_addr = in.a.bits.address(log2Up(nDatas) - 1 + off, off)
    memory.addr(write_port) := bot_addr
    memory.chip_select(write_port) := true.B
    memory.read_enable(write_port) := false.B
    memory.write_enable(write_port) := true.B
    memory.data_in(write_port) := in.a.bits.data
  }
}

