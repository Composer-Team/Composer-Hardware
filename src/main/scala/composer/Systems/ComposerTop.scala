package composer.Systems

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import composer.Generation.{ConstraintGeneration, CppGeneration, DesignObjective, EventPerformanceCounter, NoObjective, Tunable}
import composer.RoccHelpers.{AXI4Compat, AXILHub}
import composer.Systems.ComposerTop._
import composer._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._

import scala.annotation.tailrec
import scala.language.implicitConversions


object ComposerTop {
  /**
   * Get the address mask given the desired address space size (per DIMM) in bytes and the mask for channel bits
   *
   * @param addrBits  total number of address bits per DIMM
   * @param baseTotal mask for the channel bits - address bits are masked out for this
   * @param idx       DO NOT DEFINE - recursive parameter
   * @param acc       DO NOT DEFINE - recursive parameter
   * @return
   */
  @tailrec
  def getAddressMask(addrBits: Int, baseTotal: Long, idx: Int = 0, acc: Long = 0): Long = {
    if (addrBits == 0) acc
    else if (((baseTotal >> idx) & 1) != 0) getAddressMask(addrBits, baseTotal, idx + 1, acc)
    else getAddressMask(addrBits - 1, baseTotal, idx + 1, acc | (1L << idx))
  }

  def getAddressSet(ddrChannel: Int)(implicit p: Parameters): AddressSet = {

    val nMemChannels = p(ExtMem).get.nMemoryChannels
    // this one is the defuault for rocket chip. Each new cache line (size def by CacheBlockBytes) is on another
    // DIMM. This makes fetching 4 contiguous cache blocks completely parallelized. Should be way faster...
    //    val continuity = p(CacheBlockBytes)
    //  this one splits the DIMMS into contiguous address spaces. Not sure what that's good for...
    //  but it seems anyways that it won't work UNLESS it's like this!
    val continuity = 1L << 34
    val baseTotal = (nMemChannels - 1) * continuity
    val amask = getAddressMask(log2Up(p(ExtMem).get.master.size), baseTotal)
    AddressSet(continuity * ddrChannel, amask)
  }
}

class ComposerTop(implicit p: Parameters) extends LazyModule() {
  private val externalMemParams: MemoryPortParams = p(ExtMem).get
  private val lineSize = p(CacheBlockBytes)
  private val nMemChannels = externalMemParams.nMemoryChannels
  private val device = new MemoryDevice

  // AXI-L Port - commands come through here
  val S00_AXI = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "S00_AXI",
      aligned = true,
      maxFlight = Some(1),
      id = IdRange(0, 1 << 16)
    )),
  )))

  val cmd_resp_axilhub = LazyModule(new AXILHub())

  // connect axil hub to external axil port
  cmd_resp_axilhub.node := AXI4Buffer() := S00_AXI
  // connect axil hub to accelerator

  val dummyTL = p.alterPartial({ case TileVisibilityNodeKey => cmd_resp_axilhub.axil_widget.throughId })


  val acc = LazyModule(new ComposerAccSystem()(dummyTL))

  val AXI_MEM = if (acc.mem.nonEmpty) Some(AXI4SlaveNode(Seq.tabulate(nMemChannels) { channel =>
    AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(getAddressSet(channel)),
        resources = device.reg,
        regionType = RegionType.UNCACHED,
        supportsWrite = TransferSizes(1, lineSize),
        supportsRead = TransferSizes(1, lineSize),
        interleavedId = Some(1)
      )),
      beatBytes = externalMemParams.master.beatBytes)
  })) else None

  val dma_port = if (p(HasDMA).isDefined) {
    val dma_node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "S01_AXI",
        maxFlight = Some(2),
        aligned = true,
        id = IdRange(0, 1 << p(HasDMA).get),
      ))
    )))
    Some(dma_node)
  } else {
    None
  }

  // We have to share shell DDR ports with DMA bus (which is AXI4). Use RocketChip utils to do that instead of the
  // whole shebang with instantiating strange encrypted Xilinx IPs'

  val composer_mems = acc.mem map { case m =>
    val composer_mem = AXI4IdentityNode()
    (composer_mem
      := AXI4Buffer()
      := TLToAXI4()
      := m)
    composer_mem
  }
  val extMemIDBits = p(ExtMem) match {
    case None => 0
    case Some(a) => a.master.idBits
  }
  val mem_tops = if (p(HasDMA).isDefined) {
    val dma_mem_xbar = Seq.fill(nMemChannels)(AXI4Xbar(maxFlightPerId = p(MaxInFlightMemTxsPerSource)))
    val dma = AXI4Xbar()
    dma := AXI4Buffer() := dma_port.get
    dma_mem_xbar zip composer_mems foreach { case (xb, cm) =>
      xb := cm
      xb := dma
    }
    dma_mem_xbar
  } else {
    composer_mems
  }

  mem_tops foreach { mt =>
    AXI_MEM.get :=
      AXI4Buffer() :=
      AXI4UserYanker(capMaxFlight = Some(p(MaxInFlightMemTxsPerSource))) :=
      AXI4IdIndexer(extMemIDBits) :=
      AXI4Buffer() := mt
  }



  lazy val module = new TopImpl(this)
}

class TopImpl(outer: ComposerTop) extends LazyModuleImp(outer) {
  val acc = outer.acc
  val axil_hub = outer.cmd_resp_axilhub
  val ocl_port = outer.S00_AXI
  acc.module.io.cmd <> axil_hub.module.io.rocc_in
  axil_hub.module.io.rocc_out <> acc.module.io.resp

  val S00_AXI = IO(Flipped(new AXI4Compat(ocl_port.out(0)._1.params)))
  AXI4Compat.connectCompatSlave(S00_AXI, ocl_port.out(0)._1)

  if (outer.AXI_MEM.isDefined) {
    val dram_ports = outer.AXI_MEM.get
    val M00_AXI = dram_ports.in.zipWithIndex.map(a => {
      val io = IO(new AXI4Compat(a._1._1.params))
      io.suggestName(s"M0${a._2}_AXI")
      io
    })
    //  val axi4_mem = IO(HeterogeneousBag.fromNode(dram_ports.in))
    (M00_AXI zip dram_ports.in) foreach { case (i, (o, _)) => AXI4Compat.connectCompatMaster(i, o) }

    val read_success = EventPerformanceCounter(M00_AXI.map(axi => axi.rvalid && axi.rready), new NoObjective)
    val read_conflict = EventPerformanceCounter(M00_AXI.map(axi => !axi.rready && axi.rvalid), new NoObjective)
    val write_request_conflict = EventPerformanceCounter(M00_AXI.map(axi => !axi.awready && axi.awvalid), new NoObjective)
    val read_request_conflict = EventPerformanceCounter(M00_AXI.map(axi => !axi.arready && axi.arvalid), new NoObjective)

    // make incoming dma port and connect it
    if (p(HasDMA).isDefined) {
      val dma = IO(Flipped(new AXI4Compat(outer.AXI_MEM.get.in(0)._1.params)))
      AXI4Compat.connectCompatSlave(dma, outer.dma_port.get.out(0)._1)
    }

  }

  // try to make this the last thing we do
  CppGeneration.genCPPHeader(outer.cmd_resp_axilhub.axil_widget.module.crRegistry, acc.acc)
  ConstraintGeneration.writeConstraints()
  Tunable.exportNames()
}