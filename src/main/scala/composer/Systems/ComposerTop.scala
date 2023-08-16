package composer.Systems

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import composer._
import composer.Generation._
import composer.RoccHelpers.{AXI4Compat, FrontBusHub}
import composer.Systems.ComposerTop._
import composer.common.CLog2Up
import composer.Generation.Tune.Tunable
import composer.Platforms.{BuildModeKey, FrontBusProtocol, FrontBusProtocolKey, HasDMA}
import freechips.rocketchip.amba.ahb._
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
    val continuity = p(ExtMem).get.master.size
    val baseTotal = (nMemChannels - 1) * continuity
    val amask = getAddressMask(log2Up(p(ExtMem).get.master.size), baseTotal.toLong)
    AddressSet(continuity * ddrChannel, amask)
  }
}

class ComposerTop(implicit p: Parameters) extends LazyModule() {
  private val externalMemParams: MemoryPortParams = p(ExtMem).get
  private val nMemChannels = externalMemParams.nMemoryChannels
  private val device = new MemoryDevice

  val cmd_resp_axilhub = LazyModule(new FrontBusHub())

  // AXI-L Port - commands come through here
  val COMM_IN = p(FrontBusProtocolKey) match {
    case FrontBusProtocol.AXI4 | FrontBusProtocol.AXIL =>
      val axi_master = AXI4MasterNode(Seq(AXI4MasterPortParameters(
        masters = Seq(AXI4MasterParameters(
          name = "S00_AXI",
          aligned = true,
          maxFlight = Some(1),
          id = IdRange(0, 1 << 16)
        )),
      )))
      cmd_resp_axilhub.node.asInstanceOf[AXI4IdentityNode] := axi_master
      axi_master
    case FrontBusProtocol.AHB =>
      val ahb_master = AHBSlaveSourceNode(
        portParams = Seq(AHBMasterPortParameters(
          masters = Seq(AHBMasterParameters(
            "S00_AHB"
          ))
        ))
      )
      cmd_resp_axilhub.node.asInstanceOf[AHBSlaveIdentityNode] := ahb_master
      ahb_master
  }

  // connect axil hub to external axil port
//  cmd_resp_axilhub.node := AXI4Buffer() := COMM_IN
  // connect axil hub to accelerator

  val dummyTL = p.alterPartial({ case TileVisibilityNodeKey => cmd_resp_axilhub.widget.node })


  val acc = LazyModule(new ComposerAccSystem()(dummyTL))

  val AXI_MEM = if (acc.mem.nonEmpty) Some(Seq.tabulate(nMemChannels) { channel_idx =>
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(getAddressSet(channel_idx)),
        resources = device.reg,
        regionType = RegionType.UNCACHED,
        supportsRead = TransferSizes(externalMemParams.master.beatBytes * 16),
        supportsWrite = TransferSizes(externalMemParams.master.beatBytes * 16),
        interleavedId = Some(1)
      )),
      beatBytes = externalMemParams.master.beatBytes
    )))
  }) else None

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

  val composer_mems = acc.mem map { m =>
    val composer_mem = AXI4IdentityNode()
    val DMASourceBits = if (p(HasDMA).isDefined) CLog2Up(p(HasDMA).get) else 0
    val availableComposerSources = 1 << (p(ExtMem).get.master.idBits - DMASourceBits)
    (composer_mem
      := AXI4Buffer()
      := TLToAXI4()
      := TLBuffer()
      := TLSourceShrinker2(availableComposerSources)
      := TLBuffer()
      := m)
    composer_mem
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

  AXI_MEM match {
    case Some(mems) =>
      mem_tops zip mems foreach { case (mt, endpoint) =>
        endpoint :=
          AXI4Buffer() :=
          AXI4UserYanker(capMaxFlight = Some(p(MaxInFlightMemTxsPerSource))) :=
          AXI4Buffer() := mt
      }
    case None => ;
  }



  lazy val module = new TopImpl(this)
}

class TopImpl(outer: ComposerTop) extends LazyModuleImp(outer) {
  val acc = outer.acc
  val axil_hub = outer.cmd_resp_axilhub
  val ocl_port = outer.COMM_IN
  acc.module.io.cmd <> axil_hub.module.io.rocc_in
  axil_hub.module.io.rocc_out <> acc.module.io.resp

  val S00_AXI = p(FrontBusProtocolKey) match {
    case FrontBusProtocol.AXI4 | FrontBusProtocol.AXIL =>
      val port_cast = ocl_port.asInstanceOf[AXI4MasterNode]
      val S00_AXI = IO(Flipped(new AXI4Compat(port_cast.out(0)._1.params)))
      AXI4Compat.connectCompatSlave(S00_AXI, port_cast.out(0)._1)
      S00_AXI
    case FrontBusProtocol.AHB =>
      val port_cast = ocl_port.asInstanceOf[AHBSlaveSourceNode]
      val S00_AHB = IO(Flipped(AHBSlaveBundle(port_cast.out(0)._1.params)))
      S00_AHB.suggestName("S00_AHB")
      S00_AHB <> port_cast.out(0)._1
      S00_AHB
  }

  if (outer.AXI_MEM.isDefined) {
    val dram_ports = outer.AXI_MEM.get
    val M00_AXI = dram_ports.zipWithIndex.map{case (a, idx) =>
      val io = IO(new AXI4Compat(a.in(0)._1.params))
      io.suggestName(s"M0${idx}_AXI")
      io
    }
    val ins = dram_ports.map(_.in(0))
    //  val axi4_mem = IO(HeterogeneousBag.fromNode(dram_ports.in))
    (M00_AXI zip ins) foreach { case (i, (o, _)) => AXI4Compat.connectCompatMaster(i, o) }

//    val read_success = EventPerformanceCounter(M00_AXI.map(axi => axi.rvalid && axi.rready), new NoObjective)
    //    val read_conflict = EventPerformanceCounter(M00_AXI.map(axi => !axi.rready && axi.rvalid), new NoObjective)
    //    val write_request_conflict = EventPerformanceCounter(M00_AXI.map(axi => !axi.awready && axi.awvalid), new NoObjective)
    //    val read_request_conflict = EventPerformanceCounter(M00_AXI.map(axi => !axi.arready && axi.arvalid), new NoObjective)

    // make incoming dma port and connect it
    if (p(HasDMA).isDefined) {
      val dma = IO(Flipped(new AXI4Compat(outer.AXI_MEM.get(0).in(0)._1.params)))
      AXI4Compat.connectCompatSlave(dma, outer.dma_port.get.out(0)._1)
    }

    p(ExtMem) match {
      case None => ;
      case Some(a) => require(M00_AXI(0).rid.getWidth <= a.master.idBits,
        s"Too many ID bits for this platform. Try reducing the\n" +
          s"prefetch length of scratchpads/readers/writers.\n" +
          s"Current width: ${M00_AXI(0).rid.getWidth}\n" +
          s"Required width: ${a.master.idBits}")
    }
  }

  // try to make this the last thing we do
  CppGeneration.genCPPHeader(outer.cmd_resp_axilhub.widget.module.crRegistry, acc.acc)
  ConstraintGeneration.writeConstraints()
  if (p(BuildModeKey).isInstanceOf[BuildMode.Tuning]) {
    Tunable.exportNames()
  }
}
