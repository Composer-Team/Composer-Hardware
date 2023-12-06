package composer.Systems

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import composer._
import composer.Generation._
import composer.RoccHelpers.FrontBusHub
import composer.Systems.ComposerTop._
import composer.common.CLog2Up
import composer.Generation.Tune.Tunable
import composer.Platforms.{BuildModeKey, FrontBusAddressBits, FrontBusProtocol, FrontBusProtocolKey, HasDMA, PlatformType, PlatformTypeKey}
import composer.Protocol.{ACE, AXI4Compat}
import composer.TLManagement.{TLSourceShrinkerDynamic, TLSourceShrinkerDynamicBlocking, TLToAXI4SRW}
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
  val COMM_IN = (p(FrontBusProtocolKey), p(BuildModeKey)) match {
    case (FrontBusProtocol.AXI4, _) | (FrontBusProtocol.AXIL, _) | (_, BuildMode.Simulation) =>
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
    case (FrontBusProtocol.AHB, _) =>
      val ahb_master = AHBSlaveSourceNode(
        portParams = Seq(AHBMasterPortParameters(
          masters = Seq(AHBMasterParameters(
            "S00_AHB"
          ))
        )))
      cmd_resp_axilhub.node.asInstanceOf[AHBSlaveIdentityNode] := ahb_master
      ahb_master
  }
  val dummyTL = p.alterPartial({ case TileVisibilityNodeKey => cmd_resp_axilhub.widget.node })

  // Generate accelerator SoC
  val accelerator_system = LazyModule(new ComposerAccSystem()(dummyTL))

  // Rocketchip AXI Nodes

  val has_memory_endpoints = accelerator_system.r_mem.nonEmpty || accelerator_system.w_mem.nonEmpty
  val AXI_MEM = if (has_memory_endpoints) Some(Seq.tabulate(nMemChannels) { channel_idx =>
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(getAddressSet(channel_idx)),
        resources = device.reg,
        regionType = RegionType.UNCACHED,
        supportsRead = TransferSizes(
          externalMemParams.master.beatBytes,
          externalMemParams.master.beatBytes * p(PrefetchSourceMultiplicity)),
        supportsWrite = TransferSizes(
          externalMemParams.master.beatBytes,
          externalMemParams.master.beatBytes * p(PrefetchSourceMultiplicity)),
        interleavedId = Some(1)
      )),
      beatBytes = externalMemParams.master.beatBytes
    )))
  }) else None

  if (AXI_MEM.isDefined) {
//    AXI_MEM.get.foreach(println(_))
  }

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

  val DMASourceBits = if (p(HasDMA).isDefined) CLog2Up(p(HasDMA).get) else 0
  val availableComposerSources = 1 << (p(ExtMem).get.master.idBits - DMASourceBits)

  // Connect accelerators to memory
  val composer_mems = if (accelerator_system.w_mem.isEmpty) {
    accelerator_system.r_mem.map{ src => val tl2axi = TLToAXI4() := src ; tl2axi }
  } else if (accelerator_system.r_mem.isEmpty) {
    accelerator_system.w_mem.map{ src => val tl2axi = TLToAXI4() := src ; tl2axi }
  } else {
    accelerator_system.r_mem.zip(accelerator_system.w_mem).zipWithIndex map { case ((r, w), idx) =>
      val Seq(rss, wss) = Seq(r, w).zip(Seq("readIDShrinker", "writeIDShrinker")).map { case (m, nm) => TLSourceShrinkerDynamicBlocking(availableComposerSources, Some(nm)) := m }
      val tl2axi = LazyModule(new TLToAXI4SRW(
        addressSet = ComposerTop.getAddressSet(idx),
        idMax = availableComposerSources))
      tl2axi.tlReader := rss
      tl2axi.tlWriter := wss
      // readers and writers are separated into separate systems and re-unified for the AXI bus. In reality though,
      // the read and write busses are logically unrelated so this unification is only symbolic
      tl2axi.axi_client
    }
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
  } else composer_mems

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
  val acc = outer.accelerator_system
  val axil_hub = outer.cmd_resp_axilhub
  val ocl_port = outer.COMM_IN
  acc.module.io.cmd <> axil_hub.module.io.rocc_in
  axil_hub.module.io.rocc_out <> acc.module.io.resp

  val S00_AXI = (p(FrontBusProtocolKey), p(BuildModeKey)) match {
    case (FrontBusProtocol.AXI4, _) | (FrontBusProtocol.AXIL, _) | (_, BuildMode.Simulation) =>
      val port_cast = ocl_port.asInstanceOf[AXI4MasterNode]
      val ap = port_cast.out(0)._1.params
      val S00_AXI = IO(Flipped(new AXI4Compat(MasterPortParams(
        base = 0,
        size = 1L << p(FrontBusAddressBits),
        beatBytes = ap.dataBits / 8,
        idBits = ap.idBits))))
      AXI4Compat.connectCompatSlave(S00_AXI, port_cast.out(0)._1)
      S00_AXI
    case (FrontBusProtocol.AHB, _) =>
      val port_cast = ocl_port.asInstanceOf[AHBSlaveSourceNode]
      val S00_AHB = IO(Flipped(AHBSlaveBundle(port_cast.out(0)._1.params)))
      S00_AHB.suggestName("S00_AHB")
      S00_AHB <> port_cast.out(0)._1
      S00_AHB
  }

  if (outer.AXI_MEM.isDefined) {
    val dram_ports = outer.AXI_MEM.get
    val M00_AXI = dram_ports.zipWithIndex.map { case (a, idx) =>
      val io = IO(AXI4Compat(a.in(0)._1.params))
      io.suggestName(s"M0${idx}_AXI")
      io
    }
    val ins = dram_ports.map(_.in(0))
    //  val axi4_mem = IO(HeterogeneousBag.fromNode(dram_ports.in))
    (M00_AXI zip ins) foreach { case (i, (o, _)) =>
      AXI4Compat.connectCompatMaster(i, o)
    }

    // make incoming dma port and connect it
    if (p(HasDMA).isDefined) {
      val dma = IO(Flipped(AXI4Compat(outer.AXI_MEM.get(0).in(0)._1.params)))
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

  // Generate C++ headers once all of the cores have been generated so that they have
  //   the opportunity to dictate which symbols they want exported
  CppGeneration.genCPPHeader(outer.cmd_resp_axilhub.widget.module.crRegistry, acc.acc)
  ConstraintGeneration.writeConstraints()
  if (p(BuildModeKey).isInstanceOf[BuildMode.Tuning]) {
    Tunable.exportNames()
  }
}
