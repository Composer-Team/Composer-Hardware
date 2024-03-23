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
import composer.Platforms._
import composer.Protocol._
import composer.TLManagement._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.{TLIdentityNode, TLMasterParameters, TLMasterPortParameters, TLNode, TLSlaveParameters, TLSlavePortParameters, TLXbar}

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
    val nMemChannels = platform.extMem.nMemoryChannels
    // this one is the defuault for rocket chip. Each new cache line (size def by CacheBlockBytes) is on another
    // DIMM. This makes fetching 4 contiguous cache blocks completely parallelized. Should be way faster...
    //    val continuity = p(CacheBlockBytes)
    //  this one splits the DIMMS into contiguous address spaces. Not sure what that's good for...
    //  but it seems anyways that it won't work UNLESS it's like this!
    val continuity = platform.extMem.master.size
    val baseTotal = (nMemChannels - 1) * continuity
    val amask = getAddressMask(log2Up(platform.extMem.master.size), baseTotal.toLong)

    if (platform.extMem.master.base > 0) {
      assert(ddrChannel == 0)
      require(isPow2(platform.extMem.master.base))
      val em = platform.extMem.master
      AddressSet(em.base, em.base - 1)
    } else {

      AddressSet(continuity * ddrChannel, amask)
    }
  }
}

class ComposerTop(implicit p: Parameters) extends LazyModuleWithSLRs() {
  override val baseName: String = "TopModule"
  private val externalMemParams: MemoryPortParams = platform.extMem
  private val nMemChannels = externalMemParams.nMemoryChannels
  private val device = new MemoryDevice

  val cmd_resp_axilhub = LazyModule(new FrontBusHub())

  // AXI-L Port - commands come through here
  val (comm_node, frontSource, frontDMA) = platform.frontBusProtocol.deriveTLSources(p)
  cmd_resp_axilhub.tl_head := frontSource

  val dummyTL = p.alterPartial({ case TileVisibilityNodeKey => cmd_resp_axilhub.widget.node })

  // Generate accelerator SoC
  val accelerator_system = LazyModule(new ComposerAccSystem(frontDMA.isDefined)(dummyTL))
  if (frontDMA.isDefined) accelerator_system.tldma.get := frontDMA.get

  val has_memory_endpoints = accelerator_system.r_mem.nonEmpty || accelerator_system.w_mem.nonEmpty || platform.frontBusCanDriveMemory
  val AXI_MEM = if (has_memory_endpoints) Some(Seq.tabulate(nMemChannels) { channel_idx =>
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(getAddressSet(channel_idx)),
        resources = device.reg,
        regionType = RegionType.UNCACHED,
        supportsRead = TransferSizes(
          externalMemParams.master.beatBytes,
          externalMemParams.master.beatBytes * platform.prefetchSourceMultiplicity),
        supportsWrite = TransferSizes(
          externalMemParams.master.beatBytes,
          externalMemParams.master.beatBytes * platform.prefetchSourceMultiplicity),
        interleavedId = Some(1)
      )),
      beatBytes = externalMemParams.master.beatBytes
    )))
  }) else None

  val (dma_port, dmaSourceBits) = platform match {
    case pWithDMA: Platform with PlatformHasSeparateDMA =>
      val dma_node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
        masters = Seq(AXI4MasterParameters(
          name = "S01_AXI",
          maxFlight = Some(2),
          aligned = true,
          id = IdRange(0, 1 << pWithDMA.DMAIDBits),
        ))
      )))
      (Some(dma_node), pWithDMA.DMAIDBits)
    case _ => (None, 0)
  }

  val availableComposerSources = 1 << (platform.extMem.master.idBits - dmaSourceBits)

  def tieFBandShrink(tl: TLNode, name: Option[String]): TLNode = {
    val shrinker = TLSourceShrinkerDynamicBlocking(availableComposerSources, name)
    shrinker := tl
    shrinker
  }

  // Connect accelerators to memory
  val composer_mems = if (accelerator_system.w_mem.isEmpty) {
    accelerator_system.r_mem.map { src =>
      val tl2axi = LazyModule(new TLToAXI4R(
        addressSet = ComposerTop.getAddressSet(0),
        idMax = availableComposerSources))
      tl2axi.tlReader := tieFBandShrink(src, Some("read_shrink"))
      tl2axi.axi_client
    }
  } else if (accelerator_system.r_mem.isEmpty) {
    accelerator_system.w_mem.map { src =>
      val tl2axi = LazyModule(new TLToAXI4W(
        addressSet = ComposerTop.getAddressSet(0),
        idMax = availableComposerSources))
      tl2axi.tlWriter := tieFBandShrink(src, Some("write_shrink"))
      tl2axi.axi_client
    }
  } else {
    accelerator_system.r_mem.zip(accelerator_system.w_mem).zipWithIndex map { case ((r, w), idx) =>
      val Seq(rss, wss) = Seq(r, w).zip(Seq("readIDShrinker", "writeIDShrinker")).map {
        case (m, nm) => TLSourceShrinkerDynamicBlocking(availableComposerSources, Some(nm)) := m
      }
      val tl2axi = LazyModule(new TLToAXI4SRW(
        addressSet = ComposerTop.getAddressSet(idx),
        idMax = availableComposerSources))
      tl2axi.tlReader := tieFBandShrink(rss, Some("read_shrink"))
      tl2axi.tlWriter := tieFBandShrink(wss, Some("write_shrink"))
      // readers and writers are separated into separate systems and re-unified for the AXI bus. In reality though,
      // the read and write busses are logically unrelated so this unification is only symbolic
      tl2axi.axi_client
    }
  }
  val mem_tops = if (platform.isInstanceOf[PlatformHasSeparateDMA]) {
    if (p(ConstraintHintsKey).contains(ComposerConstraintHint.DistributeCoresAcrossSLRs)) {
      val full_mem_xbar = Seq.tabulate(nMemChannels)(idx =>
        LazyModuleWithFloorplan(new AXI4Xbar(maxFlightPerId = p(MaxInFlightMemTxsPerSource)),
          slr_id = DieName.getMemoryBusSLR,
          requestedName = Some(s"dma_xbar_front_channel$idx")))
      val dma_front = LazyModuleWithFloorplan(new AXI4Buffer(),
        slr_id = DieName.getFrontBusSLR,
        requestedName = Some(s"dma_buff_front"))
      val dma_side = LazyModuleWithFloorplan(new AXI4Buffer(),
        slr_id = DieName.getMemoryBusSLR,
        requestedName = Some("dma_buff_mbus"))
      val dma_fanout = LazyModuleWithFloorplan(new AXI4Xbar(),
        slr_id = DieName.getFrontBusSLR,
        requestedName = Some(s"dma_xbar_front"))
      require(nMemChannels == 1, "slr needs to be improved to support multiple slrs")
      dma_front.node := dma_port.get
      dma_side.node := dma_front.node
      dma_fanout.node := dma_side.node
      full_mem_xbar zip composer_mems foreach { case (xb, cm) =>
        xb.node := cm
        xb.node := dma_fanout.node
      }
      full_mem_xbar.map(_.node)
    } else {
      val dma_mem_xbar = Seq.fill(nMemChannels)(AXI4Xbar(maxFlightPerId = p(MaxInFlightMemTxsPerSource)))
      val dma = AXI4Xbar()
      dma := AXI4Buffer() := dma_port.get
      dma_mem_xbar zip composer_mems foreach { case (xb, cm) =>
        xb := cm
        xb := dma
      }
      dma_mem_xbar
    }
  } else composer_mems

  AXI_MEM match {
    case Some(mems) =>
      mem_tops zip mems foreach { case (mt, endpoint) =>
        endpoint :=
          AXI4Buffer() := mt
      }
    case None => ;
  }

  lazy val module = new TopImpl(this)
}

class TopImpl(outer: ComposerTop)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val acc = outer.accelerator_system
  val axil_hub = outer.cmd_resp_axilhub
  val ocl_port = outer.comm_node
  acc.module.io.cmd <> axil_hub.module.io.rocc_in
  axil_hub.module.io.rocc_out <> acc.module.io.resp

  platform.frontBusProtocol.deriveTopIOs(ocl_port, clock, reset)

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
    platform match {
      case pWithDMA: PlatformHasSeparateDMA =>
        val params = outer.AXI_MEM.get(0).in(0)._1.params
        val dma = IO(Flipped(AXI4Compat(params.copy(idBits = pWithDMA.DMAIDBits))))
        AXI4Compat.connectCompatSlave(dma, outer.dma_port.get.out(0)._1)
      case _ => ;
    }

    require(M00_AXI(0).rid.getWidth <= platform.extMem.master.idBits,
      s"Too many ID bits for this platform. Try reducing the\n" +
        s"prefetch length of scratchpads/readers/writers.\n" +
        s"Current width: ${M00_AXI(0).rid.getWidth}\n" +
        s"Required width: ${platform.extMem.master.idBits}")
  }

  // Generate C++ headers once all of the cores have been generated so that they have
  //   the opportunity to dictate which symbols they want exported
  CPP.Generation.genCPPHeader(outer.cmd_resp_axilhub.widget.module.crRegistry, outer)(p.alterPartial {
    case ExtMem => if (outer.AXI_MEM.isDefined) platform.extMem else platform.extMem.copy(nMemoryChannels = 0)
  })
  ConstraintGeneration.writeConstraints()
  if (p(BuildModeKey).isInstanceOf[BuildMode.Tuning]) {
    Tunable.exportNames()
  }
}
