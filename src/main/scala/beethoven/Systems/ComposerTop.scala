package beethoven.Systems

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import beethoven.Floorplanning.{ConstraintGeneration, LazyModuleWithSLRs}
import beethoven.Generation.BuildMode.Simulation
import beethoven._
import beethoven.Generation._
import beethoven.RoccHelpers.FrontBusHub
import beethoven.Systems.BeethovenTop._
import beethoven.common.CLog2Up
import beethoven.Generation.Tune.Tunable
import beethoven.Platforms._
import beethoven.Protocol._
import beethoven.TLManagement._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.{TLIdentityNode, TLMasterParameters, TLMasterPortParameters, TLNode, TLSlaveParameters, TLSlavePortParameters, TLXbar}

import scala.annotation.tailrec
import scala.language.implicitConversions


object BeethovenTop {
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

class BeethovenTop(implicit p: Parameters) extends LazyModuleWithSLRs() {
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
  val accelerator_system = LazyModule(new BeethovenAccSystem(frontDMA.isDefined)(dummyTL))

  if (frontDMA.isDefined) {
    accelerator_system.tldma.get._1 := frontDMA.get
    val fdma_dN = DotGen.addPortNode("FrontDMA")
    DotGen.addEdge(fdma_dN, accelerator_system.tldma.get._2)
  }

  val has_memory_endpoints = accelerator_system.r_mem.nonEmpty || accelerator_system.w_mem.nonEmpty || platform.frontBusCanDriveMemory
  val AXI_MEM = if (has_memory_endpoints) Some(Seq.tabulate(nMemChannels) { channel_idx =>
    (AXI4SlaveNode(Seq(AXI4SlavePortParameters(
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
    ))), DotGen.addPortNode("AXIMem", platform match {
      case pmd: Platform with MultiDiePlatform => pmd.platformDies.indexWhere(_.memoryBus)
      case _ => -1
    }))
  }) else None

  val (dma_port, dmaSourceBits) = platform match {
    case pWithDMA: Platform with PlatformHasSeparateDMA =>
      if (p(BuildModeKey) == BuildMode.Simulation) {
        (None, 0)
      } else {
        val dma_node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
          masters = Seq(AXI4MasterParameters(
            name = "S01_AXI",
            maxFlight = Some(2),
            aligned = true,
            id = IdRange(0, 1 << pWithDMA.DMAIDBits),
          ))
        )))
        (Some(dma_node, DotGen.addPortNode("DMASep",
          platform match {
            case pmd: Platform with MultiDiePlatform => pmd.platformDies.indexWhere(_.frontBus)
            case _ => -1
          })), pWithDMA.DMAIDBits)
      }
    case _ => (None, 0)
  }

  val availableBeethovenSources = 1 << (platform.extMem.master.idBits - dmaSourceBits)

  def tieFBandShrink(tl: (TLNode, String), name: Option[String]): (TLNode, String) = {
    val shrinker = TLSourceShrinkerDynamicBlocking(availableBeethovenSources, name)
    val shrinkdN = DotGen.addNode(s"${tl._2}_shrink")
    shrinker := tl._1
    DotGen.addEdge(tl._2, shrinkdN)
    (shrinker, shrinkdN)
  }

  // Connect accelerators to memory
  val beethoven_mems = if (accelerator_system.w_mem.isEmpty) {
    accelerator_system.r_mem.map { src =>
      val tl2axi = LazyModule(new TLToAXI4R(
        addressSet = BeethovenTop.getAddressSet(0),
        idMax = availableBeethovenSources))
      val dN = DotGen.addNode("TLToAXI4R")
      val (shrunk, shrunkdN) = tieFBandShrink(src, Some("read_shrink"))
      tl2axi.tlReader := shrunk
      DotGen.addEdge(shrunkdN, dN)
      DotGen.addEdge(src._2, shrunkdN)
      (tl2axi.axi_client, dN)
    }
  } else if (accelerator_system.r_mem.isEmpty) {
    accelerator_system.w_mem.map { src =>
      val tl2axi = LazyModule(new TLToAXI4W(
        addressSet = BeethovenTop.getAddressSet(0),
        idMax = availableBeethovenSources))
      val dN = DotGen.addNode("TLToAXI4W")
      val (shrunk, shrunkdN) = tieFBandShrink(src, Some("write_shrink"))
      tl2axi.tlWriter := shrunk
      DotGen.addEdge(shrunkdN, dN)
      DotGen.addEdge(src._2, shrunkdN)
      (tl2axi.axi_client, dN)
    }
  } else {
    accelerator_system.r_mem.zip(accelerator_system.w_mem).zipWithIndex map { case ((r, w), idx) =>
      val Seq(rss, wss) = Seq(r, w).zip(Seq("readIDShrinker", "writeIDShrinker")).map { case (m, nm) =>
        val q = TLSourceShrinkerDynamicBlocking(availableBeethovenSources, Some(nm))
        q := m._1
        val dN = DotGen.addNode(nm)
        DotGen.addEdge(m._2, dN)
        (q, dN)
      }
      val tl2axi = LazyModule(new TLToAXI4SRW(
        addressSet = BeethovenTop.getAddressSet(idx),
        idMax = availableBeethovenSources))
      val dN = DotGen.addNode("TLToAXI4SRW")

      val rsh = tieFBandShrink(rss, Some("read_shrink"))
      val wsh = tieFBandShrink(wss, Some("write_shrink"))
      tl2axi.tlReader := rsh._1
      tl2axi.tlWriter := wsh._1
      DotGen.addEdge(rsh._2, dN)
      DotGen.addEdge(wsh._2, dN)
      DotGen.addEdge(r._2, rsh._2)
      DotGen.addEdge(w._2, wsh._2)
      // readers and writers are separated into separate systems and re-unified for the AXI bus. In reality though,
      // the read and write busses are logically unrelated so this unification is only symbolic
      (tl2axi.axi_client, dN)
    }
  }
  val mem_tops = if (platform.isInstanceOf[PlatformHasSeparateDMA] && p(BuildModeKey) != Simulation) {
    if (p(ConstraintHintsKey).contains(BeethovenConstraintHint.DistributeCoresAcrossSLRs)) {
      val full_mem_xbar = Seq.tabulate(nMemChannels)(idx =>
        (LazyModuleWithFloorplan(new AXI4Xbar(maxFlightPerId = p(MaxInFlightMemTxsPerSource)),
          slr_id = DieName.getMemoryBusSLR,
          name = s"dma_xbar_front_channel$idx"),
          DotGen.addNode("DMA_Xbar")))
      val dma_front = LazyModuleWithFloorplan(new AXI4Buffer(),
        slr_id = DieName.getFrontBusSLR,
        name = s"dma_buff_front")
      val dmafdN = DotGen.addNode("DMA_Buffer")
      val dma_side = LazyModuleWithFloorplan(new AXI4Buffer(),
        slr_id = DieName.getMemoryBusSLR,
        name = "dma_buff_mbus")
      val dmsdN = DotGen.addNode("DMA_Buffer")
      val dma_fanout = LazyModuleWithFloorplan(new AXI4Xbar(),
        slr_id = DieName.getFrontBusSLR,
        name = s"dma_xbar_front")
      val dmafandN = DotGen.addNode("DMA_Xbar")
      require(nMemChannels == 1, "slr needs to be improved to support multiple slrs")
      dma_front.node := dma_port.get._1
      DotGen.addEdge(dma_port.get._2, dmafdN)
      dma_side.node := dma_front.node
      DotGen.addEdge(dmafdN, dmsdN)
      dma_fanout.node := dma_side.node
      DotGen.addEdge(dmsdN, dmafandN)
      full_mem_xbar zip beethoven_mems foreach { case ((xb, xbdN), (cm, cmdN)) =>
        xb.node := cm
        DotGen.addEdge(cmdN, xbdN)
        xb.node := dma_fanout.node
        DotGen.addEdge(dmafandN, xbdN)
      }
      full_mem_xbar.map(a => (a._1.node, a._2))
    } else {
      val dma_mem_xbar = Seq.fill(nMemChannels)((
        AXI4Xbar(maxFlightPerId = p(MaxInFlightMemTxsPerSource)),
        DotGen.addNode("DMA_Xbar")))
      val dma = AXI4Xbar()
      def extendDMA(l: Int, node: AXI4Node = dma_port.get._1): AXI4Node = {
        if (l == 0) node
        else {
          val buff = AXI4Buffer()
          buff := node
          extendDMA(l - 1, buff)
        }
      }
      val dmaBdN = DotGen.addNode(f"DMA_buff.latency4")
      val dmaxbdN = DotGen.addNode("DMA_Xbar")
      dma := extendDMA(4)
      DotGen.addEdge(dma_port.get._2, dmaBdN)
      DotGen.addEdge(dmaBdN, dmaxbdN)
      dma_mem_xbar zip beethoven_mems foreach { case ((xb, xbdn), (cm, cdn)) =>
        xb := cm
        xb := dma
        DotGen.addEdge(cdn, xbdn)
        DotGen.addEdge(dmaxbdN, xbdn)
      }
      dma_mem_xbar
    }
  } else beethoven_mems

  AXI_MEM match {
    case Some(mems) =>
      mem_tops zip mems foreach { case ((mt, mtn), (endpoint, epN)) =>
        endpoint := AXI4Buffer() := mt
        val buffdN = DotGen.addNode("AXI4Buffer")
        DotGen.addEdge(mtn, buffdN)
        DotGen.addEdge(buffdN, epN)
      }
    case None => ;
  }

  lazy val module = new TopImpl(this)
}

class TopImpl(outer: BeethovenTop)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val acc = outer.accelerator_system
  val axil_hub = outer.cmd_resp_axilhub
  val ocl_port = outer.comm_node
  acc.module.io.cmd <> axil_hub.module.io.rocc_in
  axil_hub.module.io.rocc_out <> acc.module.io.resp
  val frontdN = DotGen.addPortNode("FrontBus", platform match {
    case pmd: Platform with MultiDiePlatform => pmd.platformDies.indexWhere(_.frontBus)
    case _ => -1
  })
  DotGen.addEdge(outer.accelerator_system.acc.sysRespN, frontdN)
  DotGen.addEdge(frontdN, outer.accelerator_system.acc.sysCmddN)

  platform.frontBusProtocol.deriveTopIOs(ocl_port, clock, reset)

  if (outer.AXI_MEM.isDefined) {
    val dram_ports = outer.AXI_MEM.get
    val M00_AXI = dram_ports.zipWithIndex.map { case (a, idx) =>
      val io = IO(AXI4Compat(a._1.in(0)._1.params))
      io.suggestName(s"M0${idx}_AXI")
      io
    }
    val ins = dram_ports.map(_._1.in(0))
    //  val axi4_mem = IO(HeterogeneousBag.fromNode(dram_ports.in))
    (M00_AXI zip ins) foreach { case (i, (o, _)) =>
      AXI4Compat.connectCompatMaster(i, o)
    }

    // make incoming dma port and connect it
    platform match {
      case pWithDMA: PlatformHasSeparateDMA =>
        if (p(BuildModeKey) != Simulation) {
          val params = outer.AXI_MEM.get(0)._1.in(0)._1.params
          val dma = IO(Flipped(AXI4Compat(params.copy(idBits = pWithDMA.DMAIDBits))))
          AXI4Compat.connectCompatSlave(dma, outer.dma_port.get._1.out(0)._1)
        }
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
  if (p(BuildModeKey) == BuildMode.Synthesis)
    ConstraintGeneration.writeConstraints()
  if (p(BuildModeKey).isInstanceOf[BuildMode.Tuning]) {
    Tunable.exportNames()
  }
  DotGen.writeToFile("beethoven.dot")
}
