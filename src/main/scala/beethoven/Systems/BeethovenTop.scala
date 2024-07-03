package beethoven.Systems

import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import beethoven.Floorplanning.ConstraintGeneration
import beethoven.Generation.BuildMode
import beethoven._
import beethoven.Systems.BeethovenTop._
import beethoven.Generation.Tune.Tunable
import beethoven.Parameters._
import beethoven.Platforms._
import beethoven.Protocol.AXI.AXI4Compat
import beethoven.Protocol.RoCC.{RoccNode, TLToRocc}
import beethoven.Protocol.tilelink.{TLRWFilter, TLSourceShrinkerDynamicBlocking, TLToAXI4SRW}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._

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

class BeethovenTop(implicit p: Parameters) extends LazyModule {
  private val externalMemParams: MemoryPortParams = platform.extMem
  private val nMemChannels = externalMemParams.nMemoryChannels
  private val device = new MemoryDevice

  // AXI-L Port - commands come through here
  val (comm_node, rocc_front, frontDMA_joined) = platform.frontBusProtocol.deriveTLSources(p)

  val (frontDMA_r, frontDMA_w) = if (frontDMA_joined.isDefined) {
    val maxTransfer = TransferSizes(platform.extMem.master.beatBytes, platform.extMem.master.beatBytes * (1 << AXI4Compat.lenWidth))
    val splitter = LazyModule(new TLRWFilter(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = (0 until platform.memoryNChannels).map(BeethovenTop.getAddressSet(_)),
        regionType = RegionType.IDEMPOTENT,
        supportsGet = maxTransfer,
        supportsPutFull = maxTransfer,
        supportsPutPartial = maxTransfer)),
      beatBytes = platform.extMem.master.beatBytes, endSinkId = 0),
      mpp = TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          "DMA",
          sourceId = IdRange(1, 4),
          supportsProbe = maxTransfer,
          supportsGet = maxTransfer,
          supportsPutFull = maxTransfer,
          supportsPutPartial = maxTransfer
        )))))
    val indexer = LazyModule(new TLSourceShrinkerDynamicBlocking(4))
    splitter.in_node := indexer.node := frontDMA_joined.get
    (Some(splitter.read_out), Some(splitter.write_out))
  } else (None, None)


  // Generate accelerator SoC
  val devices = platform.physicalDevices.filter { dev =>
    // only consider devices that will have a core on it
    p(AcceleratorSystems).map(as => slr2ncores(dev.identifier, as.nCores)._1).sum > 0
  }.map { dev =>
    val lm = LazyModuleWithFloorplan(new Subdevice(dev.identifier)(p.alterPartial {
      case TileVisibilityNodeKey => rocc_front
    }), dev.identifier, f"beethovenDevice${dev.identifier}")
    lm
  }

  val has_r_mem = devices.map(_.r_nodes.nonEmpty).reduce(_ || _) || frontDMA_joined.isDefined
  val has_w_mem = devices.map(_.w_nodes.nonEmpty).reduce(_ || _) || frontDMA_joined.isDefined
  val AXI_MEM = if (has_r_mem || has_w_mem) Some(Seq.tabulate(nMemChannels) { channel_idx =>
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

  // deal with memory interconnect
  {
    var cnt = 1
    val r_map = {
      val uncondensed = devices.flatMap { d =>
        val on_chip = d.r_nodes.map(b => {
          val r = (d.deviceId, (b, List(cnt)))
          cnt = cnt + 1
          r
        })
        val off_chip = if (frontDMA_r.isDefined && platform.physicalInterfaces.find(_.isInstanceOf[PhysicalHostInterface]).get.locationDeviceID == d.deviceId)
          Seq((d.deviceId, (frontDMA_r.get, List(0))))
        else Seq()
        on_chip ++ off_chip
      }
      Map.from(devices.map(d => (d.deviceId, uncondensed.filter(_._1 == d.deviceId).map(_._2))))
    }

    val w_map = {
      val uncondensed = devices.flatMap { d =>
        val on_chip = d.w_nodes.map(b => {
          val r = (d.deviceId, (b, List(cnt)))
          cnt = cnt + 1
          r
        })
        val off_chip = if (frontDMA_w.isDefined && platform.physicalInterfaces.find(_.isInstanceOf[PhysicalHostInterface]).get.locationDeviceID == d.deviceId)
          Seq((d.deviceId, (frontDMA_w.get, List(0))))
        else Seq()
        on_chip ++ off_chip
      }
      Map.from(devices.map(d => (d.deviceId, uncondensed.filter(_._1 == d.deviceId).map(_._2))))
    }

    val mem_sinks = platform.physicalInterfaces.filter(_.isInstanceOf[PhysicalMemoryInterface]).map(_.locationDeviceID)

    val Seq(r_commits, w_commits) = Seq(r_map, w_map).map { carry_init =>
      val fpass = topological_xbarReduce_over_SUG(
        devices = platform.physicalDevices.map(_.identifier),
        connectivity = platform.physicalConnectivity,
        carries = carry_init,
        commits = Map.from(mem_sinks.map(a => (a, List.empty))),
        make_buffer = make_tl_buffer,
        make_xbar = make_tl_xbar,
        assign = tl_assign)(p, mem_sinks)
      topological_xbarReduce_over_SUG(
        platform.physicalDevices.map(_.identifier),
        connectivity = reverse_connectivity(platform.physicalConnectivity),
        carries = carry_init,
        commits = fpass,
        make_buffer = make_tl_buffer,
        make_xbar = make_tl_xbar,
        assign = tl_assign)(p, mem_sinks)
    }
    platform.physicalInterfaces.foreach {
      case pmi: PhysicalMemoryInterface =>
        val mem = (AXI_MEM.get)(pmi.channelIdx)
        val sTLToAXI = LazyModuleWithFloorplan(new TLToAXI4SRW(), pmi.locationDeviceID).node
        Seq(r_commits, w_commits) foreach (commit_set =>
          xbar_tree_reduce_sources(commit_set(pmi.locationDeviceID).map(_._1), platform.xbarMaxDegree, 1,
            make_tl_xbar,
            make_tl_buffer,
            (s: Seq[TLNode], t: TLNode) => tl_assign(s, t)(p))
            .foreach(sTLToAXI := TLSourceShrinkerDynamicBlocking(1 << platform.memoryControllerIDBits) := _))
        mem := AXI4Buffer() := sTLToAXI
      case _ => ;
    }
  }

  // commands
  {
    // the sinks consist of the host interface and any system that can emit commands
    // the sources consist of all systems
    var srocc_id = 0
    val emitters_per_device = devices.map { sd =>
      val device_has_host = platform.physicalInterfaces.filter(_.locationDeviceID == sd.deviceId).exists(_.isInstanceOf[PhysicalHostInterface])
      (sd.deviceId, (if (device_has_host) List(rocc_front) else List()) ++ sd.source_rocc)
    }.map { a =>
      (a._1, a._2.map { b =>
        val q = srocc_id
        srocc_id = srocc_id + 1
        (b, List(q))
      })
    }

    val devices_with_sinks = devices.map { sd => sd.deviceId }

    val carry_init = Map.from(emitters_per_device)

    val fpass = topological_xbarReduce_over_SUG(
      platform.physicalDevices.map(_.identifier),
      platform.physicalConnectivity,
      carry_init,
      Map.from(devices_with_sinks.map((_, List.empty))),
      make_rocc_buffer,
      make_rocc_xbar,
      rocc_assign)(p, devices_with_sinks)
    val net = topological_xbarReduce_over_SUG(
      platform.physicalDevices.map(_.identifier),
      reverse_connectivity(platform.physicalConnectivity),
      carry_init,
      fpass,
      make_rocc_buffer,
      make_rocc_xbar,
      rocc_assign)(p, devices_with_sinks)
    // push the commands to the devices
    devices.foreach { sd =>
      val sink = sd.host_rocc
      val sources = net(sd.deviceId).map(_._1)
      val source_unit = xbar_tree_reduce_sources(sources, platform.xbarMaxDegree, 1, make_rocc_xbar, make_rocc_buffer,
        (a: Seq[RoccNode], b: RoccNode) => rocc_assign(a, b)(p))(p)(0)
      sink := source_unit
    }
  }

  // on chip memory transfers
  {
    val devices_with_sinks = devices.filter(_.incoming_mem.nonEmpty).map { sd => sd.deviceId }
    var cnt = 0
    val carry_init = Map.from(devices.map(d => (d.deviceId, d.outgoing_mem)).map { case (a, b) => (a, b.map(c => {
      val r = (c, List(cnt))
      cnt = cnt + 1
      r
    }).toList)
    })
    val fpass = topological_xbarReduce_over_SUG(
      platform.physicalDevices.map(_.identifier),
      platform.physicalConnectivity,
      carry_init,
      Map.from(devices_with_sinks.map((_, List.empty))),
      make_tl_buffer,
      make_tl_xbar,
      tl_assign)(p, devices_with_sinks)
    val net = topological_xbarReduce_over_SUG(
      platform.physicalDevices.map(_.identifier),
      reverse_connectivity(platform.physicalConnectivity),
      carry_init,
      fpass,
      make_tl_buffer,
      make_tl_xbar,
      tl_assign)(p, devices_with_sinks)
    devices_with_sinks.foreach { sd =>
      val sinks = xbar_tree_reduce_sinks(devices.find(_.deviceId == sd).get.incoming_mem, platform.xbarMaxDegree, platform.memEndpointsPerDevice, make_tl_xbar, make_tl_buffer, tl_assign)(p)
      val sources = net(sd).map(_._1)
      sources.foreach { source =>
        sinks.foreach { sink =>
          sink := source
        }
      }
    }
  }


  lazy val module = new TopImpl(this)
}

class TopImpl(outer: BeethovenTop)(implicit p: Parameters) extends LazyModuleImp(outer) {
  platform.frontBusProtocol.deriveTopIOs(outer.comm_node, clock, reset)

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

    require(M00_AXI(0).rid.getWidth <= platform.extMem.master.idBits,
      s"Too many ID bits for this platform. Try reducing the\n" +
        s"prefetch length of scratchpads/readers/writers.\n" +
        s"Current width: ${M00_AXI(0).rid.getWidth}\n" +
        s"Required width: ${platform.extMem.master.idBits}")
  }

  // Generate C++ headers once all of the cores have been generated so that they have
  //   the opportunity to dictate which symbols they want exported
  Generation.CPP.Generation.genCPPHeader(outer)
  if (p(BuildModeKey) == BuildMode.Synthesis)
    ConstraintGeneration.writeConstraints()
  if (p(BuildModeKey).isInstanceOf[BuildMode.Tuning]) {
    Tunable.exportNames()
  }
}
