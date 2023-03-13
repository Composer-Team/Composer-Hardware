package composer

import chisel3._
import chisel3.util._
import composer.ComposerTop._
import composer.CppGeneration.genCPPHeader
import composer.RoccHelpers.{AXI4Compat, AXILHub, RDReserves}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
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
  // AXI4 DRAM Ports
  val AXI_MEM = AXI4SlaveNode(Seq.tabulate(nMemChannels) { channel =>
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
  })

  val acc = LazyModule(new ComposerAccSystem())

  // note dummyTL doesn't do it anything. it is to avoid rocket compile errors
  val dummyTL = p.alterPartial({
    case TileVisibilityNodeKey => acc.mem.head
  })

  val dma_port =  if (p(HasDMA).isDefined) {
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

  val composer_mems = Seq.fill(nMemChannels)(AXI4IdentityNode())
  acc.mem zip composer_mems foreach { case (m, x) =>
    (  x
      := AXI4Buffer()
//      := AXI4Deinterleaver()
      := AXI4Buffer()
      := TLToAXI4()
      := m)
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
    AXI_MEM := AXI4Buffer() := AXI4UserYanker(capMaxFlight = Some(p(MaxInFlightMemTxsPerSource))) := AXI4Buffer() := AXI4IdIndexer(extMemIDBits) := AXI4Buffer() := mt
  }

  val cmd_resp_axilhub = LazyModule(new AXILHub()(dummyTL))

  // connect axil hub to external axil port
  cmd_resp_axilhub.node := AXI4Buffer() := S00_AXI
  // connect axil hub to accelerator

  lazy val module = new TopImpl(this)
}

class TopImpl(outer: ComposerTop) extends LazyModuleImp(outer) {
  val acc = outer.acc
  val axil_hub = outer.cmd_resp_axilhub
  val ocl_port = outer.S00_AXI
  val dram_ports = outer.AXI_MEM
  acc.module.io.cmd <> axil_hub.module.io.rocc_in
  axil_hub.module.io.rocc_out <> acc.module.io.resp

  val S00_AXI = IO(Flipped(new AXI4Compat(ocl_port.out(0)._1.params)))
  AXI4Compat.connectCompatSlave(S00_AXI, ocl_port.out(0)._1)

  val M00_AXI = dram_ports.in.zipWithIndex.map(a => {
    val io = IO(new AXI4Compat(a._1._1.params))
    io.suggestName(s"M0${a._2}_AXI")
    io
  })
  //  val axi4_mem = IO(HeterogeneousBag.fromNode(dram_ports.in))
  (M00_AXI zip dram_ports.in) foreach { case (i, (o, _)) => AXI4Compat.connectCompatMaster(i, o) }


  // make incoming dma port and connect it

  if (p(HasDMA).isDefined) {
    val dma = IO(Flipped(new AXI4Compat(outer.AXI_MEM.in(0)._1.params)))
    AXI4Compat.connectCompatSlave(dma, outer.dma_port.get.out(0)._1)
  }

  //add thing to here
  val arCnt = RegInit(0.U(64.W))
  val awCnt = RegInit(0.U(64.W))
  val rCnt = RegInit(0.U(64.W))
  val wCnt = RegInit(0.U(64.W))
  val bCnt = RegInit(0.U(64.W))
  val q = dram_ports.in(0)._1

  when(q.ar.fire) {
    arCnt := arCnt + 1.U
  }
  when(q.aw.fire) {
    awCnt := awCnt + 1.U
  }
  when(q.r.fire) {
    rCnt := rCnt + 1.U
  }
  when(q.w.fire) {
    wCnt := wCnt + 1.U
  }
  when(q.b.fire) {
    bCnt := bCnt + 1.U
  }

  val rWait = RegInit(0.U(64.W))
  val bWait = RegInit(0.U(64.W))
  when(q.r.ready && !q.r.valid) {
    rWait := rWait + 1.U
  }

  when(q.b.ready && !q.b.valid) {
    bWait := bWait + 1.U
  }

  switch(acc.module.io.resp.bits.rd) {
    is(RDReserves.arCnt.U) {
      axil_hub.module.io.rocc_out.bits.data := arCnt
    }
    is(RDReserves.awCnt.U) {
      axil_hub.module.io.rocc_out.bits.data := awCnt
    }
    is(RDReserves.rCnt.U) {
      axil_hub.module.io.rocc_out.bits.data := rCnt
    }
    is(RDReserves.wCnt.U) {
      axil_hub.module.io.rocc_out.bits.data := wCnt
    }
    is(RDReserves.bCnt.U) {
      axil_hub.module.io.rocc_out.bits.data := bCnt
    }
    is(RDReserves.rWait.U) {
      axil_hub.module.io.rocc_out.bits.data := rWait
    }
    is(RDReserves.bWait.U) {
      axil_hub.module.io.rocc_out.bits.data := bWait
    }
  }

  // try to make this the last thing we do
  genCPPHeader(outer.cmd_resp_axilhub.axil_widget.module.crRegistry, acc.acc)
}
