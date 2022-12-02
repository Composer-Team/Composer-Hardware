package composer

import chisel3._
import chisel3.util._
import composer.ComposerTop.getAddressSet
import composer.CppGenerationUtils.genCPPHeader
import freechips.rocketchip.amba.axi4
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import scala.annotation.tailrec
import scala.language.implicitConversions


object ComposerTop {
  def getAddressSet(ddrChannel: Int)(implicit p: Parameters): AddressSet = {
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
  val ocl_port = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "ocl",
      //      aligned = false, // could be true?
      maxFlight = Some(1)
    )),
    //    userBits = 0
  )))
  // AXI4 DRAM Ports
  val dram_ports = AXI4SlaveNode(Seq.tabulate(nMemChannels) { channel =>
    // TODO Brendan? DDR Controllers on F1 are oblivious to address mappings
    //    It seems like RocketChip wrote the following stuff up assuming different DRAM rows would be on separate DIMMs
    //    and that something else would be filtering out the bits for that but F1 controllers certainly don't seem to
    //    do that, so we should be able to do that ourselves. Consecutive addresses usually live on different DRAM
    //    DIMMs for performance reasons. But I support putting them in the same bank is fine too :( Bank conflicts
    //    are a serialization point
    val as = {
      val q = getAddressSet(channel)
      if (p(MMIOBaseAddress).isDefined) {
        val base = p(MMIOBaseAddress).get
        val mmio = AddressSet(base, 0x3F)
        q.subtract(mmio)
      } else Seq(q)
    }
    println(as)

    AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = as,
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

  val dma_port =  if (p(HasDMA)) {
    val dma_node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "dma",
        maxFlight = Some(2),
        aligned = true,
        id = IdRange(0, 1),
      ))
    )))
    Some(dma_node)
  } else {
    None
  }

  // TODO think about IDs overlapping....

  // We have to share shell DDR ports with DMA bus (which is AXI4). Use RocketChip utils to do that instead of the
  // whole shebang with instantiating strange encrypted Xilinx IPs'

  val composer_mems = Seq.fill(nMemChannels)(AXI4IdentityNode())
  acc.mem zip composer_mems foreach { case (m, x) =>
    (  x
      := AXI4Buffer()
      := AXI4UserYanker()
      := AXI4Buffer()
      //:= AXI4IdIndexer(idBits = 9)
      := AXI4Deinterleaver(64)
      := AXI4Buffer()
      := TLToAXI4()
      // TODO CHECK WITH LISA - This component shrinks TL transactions down to 32B at a time, allowing less resource
      //  usage in readers/writers?
      := TLWidthWidget(64)
      := m)
  }

  if (p(HasDMA)) {
    val dma_mem_xbar = Seq.fill(nMemChannels)(AXI4Xbar())
    val dma = AXI4Xbar()
    dma := AXI4Buffer() := dma_port.get
    dma_mem_xbar zip composer_mems foreach { case (xb, cm) =>
      xb := cm
      xb := dma
    }
    dma_mem_xbar foreach (dram_ports := _)
  } else {
    composer_mems foreach (dram_ports := _)
  }

  val cmd_resp_axilhub = LazyModule(new AXILHub()(dummyTL))

  // connect axil hub to external axil port
  cmd_resp_axilhub.node := ocl_port
  // connect axil hub to accelerator

  (acc.hostmem
    := TLFIFOFixer()
    := TLBuffer()
    := TLWidthWidget(4) // axil hub width = 4 bytes, adamacc width = 32 bytes
    := TLBuffer()
    := AXI4ToTL()
    //:= AXI4UserYanker()
    //:= AXI4Fragmenter()
    //:= AXI4IdIndexer(idBits = log2Ceil(8))
    := cmd_resp_axilhub.mem_out
    )

  lazy val module = new TopImpl(this)
}

class TopImpl(outer: ComposerTop) extends LazyModuleImp(outer) {
  val acc = outer.acc
  val axil_hub = outer.cmd_resp_axilhub
  val ocl_port = outer.ocl_port
  val dram_ports = outer.dram_ports
  acc.module.io.cmd <> axil_hub.module.io.rocc_in
  axil_hub.module.io.rocc_out <> acc.module.io.resp

  genCPPHeader(outer.cmd_resp_axilhub.axil_widget.module.crRegistry, acc.acc)


  val ocl = IO(Flipped(HeterogeneousBag.fromNode(ocl_port.out)))
  (ocl zip ocl_port.out) foreach { case (o, (i, _)) => i <> o }

  val mem: Seq[AXI4Bundle] = dram_ports.in.map(a => {
    val q: AXI4BundleParameters = a._1.params
    IO(new AXI4Bundle(a._1.params))
  })

  // make incoming dma port and connect it

  if (p(HasDMA)) {
    val q = IO(Flipped(new AXI4Bundle(outer.dram_ports.in(0)._1.params)))
    outer.dma_port.get.out(0)._1 <> q
  }
  //  val axi4_mem = IO(HeterogeneousBag.fromNode(dram_ports.in))
  (mem zip dram_ports.in) foreach { case (i, (o, _)) => i <> o }

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
    is(16.U) {
      axil_hub.module.io.rocc_out.bits.data := arCnt
    }
    is(17.U) {
      axil_hub.module.io.rocc_out.bits.data := awCnt
    }
    is(18.U) {
      axil_hub.module.io.rocc_out.bits.data := rCnt
    }
    is(19.U) {
      axil_hub.module.io.rocc_out.bits.data := wCnt
    }
    is(20.U) {
      axil_hub.module.io.rocc_out.bits.data := bCnt
    }
    is(21.U) {
      axil_hub.module.io.rocc_out.bits.data := rWait
    }
    is(22.U) {
      axil_hub.module.io.rocc_out.bits.data := bWait
    }
  }
}
