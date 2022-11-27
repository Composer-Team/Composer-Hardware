package composer

import chisel3._
import chisel3.util._
import composer.ComposerTop.getAddressSet
import composer.CppGenerationUtils.genCPPHeader
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import scala.language.implicitConversions


object ComposerTop {
  def getAddressSet(ddrChannel: Int)(implicit p: Parameters): AddressSet = {
    def getAddressMask(addrBits: Int, baseTotal: Long, idx: Int = 0, acc: Long = 0): Long = {
      if (addrBits == 0) acc
      else if (((baseTotal >> idx) & 1) != 0) getAddressMask(addrBits, baseTotal, idx + 1, acc)
      else getAddressMask(addrBits - 1, baseTotal, idx + 1, acc | (1L << idx))
    }

    val nMemChannels = p(ExtMem).get.nMemoryChannels
//    val lineSize = p(CacheBlockBytes)
    val lineSize = 1L << 34;
    val baseTotal = (nMemChannels - 1) * lineSize
    println(baseTotal)
    val amask = getAddressMask(log2Up(p(ExtMem).get.master.size), baseTotal)
    AddressSet(lineSize * ddrChannel, amask)
  }
}
class ComposerTop(implicit p: Parameters) extends LazyModule() {

  private val externalMemParams: MemoryPortParams = p(ExtMem).get
  private val lineSize = p(CacheBlockBytes)
  println("line size is " + lineSize)
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
    val as = getAddressSet(channel)
    println(as)
    AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = Seq(as),
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


  val dma_port = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "dma",
      maxFlight = Some(2),
      aligned = true,
      id = IdRange(0, 1),
    ))
  )))

  val dram_channel_xbars = Seq.fill(nMemChannels)(AXI4Xbar())
  val dmaxbar = AXI4Xbar()
  dmaxbar := dma_port

  dram_channel_xbars foreach ( dram_ports := _)
  dram_channel_xbars foreach { _ := dmaxbar }
  // TODO think about IDs overlapping....

  // We have to share shell DDR ports with DMA bus (which is AXI4). Use RocketChip utils to do that instead of the
  // whole shebang with instantiating strange encrypted Xilinx IPs'

  acc.mem zip dram_channel_xbars foreach { case (m, dram_channel_xbar) =>
    (dram_channel_xbar
      := AXI4UserYanker()
      //:= AXI4IdIndexer(idBits = 9)
      := AXI4Deinterleaver(64)
      := TLToAXI4()
      // TODO CHECK WITH LISA - This component shrinks TL transactions down to 32B at a time, allowing less resource
      //  usage in readers/writers?
      := TLWidthWidget(64)
      := m)
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
    println("qos bits: " + q.qosBits)
    IO(new AXI4Bundle(a._1.params))
  })

  // make incoming dma port and connect it
  val dma = IO(Flipped(new AXI4Bundle(outer.dram_ports.in(0)._1.params)))
  outer.dma_port.out(0)._1 <> dma

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
