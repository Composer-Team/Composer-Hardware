package composer

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import scala.language.implicitConversions

class ComposerTop(implicit p: Parameters) extends LazyModule() {

  private val externalMemParams: MemoryPortParams = p(ExtMem).get
  private val lineSize = p(CacheBlockBytes)
  private val nMemChannels = p(NMemChan)
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
    val base = AddressSet(externalMemParams.master.base, externalMemParams.master.size - 1)
    val filter = AddressSet(channel * lineSize, ~((nMemChannels - 1) * lineSize))
    AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address = base.intersect(filter).toList,
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


  acc.mem.foreach { m =>
    (dram_ports
      := AXI4Buffer()
      := AXI4UserYanker() // generated verilog doesn't appear to have user bits...
      //:= AXI4IdIndexer(idBits = 9)
      := TLToAXI4()
      := TLBuffer() // necessary? TODO measure impact of having buffers?
      := TLWidthWidget(32)
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

  // TODO what in the world does this do?
  val ocl = IO(Flipped(HeterogeneousBag.fromNode(ocl_port.out)))
  (ocl zip ocl_port.out) foreach { case (o, (i, _)) => i <> o }

  val axi4_mem = IO(HeterogeneousBag.fromNode(dram_ports.in))
  (axi4_mem zip dram_ports.in) foreach { case (i, (o, _)) => i <> o }

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
