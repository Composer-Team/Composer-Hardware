package composer

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, MemoryDevice, TransferSizes}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.system.SimAXIMem

class TestHarness(implicit p: Parameters) extends Module {

  val ldut = LazyModule(new ComposerTop)
  val dut = Module(ldut.module)


  val io = IO(new Bundle {
    val ocl = chiselTypeOf(dut.ocl)
  })

  dut.ocl <> io.ocl

  // connect sim axi mem to ddr channels
  val nMemChannels = p(NMemChan)
  private val externalMemParams = p(ExtMem).get
//  private val lineSize = p(CacheBlockBytes)
//  private val device = new MemoryDevice
  val base = AddressSet(externalMemParams.master.base, externalMemParams.master.size - 1)

  dut.axi4_mem.zip(dut.dram_ports.in).map { case (io, (_, edge)) =>
    val mem = LazyModule(new SimAXIMem(edge, base = p(ExtMem).get.master.base, size = p(ExtMem).get.master.size))
    Module(mem.module).suggestName("mem")
    mem.io_axi4.head <> io
    mem
  }

  //  lazy val simMem = LazyModule(new SimAXIMem(
//    edge = AXI4EdgeParameters(
//      master = AXI4MasterPortParameters(Seq.fill(nMemChannels)(AXI4MasterParameters("mem"))),
//      slave = AXI4SlavePortParameters(Seq.tabulate(nMemChannels) { channel =>
//        val filter = AddressSet(channel * lineSize, ~((nMemChannels - 1) * lineSize))
//        AXI4SlaveParameters(base.intersect(filter).toList,
//          supportsRead = TransferSizes(1, p(CacheBlockBytes)),
//          supportsWrite = TransferSizes(1, p(CacheBlockBytes))
//        )
//      },
//        beatBytes = p(CacheBlockBytes)),
//      params = p,
//      sourceInfo = SourceInfo.materialize),
//    0x400000))
//
//  (0 until nMemChannels) foreach { i =>
//    dut.axi4_mem(i) <> simMem.io_axi4(i)
//  }
}
