package composer

import chisel3._
import chisel3.util._

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tile.{RoCCCommand, RoCCResponse}
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.config.Parameters


class AXILWidget(implicit p: Parameters) extends Widget()(p) {
  override val crFile = LazyModule(new MCRFile(15)(p))
  crFile.node := node
  override lazy val module = new AXILWidgetModule(this)
}

class AXILWidgetModule(outer: AXILWidget) extends WidgetModule(outer) {

  val io = IO(new Bundle {
    val cmds = Decoupled(UInt(nastiXDataBits.W))
    val resp = Flipped(Decoupled(UInt(nastiXDataBits.W)))
    val mem_in  = Decoupled(UInt(nastiXDataBits.W))
    val mem_out = Flipped(Decoupled(UInt(nastiXDataBits.W)))
    val read_addrs = Decoupled(UInt(nastiXDataBits.W))
  })

  val roccCmdFifo = Module(new Queue(UInt(nastiXDataBits.W), 16))
  val roccRespFifo = Module(new Queue(UInt(nastiXDataBits.W), 16))
  val writeFifo = Module(new Queue(UInt(nastiXDataBits.W), 32))
  val readFifo = Module(new Queue(UInt(nastiXDataBits.W), 32))
  val readReqs = Module(new Queue(UInt(nastiXDataBits.W), 32))

  genROReg(roccRespFifo.io.deq.bits, "resp_bits")
  genROReg(roccRespFifo.io.deq.valid, "resp_valid")
  Pulsify(genWORegInit(roccRespFifo.io.deq.ready, "resp_ready", false.B), pulseLength = 1)

  genWOReg(roccCmdFifo.io.enq.bits, "cmd_bits")
  Pulsify(genWORegInit(roccCmdFifo.io.enq.valid, "cmd_valid", false.B), pulseLength = 1)
  genROReg(roccCmdFifo.io.enq.ready, "cmd_ready")

  genWOReg(writeFifo.io.enq.bits, "write_bits")
  Pulsify(genWORegInit(writeFifo.io.enq.valid, "write_valid", false.B), pulseLength = 1)
  genROReg(writeFifo.io.enq.ready, "write_ready")

  genWOReg(readReqs.io.enq.bits, "read_addr_bits")
  Pulsify(genWORegInit(readReqs.io.enq.valid, "read_addr_valid", false.B), pulseLength = 1)
  genROReg(readReqs.io.enq.ready, "read_addr_ready")

  genROReg(readFifo.io.deq.bits, "read_bits")
  genROReg(readFifo.io.deq.valid, "read_valid")
  Pulsify(genWORegInit(readFifo.io.deq.ready, "read_ready", false.B), pulseLength = 1)

  genCRFile()

  printCRs

  io.cmds <> roccCmdFifo.io.deq
  roccRespFifo.io.enq <> io.resp
  io.mem_in <> writeFifo.io.deq
  readFifo.io.enq <> io.mem_out
  io.read_addrs <> readReqs.io.deq
}

// converts a series of 32-bit values from OCL
// into a single 32-bit data 32-bit addr AXIL message
class AXILAggregator(implicit p: Parameters) extends LazyModule {
  // number of in flight commands?
  val numInFlight = 8
  // width of AXI-L bus?
  val nastiXDataBits = 32
  val node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "axil_hub_mem_out",
      id = IdRange(0, numInFlight),
      aligned = true,
      maxFlight = Some(8)
    )),
//    userBits = 0
  )))

  override lazy val module = new AXILAggregatorModule(this)
}

class AXILAggregatorModule(outer: AXILAggregator)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val write_in = Flipped(Decoupled(UInt(outer.nastiXDataBits.W)))
    val read_in = Flipped(Decoupled(UInt(outer.nastiXDataBits.W)))
    val read_out = Decoupled(UInt(outer.nastiXDataBits.W))
  })

  val (out, _) = outer.node.out(0)

  private val tlBlockBytes = p(CacheBlockBytes)
  private val nastiXDataBytes = outer.nastiXDataBits / 8
  private val nastiXAddrBits = out.aw.bits.addr.getWidth

  //writes a cacheBlock at a time with one address input
  val writes = RegInit(0.U(outer.numInFlight.W))
  val writes_onehot = PriorityEncoderOH(~writes)
  val sIdle :: sWriteHasAddrHigh :: sWriteNeedData :: sWriteSend :: sReadHasAddrHigh :: sReadSendAddr :: Nil = Enum(6)
  val state = RegInit(sIdle)
  val writeAddr = RegInit(0.U(nastiXAddrBits.W))
  val writeCounter = RegInit(0.U(log2Ceil(tlBlockBytes/nastiXDataBytes).W))
  val writeData = RegInit(0.U(outer.nastiXDataBits.W))
  val writeID_low = OHToUInt(writes_onehot)
  val reads = RegInit(0.U(outer.numInFlight.W))
  val reads_onehot = PriorityEncoderOH(~reads)
  val readCounter = RegInit(0.U(log2Ceil(tlBlockBytes/nastiXDataBytes).W))
  val readAddr = RegInit(0.U(nastiXAddrBits.W))
  val readID_low = OHToUInt(reads_onehot)
  //high order bits in writeAddr(0)
  //capture a start writeAddress, then increment it

  io.write_in.ready := (state === sIdle ||
    state === sWriteHasAddrHigh ||
    state === sWriteNeedData) &&
    (!writes.andR)
  io.read_in.ready := (state === sIdle || state === sReadHasAddrHigh) &&
    (!reads.andR) && !io.write_in.valid
  out.aw.valid := (state === sWriteSend)
  out.w.valid  := (state === sWriteSend)
  out.b.ready := true.B
  switch (state) {
    is (sIdle) {
      when (io.write_in.fire) {
        writeAddr := Cat(io.write_in.bits(19,0), 0.U(32.W))
        state := sWriteHasAddrHigh
      }.elsewhen(io.read_in.fire) {
        readAddr := Cat(io.read_in.bits(19,0), 0.U(32.W))
        state := sReadHasAddrHigh
      }
    }
    is (sWriteHasAddrHigh) {
      when (io.write_in.fire) {
        writeAddr := writeAddr | io.write_in.bits
        state := sWriteNeedData
      }
    }
    is (sWriteNeedData) {
      when (io.write_in.fire) {
        state := sWriteSend
        writeData := io.write_in.bits
      }
    }
    is (sWriteSend) {
      when (out.aw.fire) {
        writeAddr := writeAddr + nastiXDataBytes.U
      }
      when (out.w.fire) {
        when (writeCounter === ((tlBlockBytes/nastiXDataBytes) - 1).U) {
          state := sIdle
          writeCounter := 0.U
        }.otherwise {
          state := sWriteNeedData
          writeCounter := writeCounter + 1.U
        }
      }
    }
    is (sReadHasAddrHigh) {
      when (io.read_in.fire) {
        readAddr := readAddr | io.read_in.bits
        state := sReadSendAddr
      }
    }
    is (sReadSendAddr) {
      when (out.ar.fire) {
        readAddr := readAddr + nastiXDataBytes.U
        //this could be simplified to use reduce
        when (readCounter === ((tlBlockBytes/nastiXDataBytes) - 1).U) {
          readCounter := 0.U
          state := sIdle
        }.otherwise {
          readCounter := readCounter + 1.U
        }
      }
    }
  }
  out.aw.bits.addr := writeAddr
  out.aw.bits.id := Cat(0.U(2.W), writeID_low)
  out.aw.bits.size := 2.U
  out.aw.bits.len := 0.U
  out.aw.bits.prot := 0.U
  out.aw.bits.burst := 1.U
  out.aw.bits.cache := 0.U
  out.aw.bits.qos := 0.U
  out.w.bits.data := writeData
  out.w.bits.last := 1.U
  out.w.bits.strb := 0xF.U

  writes := (writes | Mux(out.w.fire, writes_onehot, 0.U)).asUInt &
    (~ Mux(out.b.fire, UIntToOH(out.b.bits.id(2,0)).asUInt, 0.U)).asUInt

  //read requests

  out.ar.bits.addr := readAddr
  out.ar.valid := (state === sReadSendAddr)
  out.ar.bits.id := Cat(2.U(2.W),readID_low)
  out.ar.bits.size := 2.U
  out.ar.bits.len := 0.U
  out.ar.bits.burst := 1.U
  out.ar.bits.cache := 0.U
  out.ar.bits.prot := 0.U
  out.ar.bits.qos := 0.U

  io.read_out.bits := out.r.bits.data
  out.r.ready := io.read_out.ready
  io.read_out.valid := out.r.valid

  reads := (reads | Mux(out.ar.fire, reads_onehot, 0.U)).asUInt &
    (~Mux(out.r.fire, UIntToOH(out.r.bits.id(2,0)).asUInt, 0.U)).asUInt
}

class AXILHub(implicit p: Parameters) extends LazyModule {
  // TODO FIGURE OUT
  val axil_aggregator = LazyModule(new AXILAggregator())
  // Widget contains MMIO stuff
  val axil_widget = LazyModule(new AXILWidget()(p))

  val node = AXI4IdentityNode()
  val mem_out = AXI4IdentityNode()

  mem_out := axil_aggregator.node
  axil_widget.node := node
  lazy val module = new AXILHubModule(this)
}

class AXILHubModule(outer: AXILHub)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val rocc_in = Decoupled(new RoCCCommand)
    val rocc_out = Flipped(Decoupled(new RoCCResponse))
  })
  val axil_aggregator:AXILAggregatorModule = outer.axil_aggregator.module
  val axil_widget = outer.axil_widget.module

  val axil_rocc_converter = Module(new AXILRoccConverter)
  val rocc_axil_converter = Module(new RoccAXILConverter)

  axil_widget.io.resp <> rocc_axil_converter.io.out
  rocc_axil_converter.io.rocc <> io.rocc_out
  axil_rocc_converter.io.in <> axil_widget.io.cmds
  io.rocc_in <> axil_rocc_converter.io.rocc

  axil_aggregator.io.write_in <> axil_widget.io.mem_in
  axil_aggregator.io.read_in <> axil_widget.io.read_addrs
  axil_widget.io.mem_out <> axil_aggregator.io.read_out

}
