package composer.RoccHelpers

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.{AXILSlaveBeatBytes, CmdRespBusWidthBytes}
import freechips.rocketchip.amba.axi4.{AXI4MasterNode, AXI4MasterParameters, AXI4MasterPortParameters}
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.subsystem.CacheBlockBytes

// converts a series of 32-bit values from OCL
// into a single 32-bit data 32-bit addr AXIL message
class AXILAggregator(implicit p: Parameters) extends LazyModule {
  // number of in flight commands?
  val numInFlight = 8
  // width of AXI-L bus?
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
  private val slaveBytes = p(CmdRespBusWidthBytes)
  private val slaveBits = slaveBytes * 8
  private val tlBlockBytes = p(CacheBlockBytes)

  val io = IO(new Bundle {
    val write_in = Flipped(Decoupled(UInt(slaveBits.W)))
    val read_in = Flipped(Decoupled(UInt(slaveBits.W)))
    val read_out = Decoupled(UInt(slaveBits.W))
  })

  val (out, _) = outer.node.out(0)

  private val nastiXAddrBits = out.aw.bits.addr.getWidth

  //writes a cacheBlock at a time with one address input
  val writes = RegInit(0.U(outer.numInFlight.W))
  val writes_onehot = PriorityEncoderOH(~writes)
  val sIdle :: sWriteHasAddrHigh :: sWriteNeedData :: sWriteSend :: sReadHasAddrHigh :: sReadSendAddr :: Nil = Enum(6)
  val state = RegInit(sIdle)
  val writeAddr = RegInit(0.U(nastiXAddrBits.W))
  val writeCounter = RegInit(0.U(log2Ceil(tlBlockBytes/p(AXILSlaveBeatBytes)).W))
  val writeData = RegInit(0.U(slaveBits.W))
  val writeID_low = OHToUInt(writes_onehot)
  val reads = RegInit(0.U(outer.numInFlight.W))
  val reads_onehot = PriorityEncoderOH(~reads)
  val readCounter = RegInit(0.U(log2Ceil(tlBlockBytes/slaveBytes).W))
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
        writeAddr := writeAddr + slaveBytes.U
      }
      when (out.w.fire) {
        when (writeCounter === ((tlBlockBytes/slaveBytes) - 1).U) {
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
        readAddr := readAddr + slaveBytes.U
        //this could be simplified to use reduce
        when (readCounter === ((tlBlockBytes/slaveBytes) - 1).U) {
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

