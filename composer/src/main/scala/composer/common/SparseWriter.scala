package composer.common

import composer._

import chisel3._
import chisel3.util._

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.config.Parameters


class SparseWriteIO(maxBytes: Int, addressBits: Int) extends Bundle {
  val req = Flipped(new DataChannelIO((addressBits / 8) + 1))
  val channel = Flipped(new DataChannelIO(maxBytes))
  val start = Flipped(Decoupled(Bool()))
  val busy = Output(Bool())
}

abstract class AbstractSparseWriteChannel(nMemXacts: Int)(implicit p: Parameters)
  extends LazyModule {
  val lineSize = p(CacheBlockBytes)
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "SparseWriteChannel",
    sourceId = IdRange(0, nMemXacts),
    supportsProbe = TransferSizes(1, lineSize),
    supportsPutFull = TransferSizes(1, lineSize)
  )))))
  val module: AbstractSparseWriteChannelModule
}

abstract class AbstractSparseWriteChannelModule(outer: AbstractSparseWriteChannel, nMemXacts: Int)
                                               (implicit p: Parameters) extends LazyModuleImp(outer) {

  def io: SparseWriteIO

  val s_idle :: s_addr :: s_data :: s_mem :: s_finishing :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val wdata: UInt
  val wmask: UInt

  val (tl, edge) = outer.node.out(0)

  // get TL parameters from edge
  val beatBytes = edge.manager.beatBytes
  val addressBits = log2Up(edge.manager.maxAddress)
  val sizeBits = edge.bundle.sizeBits

  val fullAddr = Reg(UInt(addressBits.W))

  val xactBusy = RegInit(0.U(nMemXacts.W))
  val xactOneHot = PriorityEncoderOH(~xactBusy)

  def initUpdate(): Unit

  def addrUpdate(): Unit

  def dataUpdate(): Unit

  def memUpdate(): Unit

  def beatFinished(): Bool

  def dataFinished(): Bool

  def memFinished(): Bool

  def sendFinished(): Bool

  def flush(): Bool

  def elaborate(): Unit = {
    when(io.start.fire) {
      initUpdate()
      state := s_addr
    }

    // could have req and channel fire at same time
    when(io.req.data.fire) {
      addrUpdate()
      state := s_data
    }
    when(state === s_addr && memFinished()) {
      state := s_finishing
    }

    when(io.channel.data.fire) {
      dataUpdate()
      state := s_mem
    }

    when(tl.a.fire) {
      memUpdate()
      when(sendFinished()) {
        state := Mux(memFinished(), s_finishing, s_addr)
      }
    }

    val gntId = tl.d.bits.source
    xactBusy := (xactBusy | Mux(tl.a.fire, xactOneHot, 0.U)) &
      ~Mux(tl.d.fire, UIntToOH(gntId), 0.U)

    // TO-DO: make sure req and channel queues get flushed when finished...
    io.start.ready := state === s_idle
    io.req.data.ready := state === s_addr || state === s_finishing
    io.channel.data.ready := state === s_data || state === s_finishing
    when(state === s_finishing) {
      when(!io.channel.data.valid && !io.req.data.valid && !xactBusy.orR) {
        state := s_idle
      }
    }
    tl.a.valid := state === s_mem && !xactBusy.andR
    tl.a.bits := edge.Put(
      fromSource = OHToUInt(xactOneHot),
      toAddress = fullAddr,
      lgSize = log2Ceil(beatBytes).U,
      data = wdata,
      mask = wmask)._2

    tl.d.ready := xactBusy.orR
    io.busy := state =/= s_idle || xactBusy.orR
  }

}

class FixedSparseWriteChannel(nBytes: Int, nMemXacts: Int)(implicit p: Parameters)
  extends AbstractSparseWriteChannel(nMemXacts)(p) {
  override lazy val module = new FixedSparseWriteChannelModule(this, nBytes, nMemXacts)
}

/**
 * Writes out a set number of fixed-size items sequentially to memory.
 *
 * @param nBytes    the number of bytes in a single item
 * @param nMemXacts the number of outstanding writes that can be sent at a time
 */
class FixedSparseWriteChannelModule(outer: FixedSparseWriteChannel, nBytes: Int, nMemXacts: Int)
                                   (implicit p: Parameters) extends AbstractSparseWriteChannelModule(outer, nMemXacts)(p) {

  private val nBits = nBytes * 8
  val io = IO(new SparseWriteIO(nBytes, addressBits))

  require(nBytes >= 1)
  require(nBytes <= beatBytes)
  require(isPow2(nBytes))

  val wordsPerBeat = beatBytes / nBytes

  val idx = Reg(UInt(log2Ceil(wordsPerBeat).W))

  val dataBuf = Reg(Vec(wordsPerBeat, UInt(nBits.W)))
  val dataValid = Reg(UInt(wordsPerBeat.W))

  //val fullAddr = req.addr

  val wdata = dataBuf.asUInt
  val wmask = FillInterleaved(nBytes, dataValid)

  val finishedBuf = RegInit(false.B)


  def initUpdate(): Unit = {
    finishedBuf := false.B
  }

  def addrUpdate(): Unit = {
    fullAddr := io.req.data.bits

    if (nBytes == beatBytes) {
      idx := 0.U
    } else {
      idx := io.req.data.bits(log2Ceil(beatBytes), log2Ceil(nBytes))
    }

    // to-do: this should be on req fire
    if (nBytes > 1) {
      assert(io.req.data.bits(log2Ceil(nBytes) - 1, 0) === 0.U,
        "FixedSparseWriteChannel: Unaligned request")
    }

    dataValid := 0.U
  }

  def dataUpdate(): Unit = {
    dataBuf(idx) := io.channel.data.bits
    dataValid := dataValid | UIntToOH(idx)
  }

  def memUpdate(): Unit = {
    idx := 0.U
    dataValid := 0.U
  }

  def beatFinished(): Bool = idx === (wordsPerBeat - 1).U

  def dataFinished(): Bool = io.channel.finished || finishedBuf

  def memFinished(): Bool = io.channel.finished || finishedBuf

  def sendFinished(): Bool = true.B

  def flush(): Bool = io.channel.finished || finishedBuf

  when(io.channel.finished) {
    finishedBuf := true.B
  }

  elaborate()
}
