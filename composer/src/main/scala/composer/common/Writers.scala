package composer.common

import composer._

import chisel3._
import chisel3.util._

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.config.Parameters

// TODO CHRIS & UG:
abstract class SequentialWriteChannelIO(maxBytes: Int) extends Bundle {
  val req: DecoupledIO[Data]
  val channel = Flipped(new DataChannelIO(maxBytes))
  val busy = Output(Bool())
}

abstract class AbstractSequentialWriteChannel(nMemXacts: Int)(implicit p: Parameters)
  extends LazyModule {
  val lineSize = p(CacheBlockBytes)
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "FixedSeqWriteChannel",
      sourceId = IdRange(0, nMemXacts),
      supportsProbe = TransferSizes(1, lineSize),
      supportsPutFull = TransferSizes(1, lineSize)
    )))))
  val module: AbstractSequentialWriteChannelModule
}

abstract class AbstractSequentialWriteChannelModule(outer: AbstractSequentialWriteChannel, nMemXacts: Int)
                                                   (implicit p: Parameters) extends LazyModuleImp(outer) {

  def io: SequentialWriteChannelIO

  val s_idle :: s_data :: s_mem :: s_finishing :: Nil = Enum(4)
  val state = RegInit(s_idle)

  val fullAddr: UInt
  val wdata: UInt
  val wmask: UInt

  val (tl, edge) = outer.node.out(0)

  // get TL parameters from edge
  val beatBytes = edge.manager.beatBytes
  val addressBits = log2Up(edge.manager.maxAddress)
  val sizeBits = edge.bundle.sizeBits

  lazy val nextAddr = Cat(fullAddr(addressBits - 1, log2Ceil(beatBytes)) + 1.U,
    0.U(log2Ceil(beatBytes).W))

  val txBusyBits = Reg(Vec(nMemXacts, Bool()))
  val txPriority = PriorityEncoderOH(txBusyBits map (!_))
  val haveBusyTx = txBusyBits.fold(false.B)(_ || _)
  val haveNotBusyTx = !txBusyBits.fold(true.B)(_ && _)


  def elaborate(): Unit = {
    // new start request resets the whole state of the sequential writer
    when(io.req.fire) {
      initUpdate()
      // wait for data from the core
      state := s_data
    }

    // when the buffers are full of data or the user has signaled that
    // it's done writing to the buffer then start writing
    when(state === s_data && buffersFull()) {
      state := s_mem
    }

    // when the core data channel fires, put the data in the write
    // buffer and update buffer maintance state
    when(io.channel.data.fire) {
      dataUpdate()
      when(beatFinished() || dataFinished()) {
        state := s_mem
      }
    }

    // handle TileLink 'a' interface (request to slave)

    when(tl.a.fire) {
      memUpdate()
      when(sendFinished()) {
        state := Mux(memFinished(), s_finishing, s_data)
        txBusyBits := txBusyBits.zip(txPriority).map{case (a: Bool, b: Bool) => a || b }
      }
    }
    // can't issue more transactions than we have room for
    tl.a.valid := state === s_mem && haveNotBusyTx
    tl.a.bits := edge.Put(
      fromSource = OHToUInt(txPriority),
      toAddress = fullAddr,
      lgSize = log2Ceil(beatBytes).U,
      data = wdata,
      mask = wmask)._2

    // handle TileLink 'd' interface (response from slave)
    tl.d.ready := haveBusyTx
    when(tl.d.fire) {
      txBusyBits(tl.d.bits.source) := false.B
    }

    io.req.ready := state === s_idle
    io.channel.data.ready := state === s_data || state === s_finishing
    when(state === s_finishing) {
      when(!io.channel.data.valid && !txBusyBits.fold(false.B)(_ || _)) {
        state := s_idle
      }
    }

    io.busy := state =/= s_idle || haveBusyTx
  }

  def initUpdate(): Unit

  def dataUpdate(): Unit

  def memUpdate(): Unit

  def beatFinished(): Bool

  def dataFinished(): Bool

  def memFinished(): Bool

  def sendFinished(): Bool

  def buffersFull(): Bool
}

class FixedSequentialWriteRequest(addressBits: Int) extends Bundle {
  /** The address to start writing at */
  val addr = UInt(addressBits.W)
  /** The number of items to write */
  val len = UInt(addressBits.W)
}

class FixedSequentialWriteChannel(nBytes: Int, nMemXacts: Int)(implicit p: Parameters)
  extends AbstractSequentialWriteChannel(nMemXacts)(p) {
  override lazy val module = new FixedSequentialWriteChannelModule(this, nBytes, nMemXacts)
}

class FixedSequentialWriteIO(maxBytes: Int, addressBits: Int) extends SequentialWriteChannelIO(maxBytes) {
  val req = Flipped(Decoupled(new FixedSequentialWriteRequest(addressBits)))
}

/**
 * Writes out a set number of fixed-size items sequentially to memory.
 *
 * @param nBytes    the number of bytes in a single item
 * @param nMemXacts the number of outstanding writes that can be sent at a time
 */
class FixedSequentialWriteChannelModule(outer: FixedSequentialWriteChannel, nBytes: Int, nMemXacts: Int)
                                       (implicit p: Parameters) extends AbstractSequentialWriteChannelModule(outer, nMemXacts)(p) {

  private val nBits = nBytes * 8
  val io = IO(new FixedSequentialWriteIO(nBytes, addressBits))

  require(nBytes >= 1)
  require(nBytes <= beatBytes)
  require(isPow2(nBytes))

  val wordsPerBeat = beatBytes / nBytes

  val req = Reg(new FixedSequentialWriteRequest(addressBits))
  val idx = Reg(UInt(log2Ceil(wordsPerBeat).W))

  val dataBuf = Reg(Vec(wordsPerBeat, UInt(nBits.W)))
  val dataValid = Reg(UInt(wordsPerBeat.W))

  val fullAddr = req.addr

  val wdata = dataBuf.asUInt
  val wmask = FillInterleaved(nBytes, dataValid)

  val finishedBuf = RegInit(false.B)


  def initUpdate(): Unit = {
    if (nBytes == beatBytes) {
      idx := 0.U
    } else {
      idx := io.req.bits.addr(log2Ceil(beatBytes), log2Ceil(nBytes))
    }
    req := io.req.bits
    dataValid := 0.U
    finishedBuf := false.B

    if (nBytes > 1) {
      assert(io.req.bits.addr(log2Ceil(nBytes) - 1, 0) === 0.U,
        "FixedSequentialWriteChannel: Unaligned request")
    }
  }

  def dataUpdate(): Unit = {
    dataBuf(idx) := io.channel.data.bits
    dataValid := dataValid | UIntToOH(idx)
    idx := idx + 1.U
    req.len := req.len - 1.U
  }

  def memUpdate(): Unit = {
    req.addr := nextAddr
    idx := 0.U
    dataValid := 0.U
  }

  def beatFinished(): Bool = idx === (wordsPerBeat - 1).U

  def dataFinished(): Bool = req.len === 1.U || io.channel.finished || finishedBuf

  def memFinished(): Bool = req.len === 0.U || io.channel.finished || finishedBuf

  def sendFinished(): Bool = true.B

  def buffersFull(): Bool = io.channel.finished || finishedBuf

  when(io.channel.finished) {
    finishedBuf := true.B
  }

  elaborate()
}

/*
class ParallelFixedSequentialWriteChannel(nBytes: Int, nMemXacts: Int)(implicit p: Parameters)
  extends AbstractSequentialWriteChannel(nMemXacts)(p) {
  override lazy val module = new ParallelFixedSequentialWriteChannelModule(this, nBytes, nMemXacts)
}

class ParallelFixedSequentialWriteIO(w: Int, addressBits: Int) extends SequentialWriteChannelIO(w) {
  val req = Flipped(Decoupled(new FixedSequentialWriteRequest(addressBits)))
  val data = Flipped(Decoupled(Vec(256 / w, UInt(w.W))))

  override def cloneType = new ParallelFixedSequentialWriteIO(w, addressBits).asInstanceOf[this.type]
}


class ParallelFixedSequentialWriteChannelModule(outer: ParallelFixedSequentialWriteChannel, nBytes: Int, nMemXacts: Int)
                                               (implicit p: Parameters) extends AbstractSequentialWriteChannelModule(outer, nMemXacts)(p) {

  private val nBits = nBytes * 8
  val io = IO(new ParallelFixedSequentialWriteIO(nBits, addressBits))

  require(nBytes >= 1)
  require(nBytes < beatBytes)
  require(isPow2(nBytes))

  val elementsPerBeat = beatBytes / nBytes

  val req = Reg(new FixedSequentialWriteRequest(addressBits))
  //val idx = Reg(UInt(log2Ceil(wordsPerBeat).W))

  val dataBuf = Reg(Vec(elementsPerBeat, UInt(nBits.W)))
  //val dataValid = Reg(Bool())

  val fullAddr = req.addr

  val wdata = dataBuf.asUInt
  val wmask = FillInterleaved(beatBytes, 1.U)

  val finishedBuf = RegInit(false.B)


  def initUpdate() {
    //idx := io.req.bits.addr(log2Ceil(beatBytes), log2Ceil(nBytes))
    req := io.req.bits
    finishedBuf := false.B

    if (nBytes > 1) {
      assert(io.req.bits.addr(log2Ceil(nBytes) - 1, 0) === 0.U,
        "FixedSequentialWriteChannel: Unaligned request")
    }
  }

  def dataUpdate() {
    dataBuf := io.data.bits
    //dataValid := dataValid | UIntToOH(idx)
    //idx := idx + 1.U
    when(req.len < elementsPerBeat.U) {
      req.len := 0.U
    }.otherwise {
      req.len := req.len - elementsPerBeat.U
    }
  }

  def memUpdate() {
    req.addr := nextAddr
    //idx := 0.U
    //dataValid := 0.U
  }

  def beatFinished(): Bool = true.B

  def dataFinished(): Bool = req.len <= elementsPerBeat.U || io.finished || finishedBuf

  def memFinished(): Bool = req.len === 0.U || io.finished || finishedBuf

  def sendFinished(): Bool = true.B

  def flush(): Bool = io.finished || finishedBuf

  when(io.finished) {
    finishedBuf := true.B
  }

  elaborate()
}
*/
