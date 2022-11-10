package composer.MemoryStreams

import chisel3._
import chisel3.util._
import composer._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tilelink._


class SequentialWriteChannelIO(addressBits: Int, maxBytes: Int)(implicit p: Parameters) extends Bundle {
  val req = Flipped(Decoupled(new ChannelTransactionBundle(addressBits)))
  val channel = Flipped(new DataChannelIO(maxBytes))
  val busy = Output(Bool())
}

/**
  * Writes out a set number of fixed-size items sequentially to memory.
  *
  * @param nBytes the number of bytes in a single item
  */
class SequentialWriter(nBytes: Int, tlparams: TLBundleParameters, edge: TLEdgeOut)
                      (implicit p: Parameters) extends Module {

  private val nBits = nBytes * 8
  // get TL parameters from edge
  val beatBytes = edge.manager.beatBytes
  val addressBits = log2Up(edge.manager.maxAddress)

  val io = IO(new SequentialWriteChannelIO(addressBits, nBytes))

  val tl = IO(new TLBundle(tlparams))

  val s_idle :: s_data :: s_mem :: s_finishing :: Nil = Enum(4)
  val state = RegInit(s_idle)


  lazy val nextAddr = Cat(fullAddr(addressBits - 1, log2Ceil(beatBytes)) + 1.U,
    0.U(log2Ceil(beatBytes).W))

  val txBusyBits = Reg(Vec(p(MaxMemTxsKey), Bool()))
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
      // hardcoded to true.B
      //      when(sendFinished()) {
      state := Mux(memFinished(), s_finishing, s_data)
      txBusyBits := txBusyBits.zip(txPriority).map { case (a: Bool, b: Bool) => a || b }
      //      }
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
  // TODO figure out a sane value for this
  io.channel.stop := false.B

  require(nBytes >= 1)
  require(nBytes <= beatBytes)
  require(isPow2(nBytes))

  val wordsPerBeat = beatBytes / nBytes

  val req = Reg(new ChannelTransactionBundle(addressBits))
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

