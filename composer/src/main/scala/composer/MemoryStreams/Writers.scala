package composer.MemoryStreams

import chisel3._
import chisel3.util._
import composer._
import freechips.rocketchip.config.Parameters
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
  val addressBitsChop = addressBits - log2Up(beatBytes)

  val io = IO(new SequentialWriteChannelIO(addressBits, nBytes))

  val tl = IO(new TLBundle(tlparams))

  val s_idle :: s_data :: s_mem :: s_finishing :: Nil = Enum(4)
  val state = RegInit(s_idle)



//  val txBusyBits = RegInit(VecInit(Seq(p(MaxMemTxsKey))(false.B)))
  val tx_inactive :: tx_inProgress :: tx_complete :: Nil = Enum(3)
  val txStates = RegInit(VecInit(Seq.fill(p(MaxMemTxsKey))(tx_complete)))
  val txPriority = PriorityEncoderOH(txStates map (_ === tx_inactive))
  val haveTransactionToDo = txStates.map(_ === tx_inactive).fold(false.B)(_ || _)
  val allTransactionsDone = txStates.map(_ === tx_complete).fold(true.B)(_ && _)

  // handle TileLink 'd' interface (response from slave)
  tl.d.ready := !allTransactionsDone
  when(tl.d.fire) {
    txStates(tl.d.bits.source) := tx_complete
  }
  tl.a.valid := false.B

  // TODO figure out a sane value for this
  io.channel.stop := state === s_idle

  require(nBytes >= 1)
  require(nBytes <= beatBytes)
  require(isPow2(nBytes))

  val wordsPerBeat = beatBytes / nBytes

  val req_addr = Reg(UInt(addressBitsChop.W))
  val req_len = Reg(UInt(log2Up(p(MaxChannelTransactionLenKey)/nBytes).W))

  val nextAddr = req_addr + 1.U

  val idx = Reg(UInt(log2Ceil(wordsPerBeat).W))

  val dataBuf = Reg(Vec(wordsPerBeat, UInt(nBits.W)))
  val dataValid = Reg(UInt(wordsPerBeat.W))

  val wdata = dataBuf.asUInt
  val wmask = FillInterleaved(nBytes, dataValid)

  val finishedBuf = RegInit(false.B)
  val wasFinished = RegInit(false.B)

  when(io.channel.finished) {
    finishedBuf := true.B
  }

  when (finishedBuf && !RegNext(finishedBuf)) {
    wasFinished := true.B
  }

  switch(state) {
    is(s_idle) {
      wasFinished := false.B
      when(io.req.fire) {
        if (nBytes == beatBytes) {
          idx := 0.U
        } else {
          idx := io.req.bits.addr(log2Ceil(beatBytes), log2Ceil(nBytes))
        }
        req_len := io.req.bits.len.head(addressBitsChop)
        req_addr := io.req.bits.addr.head(addressBitsChop)
        dataValid := 0.U
        finishedBuf := false.B

        if (nBytes > 1) {
          assert(io.req.bits.addr(log2Ceil(nBytes) - 1, 0) === 0.U,
            "FixedSequentialWriteChannel: Unaligned request")
        }
        // wait for data from the core
        state := s_data
      }
    }
    is(s_data) {
      when(io.channel.finished || finishedBuf) {
        state := s_mem
      }
      // when the core data channel fires, put the data in the write
      // buffer and update buffer maintance state
      when(io.channel.data.fire) {
        dataBuf(idx) := io.channel.data.bits
        dataValid := dataValid | UIntToOH(idx)
        idx := idx + 1.U
        req_len := req_len - 1.U
        when(idx === (wordsPerBeat - 1).U || req_len === 1.U) {
          state := s_mem
          txStates foreach (_ := tx_inactive)
        }
      }
    }
    is(s_mem) {
      tl.a.valid := haveTransactionToDo
      // handle TileLink 'a' interface (request to slave)
      when(tl.a.fire) {
        req_addr := nextAddr
        idx := 0.U
        dataValid := 0.U

        txStates.zipWithIndex.foreach { case (st, idx) =>
          when (txPriority(idx)){
            st := tx_inProgress
          }
        }
      }
      when(allTransactionsDone) {
        when(req_len === 0.U || wasFinished) {
          state := s_idle
        }.otherwise {
          state := s_data
        }
      }
    }
  }


  tl.a.bits := edge.Put(
    fromSource = OHToUInt(txPriority),
    toAddress = Cat(req_addr, 0.U(log2Up(beatBytes).W)),
    lgSize = log2Ceil(beatBytes).U,
    data = wdata,
    mask = wmask)._2

  io.req.ready := state === s_idle
  io.channel.data.ready := state === s_data || state === s_finishing
  io.busy := state === s_idle
}

