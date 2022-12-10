package composer.MemoryStreams

import chisel3._
import chisel3.util._
import composer._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._

class WriterDataChannelIO(val dWidth: Int)(implicit p: Parameters) extends Bundle {
  val data = Flipped(Decoupled(UInt(dWidth.W)))
  val channelIdle = Output(Bool())
  val finishEarly = Input(Bool())
}

class SequentialWriteChannelIO(addressBits: Int, maxBytes: Int)(implicit p: Parameters) extends Bundle {
  val req = Flipped(Decoupled(new ChannelTransactionBundle(addressBits)))
  val channel = new WriterDataChannelIO(maxBytes * 8)
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

  val s_idle :: s_data :: s_allocate :: s_mem :: Nil = Enum(4)
  val state = RegInit(s_idle)

  val tx_inactive :: tx_inProgress :: Nil = Enum(2)
  val txIDBits = tlparams.sourceBits
  val txStates = RegInit(VecInit(Seq.fill(1 << txIDBits)(tx_inactive)))
  val txPriority = PriorityEncoderOH(txStates map (_ === tx_inactive))

  val haveTransactionToDo = txStates.map(_ === tx_inProgress).reduce(_ || _)
  val haveAvailableTxSlot = txStates.map(_ === tx_inactive).reduce(_ || _)

  // handle TileLink 'd' interface (response from slave)
  tl.d.ready := haveTransactionToDo
  when(tl.d.fire) {
    txStates(tl.d.bits.source) := tx_inactive
  }
  tl.a.valid := false.B

  val isReallyIdle = state === s_idle && !haveTransactionToDo
  io.channel.channelIdle := !haveTransactionToDo

  require(nBytes >= 1)
  require(nBytes <= beatBytes)
  require(isPow2(nBytes))

  val wordsPerBeat = beatBytes / nBytes

  val addr = Reg(UInt(addressBitsChop.W))
  val req_tx_max_length_beats = p(MaxChannelTransactionLenKey) / nBytes
  val req_tx_mlb_bits = log2Up(req_tx_max_length_beats)
  val req_len = Reg(UInt(req_tx_mlb_bits.W))

  val nextAddr = addr + 1.U

  val idx = Reg(UInt(log2Ceil(wordsPerBeat).W))

  val dataBuf = Reg(Vec(wordsPerBeat, UInt(nBits.W)))
  val dataValid = Reg(UInt(wordsPerBeat.W))

  val wdata = dataBuf.asUInt
  val wmask = FillInterleaved(nBytes, dataValid)

  val allocatedTransaction = RegInit(0.U(txIDBits.W))
  val earlyFinish = RegInit(false.B)

  tl.a.bits := DontCare

  switch(state) {
    is(s_idle) {
      when(io.req.fire) {
        if (nBytes == beatBytes) {
          idx := 0.U
        } else {
          idx := io.req.bits.addr(log2Ceil(beatBytes), log2Ceil(nBytes))
        }
        req_len := io.req.bits.len >> log2Up(nBytes)
        addr := io.req.bits.addr >> log2Up(beatBytes)
        dataValid := 0.U

        if (nBytes > 1) {
          assert(io.req.bits.addr(log2Ceil(nBytes) - 1, 0) === 0.U,
            "FixedSequentialWriteChannel: Unaligned request")
        }
        // wait for data from the core
        state := s_data
      }
    }
    is(s_data) {
      when(io.channel.finishEarly) {
        earlyFinish := true.B
        state := s_allocate
      }
      // when the core data channel fires, put the data in the write
      // buffer and update buffer maintance state
      when(io.channel.data.fire) {
        dataBuf(idx) := io.channel.data.bits
        dataValid := dataValid | UIntToOH(idx)
        idx := idx + 1.U
        req_len := req_len - 1.U
        when(idx === (wordsPerBeat - 1).U || req_len === 1.U) {
          state := s_allocate
        }
      }
    }
    is(s_allocate) {
      when(haveAvailableTxSlot) {
        allocatedTransaction := OHToUInt(txPriority)
        state := s_mem
      }
    }
    is(s_mem) {
      tl.a.valid := true.B
      tl.a.bits := edge.Put(
        fromSource = allocatedTransaction,
        toAddress = Cat(addr, 0.U(log2Up(beatBytes).W)),
        lgSize = log2Ceil(beatBytes).U,
        data = wdata,
        mask = wmask)._2

      // handle TileLink 'a' interface (request to slave)
      when(tl.a.fire) {
        txStates(allocatedTransaction) := tx_inProgress
        addr := nextAddr
        idx := 0.U
        dataValid := 0.U
        when(req_len === 0.U || earlyFinish) {
          state := s_idle
          earlyFinish := false.B
        }.otherwise {
          state := s_data
        }
      }
    }
  }

  io.req.ready := isReallyIdle
  io.channel.data.ready := state === s_data
  io.busy := state =/= s_idle
}

