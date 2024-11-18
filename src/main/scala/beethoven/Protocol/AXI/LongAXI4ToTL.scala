package beethoven.Protocol.AXI

import beethoven.MemoryStreams.Memory
import beethoven.common.ShiftReg
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.amba.axi4.AXI4ToTLNode
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLMessages

import scala.annotation.tailrec

class LongAXI4ToTL(maxTxLen: Int)(implicit p: Parameters) extends LazyModule {
  val node = AXI4ToTLNode(wcorrupt = false)

  lazy val module = new LazyModuleImp(this) {
    /**
     * buffer entire write transaction and then burst it over TL
     *
     * If the transaction is now pow2 sized # of beats, then split it up into that (TL-compliance)
     */
    val (axi_in, axi_param) = node.in(0)
    val (tl_out, tl_edge) = node.out(0)

    // let's keep DMA dead-simple for now
    val WRITE_SOURCE = 0
    val READ_SOURCE = 1

    // write machine
    val mem_latency = 2
    val axibuff = Memory(mem_latency, axi_param.bundle.dataBits, maxTxLen, 0, 0, 1)
    axibuff.initLow(clock)
    val wtx_len, wtx_cnt = RegInit(0.U(8.W))
    val s_idle :: s_axi :: s_tl :: s_drain :: s_resp_tl :: s_resp_axi :: Nil = Enum(6)
    val state = RegInit(s_idle)
    axi_in.aw.ready := state === s_idle
    when(axi_in.aw.fire) {
      wtx_len := axi_in.aw.bits.len
      wtx_cnt := 0.U
      state := s_axi
    }
    axi_in.w.ready := state === s_axi
    val waddr = Reg(axi_in.aw.bits.addr.cloneType)
    val wid = Reg(axi_in.aw.bits.id.cloneType)
    val wnbeats = Reg(axi_in.aw.bits.len.cloneType)
    when(axi_in.aw.fire) {
      waddr := axi_in.aw.bits.addr
      wid := axi_in.aw.bits.id
      wnbeats := axi_in.aw.bits.len
    }
    tl_out.a.bits.address := waddr
    val w_tl_mem_read_wire = WireInit(false.B)
    axibuff.addr(0) := wtx_cnt
    axibuff.write_enable(0) := state === s_axi
    axibuff.read_enable(0) := state === s_tl
    axibuff.chip_select(0) := axi_in.w.fire || w_tl_mem_read_wire
    axibuff.data_in(0) := axi_in.w.bits.data
    when(axi_in.w.fire) {
      wtx_cnt := wtx_cnt + 1.U
      when(wtx_cnt === wtx_len) {
        state := s_tl
        wtx_cnt := 0.U
      }
    }
    val writeQ = Module(new Queue[UInt](axi_in.w.bits.data.cloneType, mem_latency + 1))
    val Q_promised_occupancy = RegInit(0.U(log2Up(mem_latency + 2).W))
    val mem_r_valid = ShiftReg(w_tl_mem_read_wire, mem_latency)
    when(w_tl_mem_read_wire && writeQ.io.deq.fire) {
    }.elsewhen(w_tl_mem_read_wire) {
      Q_promised_occupancy := Q_promised_occupancy + 1.U
    }.elsewhen(writeQ.io.deq.fire) {
      Q_promised_occupancy := Q_promised_occupancy - 1.U
    }
    writeQ.io.enq.bits := axibuff.data_out(0)
    writeQ.io.enq.valid := mem_r_valid
    assert(writeQ.io.enq.ready)

    def axilen2tllen(a: UInt): UInt = {
      val out = Wire(tl_out.a.bits.size.cloneType)
      out := 0.U

      @tailrec
      def rec_decide(accidx: Int): Unit = {
        // if accidx can't be stored in out, then quit
        if (accidx >= (1 << out.getWidth)) return
        when(a >= ((1 << accidx)-1).U) {
          out := accidx.U
        }
        rec_decide(accidx+1)
      }

      rec_decide(1)
      out + log2Up(axi_in.w.bits.data.getWidth/8).U
    }

    val can_emit_w_tl = state === s_tl || state === s_drain
    tl_out.a.valid := can_emit_w_tl && writeQ.io.deq.valid
    writeQ.io.deq.ready := can_emit_w_tl && tl_out.a.ready
    tl_out.a.bits.corrupt := false.B
    tl_out.a.bits.source := WRITE_SOURCE.U
    tl_out.a.bits.address := waddr
    tl_out.a.bits.size := axilen2tllen(wnbeats)
    tl_out.a.bits.data := writeQ.io.deq.bits
    tl_out.a.bits.opcode := TLMessages.PutFullData

    val can_accept_wresp_tl = state === s_resp_tl &&
      tl_out.d.bits.opcode === TLMessages.AccessAck

    axi_in.b.bits.id := wid
    axi_in.b.bits.resp := 0.U
    axi_in.b.valid := false.B

    when(state === s_tl) {
      when ((Q_promised_occupancy < (mem_latency + 1).U || writeQ.io.deq.fire) && wtx_cnt <= wtx_len) {
        w_tl_mem_read_wire := true.B
        wtx_cnt := wtx_cnt + 1.U
        when(wtx_cnt === wtx_len) {
          state := s_drain
        }
      }
    }.elsewhen(state === s_drain) {
      when (!writeQ.io.deq.valid) {
        state := s_resp_tl
      }
    }.elsewhen(state === s_resp_tl) {
      when (can_accept_wresp_tl && tl_out.d.valid) {
        state := s_resp_axi
      }
    }.elsewhen(state === s_resp_axi) {
      axi_in.b.valid := true.B
      when (axi_in.b.fire) {
        state := s_idle
      }
    }

    // read machine gotta be independent
    val r_idle :: r_emit :: r_wait :: r_drain :: Nil = Enum(4)
    val rstate = RegInit(r_idle)

    axi_in.ar.ready := rstate === r_idle
    val rnbeats, rnbeatstot = Reg(axi_in.ar.bits.len.cloneType)
    val rnctr = Reg(UInt((rnbeats.getWidth+1).W))
    val raddr = Reg(axi_in.ar.bits.addr.cloneType)
    val rid = Reg(axi_in.ar.bits.id.cloneType)


    tl_out.d.ready := can_accept_wresp_tl || (tl_out.d.bits.source === READ_SOURCE.U && ((rstate === r_wait && axi_in.r.ready) || rstate === r_drain))
    axi_in.r.bits.data := tl_out.d.bits.data
    axi_in.r.bits.last := rnctr === rnbeats
    axi_in.r.bits.id := rid
    when (rstate === r_idle) {
      when (axi_in.ar.fire) {
        rnbeats := axi_in.ar.bits.len
        raddr := axi_in.ar.bits.addr
        rid := axi_in.ar.bits.id
        rnctr := 0.U
        rstate := r_emit
      }
    }.elsewhen(rstate === r_emit) {
      when (state =/= s_drain && state =/= s_tl) {
        tl_out.a.valid := true.B
        val len = axilen2tllen(rnbeats)
        tl_out.a.bits.address := raddr
        tl_out.a.bits.source := READ_SOURCE.U
        tl_out.a.bits.size := len
        tl_out.a.bits.opcode := TLMessages.Get
        tl_out.a.bits.corrupt := false.B
        rnbeatstot := (1.U << (len-log2Up(tl_out.a.bits.data.getWidth/8).U)).asUInt
        when (tl_out.a.fire) {
          rstate := r_wait
        }
      }
    }.elsewhen(rstate === r_wait) {
      axi_in.r.valid := tl_out.d.valid && tl_out.d.bits.source === READ_SOURCE.U
      when (tl_out.d.fire && tl_out.d.bits.source === READ_SOURCE.U) {
        rnctr := rnctr + 1.U
        when (rnctr === rnbeats) {
          rstate := r_drain
        }
      }
    }.elsewhen(rstate === r_drain) {
      when (rnctr === rnbeatstot) {
        rstate := r_idle
      }
      when (tl_out.d.fire && tl_out.d.bits.source === READ_SOURCE.U) {
        rnctr := rnctr + 1.U
      }
    }
    tl_out.a.bits.mask := BigInt("1" * tl_out.a.bits.mask.getWidth, radix = 2).U
  }
}
