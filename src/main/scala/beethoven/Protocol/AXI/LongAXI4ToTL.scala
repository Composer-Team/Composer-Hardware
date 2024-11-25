package beethoven.Protocol.AXI

import beethoven.MemoryStreams.Memory
import beethoven.common.{CLog2Up, ShiftReg}
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
    val wtx_len = RegInit(0.U(9.W))
    val s_idle :: s_axi :: s_tl :: s_drain :: s_resp_tl :: s_resp_axi :: Nil = Enum(6)
    val state = RegInit(s_idle)
    axi_in.aw.ready := state === s_idle
    val writeQ = Module(new Queue[UInt](axi_in.w.bits.data.cloneType, maxTxLen))
    when(axi_in.aw.fire) {
      wtx_len := axi_in.aw.bits.len +& 1.U
      state := s_axi
    }
    axi_in.w.ready := state === s_axi
    val waddr = Reg(axi_in.aw.bits.addr.cloneType)
    val wid = Reg(axi_in.aw.bits.id.cloneType)
    val wnbeats = Reg(UInt(log2Up(65).W))
    when(axi_in.aw.fire) {
      waddr := axi_in.aw.bits.addr
      wid := axi_in.aw.bits.id
      wnbeats := axi_in.aw.bits.len +& 1.U
    }
    tl_out.a.bits.address := waddr
    writeQ.io.enq.valid := state === s_axi && axi_in.w.fire
    writeQ.io.enq.bits := axi_in.w.bits.data
    writeQ.io.deq.ready := state === s_tl && tl_out.a.fire
    val firstWrite = Reg(Bool())
    val isBigReg = Reg(Bool())
    val isBig = Mux(firstWrite, wnbeats >= 8.U, isBigReg)
    val asz = Mux(isBig, (CLog2Up(axi_in.w.bits.data.getWidth/8)+3).U, CLog2Up(axi_in.w.bits.data.getWidth/8).U)
    when(axi_in.w.fire && axi_in.w.bits.last) {
      state := s_tl
      firstWrite := true.B
    }

    val DO_WRITE = WireInit(false.B)
    tl_out.a.valid := DO_WRITE
    writeQ.io.deq.ready := DO_WRITE && tl_out.a.ready
    tl_out.a.bits.corrupt := false.B
    tl_out.a.bits.source := WRITE_SOURCE.U
    tl_out.a.bits.address := waddr
    tl_out.a.bits.size := asz
    tl_out.a.bits.data := writeQ.io.deq.bits
    assert(!(tl_out.a.valid) || (tl_out.a.valid && writeQ.io.deq.ready) || (tl_out.a.valid && tl_out.a.bits.source === READ_SOURCE.U))
    tl_out.a.bits.opcode := TLMessages.PutFullData

    val can_accept_wresp_tl = state === s_resp_tl &&
      tl_out.d.bits.opcode === TLMessages.AccessAck

    axi_in.b.bits.id := wid
    axi_in.b.bits.resp := 0.U
    axi_in.b.valid := false.B

    val writeCount = Reg(UInt(3.W))
    when(state === s_tl) {
      DO_WRITE := true.B
      when (tl_out.a.fire) {
        firstWrite := false.B
        when(firstWrite) {
          isBigReg := wnbeats >= 8.U
        }
        when (wnbeats < 8.U) {
          state := s_resp_tl
        }.otherwise {
          state := s_drain
          writeCount := 7.U
        }
        wnbeats := wnbeats - 1.U
      }
    }.elsewhen(state === s_drain) {
      DO_WRITE := true.B
      when (tl_out.a.fire) {
        writeCount := writeCount - 1.U
        wnbeats := wnbeats - 1.U
        when (writeCount === 1.U) {
          state := s_resp_tl
        }
      }
    }.elsewhen(state === s_resp_tl) {
      when (can_accept_wresp_tl && tl_out.d.valid) {
        when (wnbeats === 0.U) {
          state := s_resp_axi
        }.otherwise {
          state := s_tl
          waddr := waddr + Mux(isBigReg, (axi_in.w.bits.data.getWidth).U, (axi_in.w.bits.data.getWidth/8).U)
          firstWrite := true.B
        }
      }
    }.elsewhen(state === s_resp_axi) {
      axi_in.b.valid := true.B
      when (axi_in.b.fire) {
        state := s_idle
      }
    }

    // read machine gotta be independent
    val r_idle :: r_emit :: r_wait :: Nil = Enum(3)
    val rstate = RegInit(r_idle)

    axi_in.ar.ready := rstate === r_idle
    val rnbeats, rnbeatsout = Reg(UInt(7.W))
    val rnctr = Reg(UInt(4.W))
    val raddr = Reg(axi_in.ar.bits.addr.cloneType)
    val rid = Reg(axi_in.ar.bits.id.cloneType)


    val can_read_accept = tl_out.d.bits.source === READ_SOURCE.U && axi_in.r.ready
    tl_out.d.ready := can_accept_wresp_tl || can_read_accept
    axi_in.r.valid := tl_out.d.bits.source === READ_SOURCE.U && tl_out.d.valid
    axi_in.r.bits.data := tl_out.d.bits.data
    axi_in.r.bits.last := rnbeatsout === 1.U
    axi_in.r.bits.id := rid
    when (axi_in.r.fire) {
      rnbeatsout := rnbeatsout - 1.U
    }
    when (rstate === r_idle) {
      when (axi_in.ar.fire) {
        rnbeats := axi_in.ar.bits.len +& 1.U
        rnbeatsout := axi_in.ar.bits.len +& 1.U
        raddr := axi_in.ar.bits.addr
        rid := axi_in.ar.bits.id
        rstate := r_emit
      }
    }.elsewhen(rstate === r_emit) {
      when (state =/= s_drain && state =/= s_tl) {
        tl_out.a.valid := true.B
        tl_out.a.bits.address := raddr
        tl_out.a.bits.source := READ_SOURCE.U
        val is_big = rnbeats >= 8.U
        val tx_sz_log = Mux(is_big, (log2Up(tl_out.a.bits.data.getWidth/8)+3).U, log2Up(tl_out.a.bits.data.getWidth/8).U)
        val tx_sz = Mux(is_big, 8.U, 1.U)
        rnctr := tx_sz

        tl_out.a.bits.size := tx_sz_log
        tl_out.a.bits.opcode := TLMessages.Get
        tl_out.a.bits.corrupt := false.B
        rnbeats := rnbeats - tx_sz
        raddr := raddr + Cat(tx_sz, 0.U(6.W))
        when (tl_out.a.fire) {
          rstate := r_wait
        }
      }
    }.elsewhen(rstate === r_wait) {
      when (can_read_accept && tl_out.d.valid) {
        rnctr := rnctr - 1.U
        when (rnctr === 1.U) {
          when (rnbeats === 0.U) {
            rstate := r_idle
          }.otherwise {
            rstate := r_emit
          }
        }
      }
    }
    tl_out.a.bits.mask := BigInt("1" * tl_out.a.bits.mask.getWidth, radix = 2).U
  }
}
