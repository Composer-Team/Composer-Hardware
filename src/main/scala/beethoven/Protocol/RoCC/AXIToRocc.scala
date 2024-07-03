package beethoven.Protocol.RoCC

import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import beethoven.common.AccelRoccCommand
import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._
import freechips.rocketchip.diplomacy._


class AXIToRocc(implicit p: Parameters) extends LazyModule {
  val node = AXIToRoccNode()
  lazy val module = new LazyModuleImp(this) {
    val in = node.in(0)._1
    val out = node.out(0)._1

    //this should eventually made parameterizable to the nasti width, but the nasti width is currently way wider
    //than the data width we get
    val nBeats = 5 // ((5 * 32).toFloat / bus_bits).ceil.toInt
    val bitsBuffer = Reg(Vec(nBeats, UInt(32.W)))

    val s_idle :: s_write :: s_response :: s_send :: Nil = Enum(4)
    val state = RegInit(s_idle)

    val rocc = Wire(new AccelRoccCommand)

    rocc.inst := bitsBuffer(0).asTypeOf(rocc.inst.cloneType)
    rocc.payload1 := Cat(bitsBuffer(1), bitsBuffer(2))
    rocc.payload2 := Cat(bitsBuffer(3), bitsBuffer(4))

    in.ar.ready := false.B
    in.aw.ready := false.B
    in.w.ready := false.B
    in.r.valid := false.B
    in.r.bits := DontCare
    in.b.valid := false.B

    val id = Reg(UInt(node.in(0)._2.bundle.idBits.W))
    val nbeats_state = Reg(UInt(3.W))
    val nbeats_count = Reg(UInt(3.W))
    when(state === s_idle) {
      in.aw.ready := true.B
      when(in.aw.fire) {
        nbeats_state := in.aw.bits.len
        state := s_write
        nbeats_count := 0.U
        id := in.aw.bits.id
      }
    }.elsewhen(state === s_write) {
      in.w.ready := true.B
      when(in.w.fire) {
        bitsBuffer(nbeats_count) := in.w.bits.data
        nbeats_count := nbeats_count + 1.U
        when(nbeats_count === nbeats_state) {
          state := s_response
        }
      }
    }.elsewhen(state === s_response) {
      in.b.valid := true.B
      in.b.bits.id := id
      in.b.bits.resp := 0.U
      in.b.bits.user := DontCare
      when(in.b.fire) {
        state := s_send
      }
    }.elsewhen(state === s_send) {
      out.req.valid := true.B
      when(out.req.fire) {
        state := s_idle
      }
    }
    out.req.bits := rocc

    val r_idle :: r_read :: r_read_err :: Nil = Enum(2)
    val r_state = RegInit(r_idle)

    val resp_reg = Reg(new AccelRoccCommand)
    val resp_read_so_far = Reg(UInt(3.W))
    val resp_hold_ready = RegInit(true.B)
    val read_len, read_count = Reg(UInt(3.W))
    val read_id = Reg(UInt(node.in(0)._2.bundle.idBits.W))

    out.resp.ready := resp_hold_ready
    when(out.resp.fire) {
      resp_reg := out.resp.bits
      resp_hold_ready := false.B
    }

    when(r_state === r_idle) {
      in.ar.ready := true.B
      when(in.ar.fire) {
        read_len := in.ar.bits.len
        read_count := 0.U
        read_id := in.ar.bits.id
        when(resp_hold_ready) {
          r_state := r_read_err
        }.otherwise {
          r_state := r_read
        }
      }
    }.elsewhen(r_state === r_read) {
      in.r.valid := true.B
      in.r.bits.id := read_id
      in.r.bits.last := read_count === read_len
      in.r.bits.resp := 0.U
      in.r.bits.data := resp_reg.pack()(read_count + resp_read_so_far)
      when(in.r.fire) {
        read_count := read_count + 1.U
        when(read_count === read_len) {
          r_state := r_idle
          resp_read_so_far := resp_read_so_far + 1.U + read_len
          when(read_count + resp_read_so_far >= 5.U) {
            resp_hold_ready := true.B
            resp_read_so_far := 0.U
          }
        }
      }
    }.elsewhen(r_state === r_read_err) {
      in.r.valid := true.B
      in.r.bits.id := read_id
      in.r.bits.last := read_count === read_len
      in.r.bits.resp := 0.U
      in.r.bits.data := 0xFFFFFFFFL.U
      when(in.r.fire) {
        read_count := read_count + 1.U
        when(read_count === read_len) {
          r_state := r_idle
        }
      }
    }
  }
}

object AXIToRocc {
  def apply()(implicit p: Parameters): AXIToRoccNode = LazyModuleWithFloorplan(new AXIToRocc()).node
}