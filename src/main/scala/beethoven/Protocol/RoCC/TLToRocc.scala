package beethoven.Protocol.RoCC

import beethoven.Generation._
import beethoven.common.AccelRoccCommand
import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.TLMessages

class TLToRocc(implicit p: Parameters) extends LazyModule {
  val node = TLToRoccNode()
  lazy val module = new LazyModuleImp(this) {
    val in = node.in(0)._1
    val out = node.out(0)._1

    //this should eventually made parameterizable to the nasti width, but the nasti width is currently way wider
    //than the data width we get
    val nBeats = 5 // ((5 * 32).toFloat / bus_bits).ceil.toInt
    val bitsBuffer = Reg(Vec(nBeats, UInt(32.W)))

    val s_idle :: s_response :: s_send :: s_read :: Nil = Enum(6)
    val state = RegInit(s_idle)

    val rocc = Wire(new AccelRoccCommand)
    val roccValid = RegInit(false.B)

    rocc.inst := bitsBuffer(0).asTypeOf(rocc.inst.cloneType)
    rocc.payload1 := Cat(bitsBuffer(1), bitsBuffer(2))
    rocc.payload2 := Cat(bitsBuffer(3), bitsBuffer(4))

    val resp_reg = Reg(new AccelRoccCommand)
    val resp_read_so_far = Reg(UInt(3.W))
    val resp_hold_ready = RegInit(true.B)
    val addr_hold = Reg(UInt(2.W))

    out.resp.ready := resp_hold_ready
    when(out.resp.fire) {
      resp_reg := out.resp.bits
      resp_hold_ready := false.B
    }

    in.a.ready := false.B
    in.d.valid := false.B
    in.d.bits := DontCare

    val id = Reg(UInt(node.in(0)._2.bundle.sourceBits.W))
    val read_count = RegInit(0.U(3.W))
    val write_count = RegInit(0.U(3.W))

    val rocc_write_cmd = 0
    val rocc_write_pop_resp = 4
    val rocc_read_resp = 0
    val rocc_read_resp_avail = 4

    CppGeneration.addUserCppDefinition(Seq(
      ("intptr_t", "ROCC_OFF_WRITE_CMD", rocc_write_cmd),
      ("intptr_t", "ROCC_OFF_WRITE_POP_RESP", rocc_write_pop_resp),
      ("intptr_t", "ROCC_OFF_READ_RESP", rocc_read_resp),
      ("intptr_t", "ROCC_OFF_READ_RESP_AVAIL", rocc_read_resp_avail)
    ))

    when(state === s_idle) {
      in.a.ready := true.B
      when(in.a.fire) {
        addr_hold := in.a.bits.address(3, 2)
        id := in.a.bits.source
        switch(in.a.bits.opcode) {
          is(TLMessages.Get) {
            state := s_read
          }
          is(TLMessages.PutFullData) {
            state := s_response
            when(in.a.bits.address === rocc_write_cmd.U) {
              bitsBuffer(write_count) := in.a.bits.data
              write_count := write_count + 1.U
              when(write_count === 4.U) {
                roccValid := true.B
              }
            }.elsewhen(in.a.bits.address === rocc_write_pop_resp.U) {
              resp_hold_ready := true.B
              read_count := 0.U
              state := s_response
            }
          }
        }
      }
    }.elsewhen(state === s_response) {
      in.d.valid := true.B
      in.d.bits := node.in(0)._2.AccessAck(id, 2.U)
      when(in.d.fire) {
        state := s_idle
      }
    }.elsewhen(state === s_read) {
      in.d.valid := true.B
      val read_resp = addr_hold === rocc_read_resp.U
      in.d.bits := node.in(0)._2.AccessAck(id, 2.U, Mux(read_resp, resp_reg.pack()(read_count), !resp_hold_ready))
      when(in.d.fire) {
        state := s_idle
        when(read_resp) {
          read_count := read_count + 1.U
        }
      }
    }
  }
}

object TLToRocc {
  def apply()(implicit p: Parameters): TLToRoccNode = {
    val lm = LazyModule(new TLToRocc())
    lm.node
  }
}
