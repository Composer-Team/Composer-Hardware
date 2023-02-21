package composer.TLManagement

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class TLManagerModule(tlbundle: TLBundle, tledge: TLEdgeIn) extends Module {
  val io = IO(Decoupled(UInt(tlbundle.params.dataBits.W)))
  val tl = IO(Flipped(tlbundle.cloneType))

  val s_canRecieveA :: s_Ack :: Nil = Enum(2)
  val state = RegInit(s_canRecieveA)
  val toAck = Reg(UInt(tlbundle.params.sourceBits.W))

  io.valid := false.B
  io.bits := DontCare

  tl.d.valid := false.B
  tl.d.bits := DontCare

  tl.a.ready := false.B

  switch(state) {
    is (s_canRecieveA) {
      io.valid := tl.a.valid
      tl.a.ready := io.ready
      io.bits := tl.a.bits.data
      when (io.fire) {
        state := s_Ack
        toAck := tl.a.bits.source
      }
    }
    is (s_Ack) {
      tl.d.valid := true.B
      tl.d.bits := tledge.AccessAck(toAck, log2Up(tlbundle.params.dataBits/8).U)
      when (tl.d.fire) {
        state := s_canRecieveA
      }
    }
  }
}



