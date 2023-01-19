package composer.TLManagement

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class TLClientModule(tlclient: TLClientNode) extends Module{
  val (tlbundle, tledge) = tlclient.out(0)
  val io = IO(Flipped(Decoupled(new Bundle() {
    val dat = UInt(tlbundle.params.dataBits.W)
    val addr = UInt(tlbundle.params.addressBits.W)
  })))
  val tl = IO(new TLBundle(tlbundle.params))

  val s_canEmit :: s_waitForAck :: Nil = Enum(2)
  val state = RegInit(s_canEmit)

  io.ready := false.B

  tl.a.valid := false.B
  tl.a.bits := tledge.Put(
    fromSource = 0.U,
    toAddress = io.bits.addr,
    lgSize = log2Up(tlbundle.params.dataBits/8).U,
    data = io.bits.dat
  )._2
  tl.d.ready := false.B

  switch(state) {
    is(s_canEmit) {
      io.ready := tl.a.ready
      when (io.fire) {
        state := s_waitForAck
      }
    }

    is(s_waitForAck) {
      tl.d.ready := true.B
    }
  }
}
