package beethoven.TLManagement

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import beethoven.RoccHelpers.BeethovenConsts
import beethoven.common.AccelRoccCommand
import freechips.rocketchip.tilelink._

class TLClientModuleIO(implicit p: Parameters) extends Bundle {
  val dat = UInt(BeethovenConsts.InternalCommandWidth().W)
  val addr = UInt(BeethovenConsts.getInternalCmdRoutingAddressWidth().W)
}
class TLClientModule(tlclient: TLClientNode)(implicit p: Parameters) extends Module {
  val (tlbundle, tledge) = tlclient.out(0)
  val io = IO(Flipped(Decoupled(new TLClientModuleIO)))
  val tl = IO(new TLBundle(tlbundle.params))

  // sanity checks - do not remove
  assert(tlbundle.params.dataBits == BeethovenConsts.InternalCommandWidth())
  assert(tlbundle.params.addressBits == BeethovenConsts.getInternalCmdRoutingAddressWidth())

  val s_canEmit :: s_waitForAck :: Nil = Enum(2)
  val state = RegInit(s_canEmit)

  tl.a.valid := io.valid
  tl.a.bits := tledge
    .Put(
      fromSource = 0.U,
      toAddress = io.bits.addr,
      lgSize = log2Up(tlbundle.params.dataBits / 8).U,
      data = io.bits.dat
    )
    ._2

  when(tl.d.fire) {
    state := s_canEmit
  }

  tl.d.ready := false.B
  when(state === s_canEmit) {
    io.ready := tl.a.ready
    tl.a.valid := io.valid
    when(io.fire) {
      state := s_waitForAck
    }
  }.otherwise {
    tl.a.valid := false.B
    tl.d.ready := true.B
    io.ready := false.B
  }
}
