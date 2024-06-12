// See LICENSE.SiFive for license details.

package beethoven.TLManagement

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import beethoven.common.CLog2Up
import beethoven.platform
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class TLToAXI4W(val addressSet: AddressSet, idMax: Int)(implicit p: Parameters) extends LazyModule {
  /**
   * I'm developing this TLToAXI4 converter because it allows for TL-based systems to utilize
   * both AXI busses at once. Otherwise, we're effectively halving our bus width. Masters with
   * read & write support should filter their requests first.
   */

  val axi_client = AXI4MasterNode(
    portParams = Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "TLToAXI4SRW",
        id = IdRange(0, idMax),
        aligned = true,
        maxFlight = Some(1)
      )))))
  val defaultTransferSizes = TransferSizes(
    platform.extMem.master.beatBytes,
    platform.extMem.master.beatBytes * platform.prefetchSourceMultiplicity)

  val tlWriter = TLManagerNode(
    portParams = Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = Seq(addressSet),
        supportsGet = TransferSizes.none,
        supportsPutFull = defaultTransferSizes,
        supportsPutPartial = TransferSizes.none,
      )),
      beatBytes = platform.extMem.master.beatBytes)))

  lazy val module = new TLToAXI4WImpl(this)
}

class TLToAXI4WImpl(outer: TLToAXI4W) extends LazyModuleImp(outer) {

  // ensure that all managers and clients have the same width
  val out_width = outer.axi_client.out(0)._2.bundle.dataBits
  require(outer.tlWriter.in(0)._2.bundle.dataBits == out_width)

  val (in, edgeIn) = outer.tlWriter.in(0)
  val (out, _) = outer.axi_client.out(0)
  // Fan out the ARW channel to AR and AW
  val axiLen = (UIntToOH(in.a.bits.size) >> CLog2Up(out_width / 8)).asUInt - 1.U
  // use queues to obey the irrevocability of AXI4 whereas TL components aren't
  // irrevocable
  val aw_queue = Module(new Queue(new AXI4BundleAW(out.params), entries = 4))
  val w_queue = Module(new Queue(new AXI4BundleW(out.params), entries = 4))
  val wBeatCounter = RegInit(0.U(axiLen.getWidth.W))
  val doingLongWriteTx = RegInit(false.B)

  aw_queue.io.enq.bits.id := in.a.bits.source
  aw_queue.io.enq.bits.addr := edgeIn.address(in.a.bits)
  aw_queue.io.enq.bits.burst := AXI4Parameters.BURST_INCR
  aw_queue.io.enq.bits.cache := 0.U
  aw_queue.io.enq.bits.len := axiLen
  aw_queue.io.enq.bits.size := CLog2Up(out_width / 8).U
  aw_queue.io.enq.bits.lock := 0.U // AXI3 backwards compat
  aw_queue.io.enq.bits.prot := AXI4Parameters.PROT_INSECURE
  aw_queue.io.enq.bits.qos := 0.U
  aw_queue.io.enq.bits.user := in.a.bits.user
  // Currently only support PutFull, so all the data will be
  // present in the same beat
  w_queue.io.enq.bits.last := (axiLen === 0.U) || (doingLongWriteTx && wBeatCounter === axiLen)
  when(doingLongWriteTx && w_queue.io.enq.fire) {
    wBeatCounter := wBeatCounter + 1.U
    when(wBeatCounter === axiLen) {
      wBeatCounter := 0.U
      doingLongWriteTx := false.B
    }
  }
  w_queue.io.enq.bits.user := in.a.bits.user
  w_queue.io.enq.bits.data := in.a.bits.data
  w_queue.io.enq.bits.strb := in.a.bits.mask
  aw_queue.io.enq.valid := in.a.valid && w_queue.io.enq.ready && !doingLongWriteTx
  w_queue.io.enq.valid := in.a.valid && (aw_queue.io.enq.ready || doingLongWriteTx)
  in.a.ready := (aw_queue.io.enq.ready || doingLongWriteTx) && w_queue.io.enq.ready
  when(aw_queue.io.enq.fire && axiLen > 0.U) {
    doingLongWriteTx := true.B
    wBeatCounter := wBeatCounter + 1.U
  }

  out.aw <> aw_queue.io.deq
  out.w <> w_queue.io.deq

  in.d.bits := edgeIn.AccessAck(
    toSource = out.b.bits.id,
    lgSize = 0.U,
    denied = out.b.bits.resp =/= AXI4Parameters.RESP_OKAY)
  in.d.valid := out.b.valid
  out.b.ready := in.d.ready
}
