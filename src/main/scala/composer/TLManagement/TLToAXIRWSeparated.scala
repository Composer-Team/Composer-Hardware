// See LICENSE.SiFive for license details.

package composer.TLManagement

import chisel3._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.axi4._
import chisel3.util._
import composer.PrefetchSourceMultiplicity
import composer.common.CLog2Up
import freechips.rocketchip.subsystem.ExtMem
import freechips.rocketchip.tilelink._

class TLToAXI4SRW(val addressSet: AddressSet, idMax: Int)(implicit p: Parameters) extends LazyModule {
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
    p(ExtMem).get.master.beatBytes,
    p(ExtMem).get.master.beatBytes * p(PrefetchSourceMultiplicity))

  val tlReader = TLManagerNode(
    portParams = Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = Seq(addressSet),
        supportsGet = defaultTransferSizes,
        supportsPutFull = TransferSizes.none,
        supportsPutPartial = TransferSizes.none,
      )),
      beatBytes = p(ExtMem).get.master.beatBytes)))

  val tlWriter = TLManagerNode(
    portParams = Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = Seq(addressSet),
        supportsGet = TransferSizes.none,
        supportsPutFull = defaultTransferSizes,
        supportsPutPartial = TransferSizes.none,
      )),
      beatBytes = p(ExtMem).get.master.beatBytes)))

  lazy val module = new TLToAXI4SRWImpl(this)
}

class TLToAXI4SRWImpl(outer: TLToAXI4SRW) extends LazyModuleImp(outer) {
  val tlManagers = Seq(outer.tlReader, outer.tlWriter)
  tlManagers.foreach { manager => require(log2Ceil(manager.in(0)._2.maxLgSize + 1) <= 4) }
  // ensure that all managers and clients have the same width
  val out_width = outer.axi_client.out(0)._2.bundle.dataBits
  tlManagers.foreach { manager => manager.in(0)._2.bundle.dataBits == out_width }

  val (in_write, in_write_edge) = outer.tlWriter.in(0)
  val (in_read, in_read_edge) = outer.tlReader.in(0)
  val (out, _) = outer.axi_client.out(0)
  Seq((in_write, in_write_edge, true), (in_read, in_read_edge, false)) foreach {
    case (in, edgeIn, isWrite) =>
      // Fan out the ARW channel to AR and AW
      val axiLen = (UIntToOH(in.a.bits.size) >> CLog2Up(out_width / 8)).asUInt - 1.U
      if (isWrite) {

        // use queues to obey the irrevocability of AXI4 whereas TL components aren't
        // irrevocable
        val aw_queue = Module(new Queue(new AXI4BundleAW(out.params), entries=4))
        val w_queue = Module(new Queue(new AXI4BundleW(out.params), entries=4))

        aw_queue.io.enq.bits.id := in.a.bits.source
        aw_queue.io.enq.bits.addr := edgeIn.address(in.a.bits)
        aw_queue.io.enq.bits.burst := 1.U // INCR
        aw_queue.io.enq.bits.cache := 0.U
        aw_queue.io.enq.bits.len := axiLen
        aw_queue.io.enq.bits.size := CLog2Up(out_width / 8).U
        aw_queue.io.enq.bits.lock := 0.U // AXI3 backwards compat
        aw_queue.io.enq.bits.prot := AXI4Parameters.PROT_INSECURE
        aw_queue.io.enq.bits.qos := 0.U
        aw_queue.io.enq.bits.user := in.a.bits.user
        // Currently only support PutFull, so all the data will be
        // present in the same beat
        w_queue.io.enq.bits.last := true.B
        w_queue.io.enq.bits.user := in.a.bits.user
        w_queue.io.enq.bits.data := in.a.bits.data
        w_queue.io.enq.bits.strb := in.a.bits.mask

        aw_queue.io.enq.valid := in.a.valid && w_queue.io.enq.ready
        w_queue.io.enq.valid := in.a.valid && aw_queue.io.enq.ready
        in.a.ready := aw_queue.io.enq.ready && w_queue.io.enq.ready

        out.aw <> aw_queue.io.deq
        out.w <> w_queue.io.deq

        in.d.bits := edgeIn.AccessAck(
          toSource = out.b.bits.id,
          lgSize = CLog2Up(out_width / 8).U,
          denied = out.b.bits.resp =/= AXI4Parameters.RESP_OKAY)
        in.d.valid := out.b.valid
        out.b.ready := in.d.ready
      } else {
        out.ar.bits.id := in.a.bits.source
        out.ar.bits.user := in.a.bits.user
        out.ar.bits.size := CLog2Up(out_width / 8).U
        out.ar.bits.len := axiLen
        out.ar.bits.prot := AXI4Parameters.PROT_INSECURE
        out.ar.bits.addr := edgeIn.address(in.a.bits)
        out.ar.valid := in.a.valid
        in.a.ready := out.ar.ready
        out.ar.bits.burst := AXI4Parameters.BURST_INCR
        out.ar.bits.cache := 0.U

        out.r.ready := in.d.ready
        in.d.bits := edgeIn.AccessAck(
          toSource = out.r.bits.id,
          lgSize = CLog2Up(out_width / 8).U,
          data = out.r.bits.data,
          denied = out.r.bits.resp === AXI4Parameters.RESP_DECERR,
          corrupt = out.r.bits.resp =/= AXI4Parameters.RESP_OKAY)
        in.d.valid := out.r.valid

      }
  }
}
