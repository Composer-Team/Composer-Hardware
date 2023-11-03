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
        /** Conservative implementation */
        out.aw.bits.id := in.a.bits.source
        out.aw.bits.addr := edgeIn.address(in.a.bits)
        out.aw.bits.burst := 1.U // INCR
        out.aw.bits.cache := 0.U
        out.aw.bits.len := axiLen
        out.aw.bits.size := CLog2Up(out_width / 8).U
        out.aw.bits.lock := 0.U // AXI3 backwards compat
        out.aw.bits.prot := AXI4Parameters.PROT_INSECURE
        out.aw.bits.qos := 0.U
        out.aw.bits.user := in.a.bits.user
        // Currently only support PutFull, so all the data will be
        // present in the same beat
        out.w.bits.last := true.B
        out.w.bits.user := in.a.bits.user
        out.w.bits.data := in.a.bits.data
        out.w.bits.strb := in.a.bits.mask

        in.d.bits := edgeIn.AccessAck(toSource = out.b.bits.id,
          lgSize = CLog2Up(out_width / 8).U,
          denied = out.b.bits.resp =/= AXI4Parameters.RESP_OKAY)
        in.d.valid := out.b.valid
        out.b.ready := in.d.ready


        val hasAcceptedAddressButNotData = RegInit(false.B)
        val hasAcceptedDataButNotAddress = RegInit(false.B)
        in.a.ready := false.B
        out.aw.valid := false.B
        out.w.valid := false.B

        when (hasAcceptedDataButNotAddress) {
          in.a.ready := out.aw.ready
          out.w.valid := false.B
          out.aw.valid := in.a.valid
          when (out.aw.fire) {
            hasAcceptedDataButNotAddress := false.B
          }
        }.elsewhen(hasAcceptedAddressButNotData) {
          in.a.ready := out.w.ready
          out.aw.valid := false.B
          out.w.valid := in.a.valid
          when (out.w.fire) {
            hasAcceptedAddressButNotData := false.B
          }
        }.otherwise {
          in.a.ready := out.aw.ready && out.w.ready
          out.w.valid := in.a.valid
          out.aw.valid := in.a.valid
          when (out.aw.fire && !out.w.fire) {
            hasAcceptedAddressButNotData := true.B
          }.elsewhen(!out.aw.fire && out.w.fire) {
            hasAcceptedDataButNotAddress := true.B
          }
        }
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
