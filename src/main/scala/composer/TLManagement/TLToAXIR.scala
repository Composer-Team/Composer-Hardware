// See LICENSE.SiFive for license details.

package composer.TLManagement

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import composer.platform
import composer.common.CLog2Up
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.ExtMem
import freechips.rocketchip.tilelink._

class TLToAXI4R(val addressSet: AddressSet, idMax: Int)(implicit p: Parameters) extends LazyModule {
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

  val tlReader = TLManagerNode(
    portParams = Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = Seq(addressSet),
        supportsGet = defaultTransferSizes,
        supportsPutFull = TransferSizes.none,
        supportsPutPartial = TransferSizes.none,
      )),
      beatBytes = platform.extMem.master.beatBytes)))

  lazy val module = new TLToAXI4RImpl(this)
}

class TLToAXI4RImpl(outer: TLToAXI4R) extends LazyModuleImp(outer) {
  // ensure that all managers and clients have the same width
  val out_width = outer.axi_client.out(0)._2.bundle.dataBits

  val (in, edgeIn) = outer.tlReader.in(0)
  val (out, _) = outer.axi_client.out(0)
  // Fan out the ARW channel to AR and AW
  val axiLen = (UIntToOH(in.a.bits.size) >> CLog2Up(out_width / 8)).asUInt - 1.U
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
