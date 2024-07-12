package beethoven.Protocol.tilelink

import beethoven.Protocol.tilelink.TLSlave
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import chisel3._
import chisel3.util.Queue

class TLRWFilter(spp: TLSlavePortParameters, mpp: TLMasterPortParameters)(implicit p: Parameters) extends LazyModule {

  val in_node = TLManagerNode(Seq(spp))
  val read_out = TLClientNode(
    Seq(TLMasterPortParameters.v1(
      Seq(mpp.masters(0).v1copy(
        supportsPutFull = TransferSizes.none,
        supportsPutPartial = TransferSizes.none)))))

  val write_out = TLClientNode(
    Seq(TLMasterPortParameters.v1(
      Seq(mpp.masters(0).v1copy(
        supportsGet = TransferSizes.none)))))

  lazy val module = new TLRWFilterImp(this)
}

