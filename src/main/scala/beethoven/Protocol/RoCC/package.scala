package beethoven.Protocol

import beethoven.platform
import beethoven.common.{AccelRoccCommand, AccelRoccResponse}
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

package object RoCC {

  class RoccExchange extends Bundle {
    val req = Decoupled(new AccelRoccCommand)
    val resp = Flipped(Decoupled(new AccelRoccResponse()))
  }

  case class RoccMasterParams()

  case class RoCCSlaveParams(system_core_ids: Iterable[(Int, (Int, Int))]) {
    def copy(): RoCCSlaveParams = RoCCSlaveParams(system_core_ids)
  }

  object RoCCSlaveParams {
    def core(systemID: Int, coreID: Int): RoCCSlaveParams = RoCCSlaveParams(Seq((systemID, (coreID, coreID))))
  }

  case class RoccEdge(down: RoccMasterParams, up: RoCCSlaveParams, p: Parameters, si: SourceInfo)


  object RoccNodeImp extends SimpleNodeImp[RoccMasterParams, RoCCSlaveParams, RoccEdge, RoccExchange] {
    override def edge(pd: RoccMasterParams, pu: RoCCSlaveParams, p: config.Parameters, sourceInfo: SourceInfo): RoccEdge = RoccEdge(pd, pu, p, sourceInfo)

    override def bundle(e: RoccEdge): RoccExchange = new RoccExchange

    override def render(e: RoccEdge): RenderedEdge = RenderedEdge(colour = "#00ff00")
  }


  abstract class RoCCAdapterNode(dFn: RoccMasterParams => RoccMasterParams, uFn: RoCCSlaveParams => RoCCSlaveParams)(implicit valName: ValName) extends AdapterNode(RoccNodeImp)(dFn, uFn)

  case class AXIToRoccNode(wcorrupt: Boolean = true)(implicit valName: ValName, p: Parameters) extends MixedAdapterNode(AXI4Imp, RoccNodeImp)(
    dFn = { _ => RoccMasterParams() },
    uFn = { _ =>
      AXI4SlavePortParameters(
        slaves = Seq(AXI4SlaveParameters(
          address = Seq(AddressSet(platform.frontBusBaseAddress, platform.frontBusAddressMask)),
          regionType = RegionType.IDEMPOTENT,
          supportsWrite = TransferSizes(4, 4 * 5),
          supportsRead = TransferSizes(4, 4 * 5),
          interleavedId = Some(1)
        )),
        beatBytes = 4
      )
    })

  case class TLToRoccNode(wcorrupt: Boolean = true)(implicit valName: ValName, p: Parameters) extends MixedAdapterNode(TLImp, RoccNodeImp)(
    dFn = { _ => RoccMasterParams() },
    uFn = { _ =>
      TLSlavePortParameters.v1(
        managers = Seq(TLSlaveParameters.v1(
          address = Seq(AddressSet(platform.frontBusBaseAddress, platform.frontBusAddressMask)),
          supportsGet = TransferSizes(4, 4 * 8),
          supportsPutFull = TransferSizes(4, 4 * 8)
        )), beatBytes = 4, endSinkId = 1
      )
    })

  class RoCCBufferNode(implicit valName: ValName) extends RoCCAdapterNode(
    dFn = { mp => mp },
    uFn = { sp => sp })

  case class RoccIdentityNode()(implicit valName: ValName) extends IdentityNode(RoccNodeImp)()

  case class RoccManagerNode(params: RoCCSlaveParams)(implicit valName: ValName) extends SinkNode(RoccNodeImp)(Seq(params))

  case class RoccClientNode(params: RoccMasterParams)(implicit valName: ValName) extends SourceNode(RoccNodeImp)(Seq(params))

  case class RoccNexusNode(dFn: Seq[RoccMasterParams] => RoccMasterParams,
                           uFn: Seq[RoCCSlaveParams] => RoCCSlaveParams
                          )(implicit valName: ValName) extends NexusNode(RoccNodeImp)(dFn, uFn)


  case class RoccCompositeNexusNode(inNode: RoccNexusNode, outNode: RoccNexusNode)
    extends InwardNodeHandle[RoccMasterParams, RoCCSlaveParams, RoccEdge, RoccExchange]
    with OutwardNodeHandle[RoccMasterParams, RoCCSlaveParams, RoccEdge, RoccExchange]
    with RoccNode {

    override def inward: InwardNode[RoccMasterParams, RoCCSlaveParams, RoccExchange] = inNode

    override def inner: InwardNodeImp[RoccMasterParams, RoCCSlaveParams, RoccEdge, RoccExchange] = inNode.inner

    override def outward: OutwardNode[RoccMasterParams, RoCCSlaveParams, RoccExchange] = outNode

    override def outer: OutwardNodeImp[RoccMasterParams, RoCCSlaveParams, RoccEdge, RoccExchange] = outNode.outer
  }

  type RoccNode = NodeHandle[RoccMasterParams, RoCCSlaveParams, RoccEdge, RoccExchange, RoccMasterParams, RoCCSlaveParams, RoccEdge, RoccExchange]
}



