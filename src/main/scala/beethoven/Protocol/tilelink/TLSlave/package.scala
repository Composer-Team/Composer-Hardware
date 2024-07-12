package beethoven.Protocol.tilelink

import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._


package object TLSlave {
  case class TLSlaveOnlyBundle(TLBundleParameters: TLBundleParameters) extends Bundle {
    val tl = Valid(new TLBundleA(TLBundleParameters))
  }

  object TLSlaveOnlyImp extends SimpleNodeImp[TLMasterPortParameters, TLSlavePortParameters, TLEdgeOut, TLSlaveOnlyBundle] {

    override def edge(pd: TLMasterPortParameters, pu: TLSlavePortParameters, p: config.Parameters, sourceInfo: SourceInfo): TLEdgeOut = {
      new TLEdgeOut(pd, pu, p, sourceInfo)
    }

    override def bundle(e: TLEdgeOut): TLSlaveOnlyBundle = {
      TLSlaveOnlyBundle(TLBundleParameters(
        log2Up(e.slave.maxAddress),
        e.slave.beatBytes * 8,
        sourceBits = log2Up(e.master.endSourceId),
        sinkBits = log2Up(e.slave.endSinkId),
        sizeBits = log2Up(e.maxLgSize),
        echoFields = e.master.echoFields,
        requestFields = e.master.requestFields,
        responseFields = e.slave.responseFields,
        hasBCE = false
      ))
    }

    override def render(e: TLEdgeOut): RenderedEdge = RenderedEdge(colour = "#00ff00")
  }

  type TLSlaveNode = NodeHandle[TLMasterPortParameters, TLSlavePortParameters, TLEdgeOut, TLSlaveOnlyBundle, TLMasterPortParameters, TLSlavePortParameters, TLEdgeOut, TLSlaveOnlyBundle]

  case class TLSlaveManagerNode(TLSlavePortParameters: TLSlavePortParameters)(implicit valName: ValName, p: Parameters) extends SinkNode(TLSlaveOnlyImp)(pi = Seq(TLSlavePortParameters))
    

  case class TLSlaveBroadcastNode()(implicit valName: ValName) extends NexusNode(TLSlaveOnlyImp)(
    dFn = { mps => require(mps.length == 1, "Only one source node allowed"); mps.head },
    uFn = { sp =>
      // ensure that all of the managers are the same
      val head_slave_set = sp.head.slaves.flatMap(_.address)

      def addr_eq(as1: AddressSet, as2: AddressSet): Boolean = as1.mask == as2.mask && as1.base == as2.base

      sp.tail.map(_.slaves.flatMap(_.address)) foreach { other_addresses =>
        // ensure the slaves are serving identical address spaces
        other_addresses foreach { as =>
          assert(head_slave_set.exists(addr_eq(_, as)))
        }
      }

      sp.head
    })

  case class TLToTLSlaveNode()(implicit valName: ValName) extends MixedAdapterNode(TLImp, TLSlaveOnlyImp)(
    dFn = { mp =>
      require(mp.allSupportGet.none)
      mp
    },
    uFn = { sp => sp })


  case class TLSlaveIdentityNode()(implicit valName: ValName) extends IdentityNode(TLSlaveOnlyImp)()

  case class TLSlaveNexusNode(dFn: Seq[TLMasterPortParameters] => TLMasterPortParameters,
                              uFn: Seq[TLSlavePortParameters] => TLSlavePortParameters)(implicit override val valName: ValName) extends NexusNode(TLSlaveOnlyImp)(dFn, uFn)


}
