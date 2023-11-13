package composer.Systems

import chipsalliance.rocketchip.config._
import chisel3.util._
import composer._
import composer.Generation.{ConstraintGeneration, LazyModuleWithSLRs}
import composer.RoccHelpers._
import composer.common._
import composer.Platforms._
import composer.Platforms.FPGA._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._


class ComposerSystem(val systemParams: AcceleratorSystemConfig, val system_id: Int, val canBeIntaCoreCommandEndpoint: Boolean, val acc: ComposerAcc)(implicit p: Parameters) extends LazyModuleWithSLRs {
  val nCores = systemParams.nCores
  val memParams = systemParams.memoryChannelParams
  val distributeCores = ConstraintGeneration.canDistributeOverSLRs()
  implicit val baseName = systemParams.name
  val cores: List[(Int, AccelCoreWrapper)] = List.tabulate(nCores) { core_idx: Int =>
    p(PlatformTypeKey) match {
      case PlatformType.ASIC => (core_idx, LazyModule(new AccelCoreWrapper(systemParams, core_idx, system_id, this)))
      case PlatformType.FPGA =>
        (core_idx % p(PlatformNumSLRs), LazyModuleWithSLR(new AccelCoreWrapper(systemParams, core_idx, system_id, this), slr_id = core_idx % p(PlatformNumSLRs)))
      case _ => throw new Exception()
    }
  }

  // we want to avoid high-degree xbars. Recursively make multi-stage xbar network
  // add an extra buffer for cores off the main SLR
  val Seq(r_nodes, w_nodes) = {
    def recursivelyReduceXBar(grp: Seq[TLNode], inc: Int = 0, slr_id: Int): Seq[TLIdentityNode] = {
      def help(a: Seq[Seq[TLNode]]): Seq[TLNode] = {
        a.map { r =>
          val memory_xbar = LazyModuleWithSLR(new TLXbar(), slr_id = slr_id, requestedName = Some(s"memory_xbar_${SLRHelper.getSLRFromIdx(slr_id)}"))

          r.foreach(memory_xbar.node := _)
          val memory_xbar_buffer = LazyModuleWithSLR(new TLBuffer(), slr_id = slr_id, requestedName = Some(s"memory_xbar_buffer_slr${SLRHelper.getSLRFromIdx(slr_id)}"))

          memory_xbar_buffer.node := memory_xbar.node
          memory_xbar_buffer.node
        }
      }

      def mapToEndpoint(x: TLNode): TLIdentityNode = {
        val memory_endpoint_identity = TLIdentityNode()
        memory_endpoint_identity := x
        memory_endpoint_identity
      }

      if (grp.isEmpty) Seq()
      else if (grp.length <= p(CXbarMaxDegree)) grp.map(mapToEndpoint)
      else {
        val groups = grp.grouped(p(CXbarMaxDegree))
        recursivelyReduceXBar(help(groups.toSeq), inc + 1, slr_id).map(mapToEndpoint)
      }
    }

    val reads_per_slr = cores.groupBy(_._1).map(q =>  (q._1, q._2.flatMap(_._2.readerNodes)))
    val writes_per_slr = cores.groupBy(_._1).map(q => (q._1, q._2.flatMap(_._2.writerNodes)))

    val endpoints = Seq(reads_per_slr, writes_per_slr) map { nodes =>
      if (distributeCores) {
        nodes.flatMap { case (slr, clients) =>
          val reduction = recursivelyReduceXBar(clients, slr_id = slr)
          if (reduction.isEmpty) List()
          else if (slr == SLRHelper.getMemoryBusSLR) {
            reduction
          } else {
            // route across the SLR and give to a buffer
            val canal = LazyModuleWithSLR(new TLXbar(), slr_id = slr, requestedName = Some(s"final_slrReduction_xbar_${SLRHelper.getSLRFromIdx(slr)}"))
            reduction foreach { a => canal.node := a }
            val sbuf = LazyModule(new TLBuffer())
            sbuf.suggestName(s"final_slrReduction_buffer_slr${SLRHelper.getSLRFromIdx(slr)}")
            sbuf.node := canal.node

            val endpoint = TLIdentityNode()
            endpoint := sbuf.node
            List(endpoint)
          }
        }.toSeq
      } else nodes.flatMap(_._2).toSeq
    }

    endpoints map { endpoint =>
      recursivelyReduceXBar(endpoint, slr_id = SLRHelper.getMemoryBusSLR)
    }
  }

  /* SEND OUT STUFF*/

  // ROUTE OUTGOING COMMANDS THROUGH HERE
  //  val canIssueCoreCommands = systemParams.canIssueCoreCommandsTo.nonEmpty
  val outgoingCmdXBars = Map.from(systemParams.canIssueCoreCommandsTo.map { target =>
    val xbar = LazyModule(new TLXbar())
    cores.map(_._2.externalCoreCommNodes(target)).foreach { core_target_source => xbar.node := TLBuffer() := core_target_source }
    (target, xbar.node)
  })
  // cores have their own independent command client nodes

  // SEND OUT COMMAND RESPONSES FROM A SYSTEM HERE
  val (outgoingInternalResponseClient, outgoingInternalResponseXbar) = if (canBeIntaCoreCommandEndpoint) {
    val client = TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = s"InternalReponseNode_${systemParams.name}",
        sourceId = IdRange(0, 1),
        supportsProbe = TransferSizes(1 << log2Up(AccelRoccResponse.getWidthBytes)),
        supportsPutFull = TransferSizes(1 << log2Up(AccelRoccResponse.getWidthBytes))
      )))))
    val xbar = LazyModule(new TLXbar())
    xbar.node := client
    (Some(client), Some(xbar.node))
  } else (None, None)

  /* RECIEVE STUFF */

  // INTERNAL COMMAND MANAGER NODE
  val (internalCommandManager, incomingInternalCommandXbar) = if (canBeIntaCoreCommandEndpoint) {
    val l2u_crc = 1 << log2Up(AccelRoccCommand.packLengthBytes)

    val manager = TLManagerNode(Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v2(
        address = Seq(ComposerConsts.getInternalCmdRoutingAddressSet(system_id)),
        supports = TLMasterToSlaveTransferSizes(
          putFull = TransferSizes(l2u_crc)
        ))),
      beatBytes = l2u_crc)))
    val xbar = LazyModule(new TLXbar())
    manager := TLBuffer() := xbar.node

    (Some(manager), Some(xbar.node))
  } else (None, None)

  val incomingInternalResponseHandlers = Map.from(systemParams.canIssueCoreCommandsTo.map { target =>
    val manager = TLManagerNode(
      Seq(TLSlavePortParameters.v1(
        managers = Seq(TLSlaveParameters.v1(
          Seq(ComposerConsts.getInternalCmdRoutingAddressSet(system_id)),
          supportsPutFull = TransferSizes(AccelRoccResponse.getPow2Bytes)
        )), beatBytes = AccelRoccResponse.getPow2Bytes
      )))
    val xbar = LazyModule(new TLXbar())
    manager := xbar.node
    (target, (manager, xbar.node))
  })

  lazy val module = new ComposerSystemImp(this)
}
