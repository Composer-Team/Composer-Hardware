package composer.Systems

import chipsalliance.rocketchip.config._
import chisel3.util._
import composer.Generation.{ConstraintGeneration, LazyModuleWithSLRs}
import composer.RoccHelpers._
import composer._
import composer.common._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._



class ComposerSystem(val systemParams: ComposerSystemParams, val system_id: Int, val canBeIntaCoreCommandEndpoint: Boolean, val acc: ComposerAcc)(implicit p: Parameters) extends LazyModuleWithSLRs {
  val nCores = systemParams.nCores
  val coreParams = systemParams.coreParams
  val distributeCores = ConstraintGeneration.canDistributeOverSLRs()
  implicit val baseName = systemParams.name
  val cores = List.tabulate(nCores) { core_idx: Int =>
    p(PlatformTypeKey) match {
      case PlatformType.ASIC => (core_idx, LazyModule(new ComposerCoreWrapper(systemParams, core_idx, system_id, this)))
      case PlatformType.FPGA =>
        (core_idx % p(PlatformNumSLRs), LazyModuleWithSLR(new ComposerCoreWrapper(systemParams, core_idx, system_id, this), slr_id = core_idx % p(PlatformNumSLRs)))
      case _ => throw new Exception()
    }
  }

  // we want to avoid high-degree xbars. Recursively make multi-stage xbar network
  // add an extra buffer for cores off the main SLR
  val memory_nodes = {
    def recursivelyReduceXBar(grp: Seq[TLNode], inc: Int = 0, slr_id: Int): Seq[TLIdentityNode] = {
      def help(a: Seq[Seq[TLNode]]): Seq[TLNode] = {
        a.map { r =>
          val memory_xbar = LazyModuleWithSLR(new TLXbar(), slr_id = slr_id)

          r.foreach(memory_xbar.node := _)
          val memory_xbar_buffer = LazyModuleWithSLR(new TLBuffer(), slr_id = slr_id)
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

    val endpoints = if (distributeCores) {
      val mems_per_slr = cores.groupBy(_._1).map(q => (q._1, q._2.flatMap(_._2.mem_nodes)))
      mems_per_slr.flatMap { case (slr, clients) =>
        val reduction = recursivelyReduceXBar(clients, slr_id = slr)
        if (reduction.isEmpty) List()
        else if (slr == SLRConstants.getMemoryBusSLR) {
          reduction
        } else {
          // route across the SLR and give to a buffer
          val sbuf_src = {
            val canal = LazyModuleWithSLR(new TLXbar(), slr_id = slr)
            reduction foreach { a => canal.node := a }
            val sbuf = LazyModuleWithSLR(new TLBuffer(), slr_id = slr)
            sbuf.node := canal.node
            sbuf
          }

          // generate default SLR buffer
          val dbuf = LazyModuleWithSLR(new TLBuffer(), slr_id = SLRConstants.getMemoryBusSLR)
          dbuf.node := sbuf_src.node

          val endpoint = TLIdentityNode()
          endpoint := dbuf.node
          List(endpoint)
        }
      }.toSeq
    } else cores.flatMap(_._2.mem_nodes)

    recursivelyReduceXBar(endpoints, slr_id = SLRConstants.getMemoryBusSLR)
  }

  /* SEND OUT STUFF*/

  // ROUTE OUTGOING COMMANDS THROUGH HERE
//  val canIssueCoreCommands = systemParams.canIssueCoreCommandsTo.nonEmpty
  val outgoingCmdXBars = Map.from(systemParams.canIssueCoreCommandsTo.map { target =>
    val xbar = LazyModule(new TLXbar())
    cores.map(_._2.externalCoreCommNodes(target)).foreach{ core_target_source => xbar.node := TLBuffer() := core_target_source}
    (target, xbar.node)
  })
  // cores have their own independent command client nodes

  // SEND OUT COMMAND RESPONSES FROM A SYSTEM HERE
  val (outgoingInternalResponseClient, outgoingInternalResponseXbar) = if (canBeIntaCoreCommandEndpoint) {
    val client = TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = s"InternalReponseNode_${systemParams.name}",
        sourceId = IdRange(0, 1),
        supportsProbe = TransferSizes(1 << log2Up(ComposerRoccResponse.getWidthBytes)),
        supportsPutFull = TransferSizes(1 << log2Up(ComposerRoccResponse.getWidthBytes))
      )))))
    val xbar = LazyModule(new TLXbar())
    xbar.node := client
    (Some(client), Some(xbar.node))
  } else (None, None)

  /* RECIEVE STUFF */

  // INTERNAL COMMAND MANAGER NODE
  val (internalCommandManager, incomingInternalCommandXbar) = if (canBeIntaCoreCommandEndpoint) {
    val l2u_crc = 1 << log2Up(ComposerRoccCommand.packLengthBytes)

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

  val incomingInternalResponseHandlers = Map.from (systemParams.canIssueCoreCommandsTo.map { target =>
    val manager = TLManagerNode(
      Seq(TLSlavePortParameters.v1(
        managers = Seq(TLSlaveParameters.v1(
          Seq(ComposerConsts.getInternalCmdRoutingAddressSet(system_id)),
          supportsPutFull = TransferSizes(ComposerRoccResponse.getPow2Bytes)
        )), beatBytes = ComposerRoccResponse.getPow2Bytes
      )))
    val xbar = LazyModule(new TLXbar())
    manager := xbar.node
    (target, (manager,xbar.node))
  })

  lazy val module = new ComposerSystemImp(this)
}
