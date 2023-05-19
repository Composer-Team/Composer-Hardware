package composer.Systems

import chipsalliance.rocketchip.config._
import chisel3.util._
import composer.Generation.{ConstraintGeneration, LazyModuleWithSLRs}
import composer.RoccHelpers._
import composer._
import composer.common._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._



class ComposerSystem(val systemParams: ComposerSystemParams, val system_id: Int)(implicit p: Parameters) extends LazyModuleWithSLRs {
  val nCores = systemParams.nCores
  val coreParams = systemParams.coreParams
  val distributeCores = ConstraintGeneration.canDistributeOverSLRs()
  implicit val baseName = systemParams.name

  val cores = List.tabulate(nCores) { core_idx: Int =>
    implicit val slrId = if (distributeCores) Some(core_idx % p(PlatformNumSLRs)) else None
    (core_idx % p(PlatformNumSLRs), LazyModuleWithSLR(new ComposerCoreWrapper(systemParams, core_idx, system_id)))
  }

  // we want to avoid high-degree xbars. Recursively make multi-stage xbar network
  // add an extra buffer for cores off the main SLR
  val memory_nodes = {
    def recursivelyReduceXBar(grp: Seq[TLNode], post: Option[Int], inc: Int = 0): Seq[TLIdentityNode] = {
      implicit val slrId = post
      def help(a: Seq[Seq[TLNode]]): Seq[TLNode] = {
        a.map { r =>
          val memory_xbar = LazyModuleWithSLR(new TLXbar())

          r.foreach(memory_xbar.node := _)
          val memory_xbar_buffer = LazyModuleWithSLR(new TLBuffer())
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
        recursivelyReduceXBar(help(groups.toSeq), post, inc + 1).map(mapToEndpoint)
      }
    }

    val endpoints = if (distributeCores) {
      val mems_per_slr = (0 until p(PlatformNumSLRs)).map(slr => (slr, cores.filter(_._1 == slr).flatMap(b => b._2.mem_nodes)))
      mems_per_slr.flatMap { case (slr, clients) =>
        val reduction = recursivelyReduceXBar(clients, if (ConstraintGeneration.canDistributeOverSLRs()) Some(slr) else None)
        if (reduction.isEmpty) List()
        // SLR ID 0 refers to default SLR
        else if (slr == 0) {
          reduction
        } else {
          // route across the SLR and give to a buffer
          val sbuf_src = {
            implicit val slrId = Some(slr)
            val canal = LazyModuleWithSLR(new TLXbar())
            reduction foreach { a => canal.node := a }
            val sbuf = LazyModuleWithSLR(new TLBuffer(BufferParams.default))
            sbuf.node := canal.node
            sbuf
          }

          // generate default SLR buffer
          val dbuf = LazyModuleWithSLR(new TLBuffer(BufferParams.default))
          dbuf.node := sbuf_src.node

          val endpoint = TLIdentityNode()
          endpoint := dbuf.node
          List(endpoint)
        }
      }
    } else cores.flatMap(_._2.mem_nodes)
    recursivelyReduceXBar(endpoints, None)
  }

  /* SEND OUT STUFF*/

  // ROUTE OUTGOING COMMANDS THROUGH HERE
  val outgoingCommandXbar = if (systemParams.canIssueCoreCommands) {
    val xbar = LazyModuleWithSLR(new TLXbar())
    cores.foreach(xbar.node := TLBuffer() := _._2.externalCoreCommNodes.get)
    Some(xbar.node)
  } else None
  // cores have their own independent command client nodes

  // SEND OUT COMMAND RESPONSES FROM A SYSTEM HERE
  val (outgoingInternalResponseClient, outgoingInternalResponseXbar) = if (p(RequireInternalCommandRouting)) {
    val client = TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = s"InternalReponseNode_${systemParams.name}",
        sourceId = IdRange(0, 1),
        // TODO this only needs 20B but we ask for 32 - wasteful?
        supportsProbe = TransferSizes(1 << log2Up(ComposerRoccResponse.getWidthBytes)),
        supportsPutFull = TransferSizes(1 << log2Up(ComposerRoccResponse.getWidthBytes))
      )))))
    val xbar = LazyModuleWithSLR(new TLXbar())
    xbar.node := client
    (Some(client), Some(xbar.node))
  } else (None, None)

  /* RECIEVE STUFF */

  // INTERNAL COMMAND MANAGER NODE
  val (internalCommandManager, incomingInternalCommandXbar) = if (p(RequireInternalCommandRouting)) {
    val l2u_crc = 1 << log2Up(ComposerRoccCommand.packLengthBytes)

    val manager = TLManagerNode(Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v2(
        address = Seq(ComposerConsts.getInternalCmdRoutingAddressSet(system_id)),
        supports = TLMasterToSlaveTransferSizes(
          putFull = TransferSizes(l2u_crc)
        ))),
      beatBytes = l2u_crc)))
    val xbar = LazyModuleWithSLR(new TLXbar())
    manager := TLBuffer() := xbar.node

    (Some(manager), Some(xbar.node))
  } else (None, None)

  val (incomingInternalResponseManager, incomingInternalResponseXBar) = if (systemParams.canIssueCoreCommands) {
    val manager = TLManagerNode(
      Seq(TLSlavePortParameters.v1(
        managers = Seq(TLSlaveParameters.v1(
          Seq(ComposerConsts.getInternalCmdRoutingAddressSet(system_id)),
          supportsPutFull = TransferSizes(ComposerInternallyRoutedRoccResponse.getWidthBytes)
        )), beatBytes = ComposerInternallyRoutedRoccResponse.getWidthBytes
      )))
    val xbar = LazyModuleWithSLR(new TLXbar())
    manager := xbar.node
    (Some(manager), Some(xbar.node))
  } else (None, None)

  lazy val module = new ComposerSystemImp(this)
}
