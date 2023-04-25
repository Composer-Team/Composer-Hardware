package composer.Systems

import chipsalliance.rocketchip.config._
import chisel3.util._
import composer.RoccHelpers._
import composer._
import composer.common._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

/**
 * Weird Code Alert:
 * We create these modules at the System Level
 * Although it would be proper to create a whole new sub-hierarchy, I think for the time being that adds a whole
 * bunch of complexity without any clear benefit besides simplifying clock routing. And since the architecture is
 * supposed to be transparent to the underlying platform (for the developer's sake), I'm making the decision that
 * platform-specific details should not be visible from the architectural-level - at least for the time being. I
 * may be wrong about this
 *
 * Anyways, we deal with SLR routing in the following way:
 *  Round robin assign cores to SLR mappings with preference for the default SLR. This should ease routing but may
 *  overutilize the default SLR because we have so much other logic (e.g. memory subsystem, MMIO drivers) on the
 *  default SLR.
 *
 * For the memory system, we condense all of the memory channels down to a single TL port so that it can be easily
 * routed across the SLR. Previous approaches allowed channels to be routed across the SLR fabric independently but
 * even for a modest number of channels (ie 4), we consumed around 50% of the SLR routing resources. For this reason,
 * we limit it to a single TL channel. Picture below:
 *
 * SLR default               |    SLR secondary
 *                           |
 *                           |             |------channel
 * To dram                   |             |------channel
 * <----X---------buffer---------buffer----X------channel
 *      |                    |
 *      |---mem node         |
 *
 * dTODO handle intra system cmd/resp routing
 *
 * There's some stuff here where we're passing around lambdas. We do this because LazyModule doesn't promise a clock/
 * reset signal in the contained module. This lambda is basically to enforce lazy evaluation of clock/reset signals
 */

class ComposerSystem(val systemParams: ComposerSystemParams, val system_id: Int)(implicit p: Parameters) extends LazyModule {
  val nCores = systemParams.nCores
  val coreParams = systemParams.coreParams
  val distributeCores = ConstraintGeneration.canDistributeOverSLRs()

  val cores = List.tabulate(nCores) { idx: Int =>
    val mod = LazyModule(new ComposerCoreWrapper(systemParams, idx, system_id))
    val slrID = idx % p(PlatformNumSLRs)
    val modName = f"${systemParams.name}_core$idx" + (if (distributeCores) f"_SLR_$slrID" else "")
    // prioritize SLR1 because that's where the clock signal originate
    mod.suggestName(modName)
    if (distributeCores) ConstraintGeneration.addToSLR(modName, slrID)
    (slrID, mod)
  }

  var flagAlternativeSLR: List[(LazyModule, Int)] = List.empty
  def addToSLRGroup(a: LazyModule, slr: Int): Unit = {
    flagAlternativeSLR = (a, slr) :: flagAlternativeSLR
  }

  // we want to avoid high-degree xbars. Recursively make multi-stage xbar network
  // add an extra buffer for cores off the main SLR
  val memory_nodes = {
    def recursivelyReduceXBar(grp: Seq[TLNode], post: Option[Int], inc: Int = 0): Seq[TLIdentityNode] = {
      def help(a: Seq[Seq[TLNode]]): Seq[TLNode] = {
        a.map { r =>
          val memory_xbar = LazyModule(new TLXbar())

          r.foreach(memory_xbar.node := _)
          val memory_xbar_buffer = LazyModule(new TLBuffer())
          post match {
            case Some(a) =>
              addToSLRGroup(memory_xbar, a)
              addToSLRGroup(memory_xbar_buffer, a)
              val (xb_name, xbb_name) = (f"${systemParams.name}_rec_xb_${a}_$inc",
                f"${systemParams.name}_rec_xbb_${a}_$inc")
              ConstraintGeneration.addToSLR(xb_name, a)
              ConstraintGeneration.addToSLR(xbb_name, a)
              memory_xbar.suggestName(xb_name)
              memory_xbar_buffer.suggestName(xbb_name)
            case None => ;
          }

          memory_xbar_buffer.node := memory_xbar.node
          memory_xbar_buffer.node
        }
      }

      def mapToEndpoint(x: TLNode): TLIdentityNode = {
        val memory_endpoint_identity = TLIdentityNode()
        memory_endpoint_identity := x
        memory_endpoint_identity
      }

      if (grp.length <= p(CXbarMaxDegree)) grp.map(mapToEndpoint)
      else {
        val groups = grp.grouped(p(CXbarMaxDegree))
        recursivelyReduceXBar(help(groups.toSeq), post, inc + 1).map(mapToEndpoint)
      }
    }

    val endpoints = if (distributeCores) {
      val mems_per_slr = (0 until p(PlatformNumSLRs)).map(slr => (slr, cores.filter(_._1 == slr).flatMap(b => b._2.mem_nodes)))
      mems_per_slr.flatMap { case (slr, clients) =>
        val reduction = recursivelyReduceXBar(clients, if (ConstraintGeneration.canDistributeOverSLRs()) Some(slr) else None)
        // SLR ID 0 refers to default SLR
        if (slr == 0) {
          reduction
        } else {
          // route across the SLR
          val canal = LazyModule(new TLXbar())
          reduction foreach { a => canal.node := a }
          val cname = f"${systemParams.name}_xbar$slr"
          canal.suggestName(cname)

          // put two buffers. One on the default SLR, one on the secondary SLR
          val dbuf, sbuf = LazyModule(new TLBuffer(BufferParams.default))
          val dname = f"${systemParams.name}_defaultSLR_Buf_$slr"
          val sname = f"${systemParams.name}_secondarySLR_Buf_$slr"
          dbuf.suggestName(dname)
          sbuf.suggestName(sname)

          ConstraintGeneration.addToSLR(dname, 0)

          // designate
          ConstraintGeneration.addToSLR(cname, slr)
          ConstraintGeneration.addToSLR(sname, slr)
          addToSLRGroup(canal, slr)
          addToSLRGroup(sbuf, slr)

          sbuf.node := dbuf.node
          dbuf.node := canal.node

          val endpoint = TLIdentityNode()
          endpoint := sbuf.node
          List(endpoint)
        }
      }
    } else cores.flatMap(_._2.mem_nodes)
    recursivelyReduceXBar(endpoints, None)
  }

  /* SEND OUT STUFF*/

  // ROUTE OUTGOING COMMANDS THROUGH HERE
  val outgoingCommandXbar = if (systemParams.canIssueCoreCommands) {
    val xbar = LazyModule(new TLXbar())
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
    val xbar = LazyModule(new TLXbar())
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
    val xbar = LazyModule(new TLXbar())
    manager := TLBuffer() := xbar.node

    (Some(manager), Some(xbar.node))
  } else (None, None)

  val (incomingInternalResponseManager, incomingInternalResponseXBar) = if (systemParams.canIssueCoreCommands) {
    val manager = TLManagerNode(
      Seq(TLSlavePortParameters.v1(
        managers = Seq(TLSlaveParameters.v1(
          Seq(ComposerConsts.getInternalCmdRoutingAddressSet(system_id)),
          supportsPutFull = TransferSizes(ComposerRoccResponse.getWidthBytes)
        )), beatBytes = ComposerRoccResponse.getWidthBytes
      )))
    val xbar = LazyModule(new TLXbar())
    manager := xbar.node
    (Some(manager), Some(xbar.node))
  } else (None, None)

  lazy val module = new ComposerSystemImp(this)
}
