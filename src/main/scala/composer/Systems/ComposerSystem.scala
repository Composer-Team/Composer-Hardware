package composer.Systems

import chipsalliance.rocketchip.config._
import chisel3.util._
import composer.Floorplanning.{ConstraintGeneration, LazyModuleWithSLRs}
import composer._
import composer.Generation._
import composer.MemoryStreams._
import composer.RoccHelpers._
import composer.common._
import composer.Platforms._
import composer.Platforms.FPGA._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tilelink._

object ComposerSystem {
}

class ComposerSystem(val systemParams: AcceleratorSystemConfig,
                     val system_id: Int,
                     val canBeIntaCoreCommandEndpoint: Boolean,
                     val acc: ComposerAcc)(implicit p: Parameters) extends LazyModuleWithSLRs {
  val nCores = systemParams.nCores
  val memParams = systemParams.memoryChannelParams
  val blockBytes = p(CacheBlockBytes)
  val distributeCores = ConstraintGeneration.canDistributeOverSLRs()
  implicit val baseName = systemParams.name
  val readers = (0 until nCores) map { core_id =>
    memParams.filter(_.isInstanceOf[CReadChannelParams]).map { para =>
      val param: CReadChannelParams = para.asInstanceOf[CReadChannelParams]
      (param, List.tabulate(para.nChannels) { i =>
        TLClientNode(List(TLMasterPortParameters.v1(
          clients = List(TLMasterParameters.v1(
            name = s"ReadChannel_sys${system_id}_core${core_id}_${para.name}$i",
            supportsGet = TransferSizes(blockBytes, blockBytes * platform.prefetchSourceMultiplicity),
            supportsProbe = TransferSizes(blockBytes, blockBytes * platform.prefetchSourceMultiplicity),
            sourceId = IdRange(0, param.maxInFlightTxs)
          )))))
      })
    }
  }
  val writers = (0 until nCores) map { core_id =>
    memParams.filter(_.isInstanceOf[CWriteChannelParams]).map { mcp =>
      val para = mcp.asInstanceOf[CWriteChannelParams]
      (para,
        List.tabulate(para.nChannels) { i =>
          TLClientNode(List(TLMasterPortParameters.v1(
            List(TLMasterParameters.v1(
              name = s"WriteChannel_sys${system_id}_core${core_id}_${para.name}$i",
              sourceId = IdRange(0, para.maxInFlightTxs),
              supportsPutFull = TransferSizes(1, p(CacheBlockBytes)),
              supportsPutPartial = TransferSizes(1, p(CacheBlockBytes)),
              supportsProbe = TransferSizes(1, p(CacheBlockBytes)))))))
        })
    }
  }
  val scratch_mod = Seq.tabulate(nCores)(coreIdx => memParams.filter(_.isInstanceOf[CScratchpadParams]).map(_.asInstanceOf[CScratchpadParams]).map {
    param =>
      lazy val mod = LazyModuleWithFloorplan(
        param.make,
        slr_id = core2slr(coreIdx),
        name = s"${systemParams.name}_core${coreIdx}_${param.name}")
      (param, mod)
  })
  val readerNodes = (0 until nCores).map(coreIdx =>
    (readers(coreIdx).map(_._2) :+ scratch_mod(coreIdx).map(_._2.mem_reader).filter(_.isDefined).map(_.get)).flatten)
  val writerNodes = (0 until nCores).map(coreIdx =>
    (writers(coreIdx).map(_._2) :+ scratch_mod(coreIdx).map(_._2.mem_writer).filter(_.isDefined).map(_.get)).flatten)
  // these go to external memory
  val externalCoreCommNodes = (0 until nCores).map(coreIdx => Map.from(systemParams.canIssueCoreCommandsTo.map { targetSys =>
    (targetSys, TLClientNode(Seq(TLMasterPortParameters.v1(clients = Seq(TLMasterParameters.v1(
      s"${systemParams.name}_core${coreIdx}_to$targetSys",
      supportsProbe = TransferSizes(1 << log2Up(AccelRoccCommand.packLengthBytes)),
      supportsPutFull = TransferSizes(1 << log2Up(AccelRoccCommand.packLengthBytes))
    ))))))
  }))
  val intraCoreMemSlaveNodes = memParams.filter(_.isInstanceOf[CIntraCoreMemoryPortIn]).map {
    r =>
      val mp = r.asInstanceOf[CIntraCoreMemoryPortIn]
      val memManagerParams = Seq.fill(mp.nChannels)(TLSlavePortParameters.v1(
        managers = Seq(TLSlaveParameters.v1(
          address = Seq(AddressSet(0, mp.nDatas.intValue() * mp.dataWidthBits.intValue() / 8 - 1)),
          regionType = RegionType.IDEMPOTENT,
          supportsPutFull = TransferSizes(mp.dataWidthBits.intValue() / 8)
        )), beatBytes = mp.dataWidthBits.intValue() / 8
      ))
      val xbar_sp_pairs = memManagerParams map { mm_param =>
        val intraCoreMemXbar = TLXbar()
        val sp = LazyModule(new IntraCoreScratchpad(
          asMemorySlave = mm_param,
          dataWidthBits = mp.dataWidthBits,
          nDatas = mp.nDatas,
          latency = mp.latency,
          nPorts = 1,
          readOnly = mp.readOnly))
        sp.mem_slave_node := intraCoreMemXbar
        (intraCoreMemXbar, sp)
      }
      val xbars = xbar_sp_pairs.map(_._1)
      val sps = xbar_sp_pairs.map(_._2)

      (mp.name, xbars, mp, sps)
  }
  val intraCoreMemMasters = memParams.filter(_.isInstanceOf[CIntraCoreMemoryPortOut]).map {
    r =>
      val mp = r.asInstanceOf[CIntraCoreMemoryPortOut]
      val (otherSystemParams, otherSPParams) = try {
        val otherS = p(AcceleratorSystems).filter(_.name == mp.toSystem)(0)
        val otherSP = otherS.memoryChannelParams.filter(_.name == mp.toMemoryPort)(0)
        (otherS, otherSP.asInstanceOf[CIntraCoreMemoryPortIn])
      } catch {
        case a: Exception =>
          System.err.println("Could not properly find system and memory port given by outward memory port params.")
          throw a
      }
      val memMasters = Seq.fill(otherSystemParams.nCores, otherSPParams.nChannels)(TLClientNode(Seq(TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          f"intraCoreMemPortOut_${mp.toSystem}_to_${mp.toMemoryPort}",
          supportsProbe = TransferSizes(otherSPParams.dataWidthBits.intValue() / 8),
          supportsPutFull = TransferSizes(otherSPParams.dataWidthBits.intValue() / 8)
        ))))))
      (mp, memMasters, otherSPParams)
  }


  val Seq(r_nodes, w_nodes) = {
    def recursivelyReduceXBar(grp: Seq[TLNode], inc: Int = 0, slr_id: Int): Seq[TLIdentityNode] = {
      def help(a: Seq[Seq[TLNode]]): Seq[TLNode] = {
        a.map { r =>
          val memory_xbar = LazyModuleWithFloorplan(new TLXbar(), slr_id = slr_id, name = s"memory_xbar_${DieName.getSLRFromIdx(slr_id)}")

          r.foreach(memory_xbar.node := _)
          val memory_xbar_buffer = LazyModuleWithFloorplan(new TLBuffer(), slr_id = slr_id, name = s"memory_xbar_buffer_slr${DieName.getSLRFromIdx(slr_id)}")

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
      else if (grp.length <= platform.xbarMaxDegree) grp.map(mapToEndpoint)
      else {
        val groups = grp.grouped(platform.xbarMaxDegree)
        recursivelyReduceXBar(help(groups.toSeq), inc + 1, slr_id).map(mapToEndpoint)
      }
    }


    def getNodesBySLR(q: IndexedSeq[List[TLClientNode]]): Map[Int, IndexedSeq[TLClientNode]] =
      q.zipWithIndex.groupBy(pr => core2slr(pr._2)).map[Int, IndexedSeq[TLClientNode]](p => (p._1, p._2.flatMap(_._1)))

    val reads_per_slr = getNodesBySLR(readerNodes)
    val writes_per_slr = getNodesBySLR(writerNodes)

    var node_ctr = 0
    val endpoints = Seq(reads_per_slr, writes_per_slr) map { nodes =>
      if (distributeCores) {
        nodes.flatMap { case (slr, clients) =>
          val reduction = recursivelyReduceXBar(clients, slr_id = slr)
          if (reduction.isEmpty) List()
          else if (slr == DieName.getMemoryBusSLR) {
            reduction
          } else {
            // route across the SLR and give to a buffer
            val canal = LazyModuleWithFloorplan(new TLXbar(), slr_id = slr, name = s"slrRed_xb_sys${systemParams.name}_${DieName.getSLRFromIdx(slr)}_${node_ctr}")
            reduction foreach { a => canal.node := a }
            val sbuf = LazyModule(new TLBuffer())
            sbuf.suggestName(s"slrRed_buf_sys${systemParams.name}_${DieName.getSLRFromIdx(slr)}_${node_ctr}")
            node_ctr += 1
            sbuf.node := canal.node

            val endpoint = TLIdentityNode()
            endpoint := sbuf.node
            List(endpoint)
          }
        }.toSeq
      } else nodes.flatMap(_._2).toSeq
    }

    endpoints map { endpoint =>
      recursivelyReduceXBar(endpoint, slr_id = DieName.getMemoryBusSLR)
    }
  }

  /* SEND OUT STUFF*/

  // ROUTE OUTGOING COMMANDS THROUGH HERE
  //  val canIssueCoreCommands = systemParams.canIssueCoreCommandsTo.nonEmpty
  val outgoingCmdXBars = Map.from(systemParams.canIssueCoreCommandsTo.map { target =>
    val xbar = LazyModule(new TLXbar())
    externalCoreCommNodes.map(_(target)).foreach { core_target_source => xbar.node := TLBuffer() := core_target_source }
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
