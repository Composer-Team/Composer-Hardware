package beethoven.Systems

import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import chipsalliance.rocketchip.config._
import beethoven.Floorplanning._
import beethoven._
import beethoven.Generation._
import beethoven.MemoryStreams._
import beethoven.Parameters.IntraCoreMemoryPortInConfig.IntraCoreCommunicationDegree
import beethoven.Parameters._
import beethoven.Protocol.RoCC._
import beethoven.Protocol.tilelink.TLSlave.{TLSlaveBuffer, TLSlaveBufferedBroadcast, TLSlaveIdentityNode, TLSlaveNode, TLSlaveXbar, TLToTLSlave}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tilelink._

import scala.annotation.tailrec


class AcceleratorSystem(val nCores: Int, core_offset: Int)(implicit p: Parameters, val systemParams: AcceleratorSystemConfig, on_deviceID: Int) extends LazyModule {
  val system_id = p(AcceleratorSystems).indexWhere(_.name == systemParams.name)
  val memParams = systemParams.memoryChannelConfig
  val blockBytes = p(CacheBlockBytes)
  implicit val baseName = systemParams.name
  val readers = (0 until nCores) map { core_id =>
    memParams.filter(_.isInstanceOf[ReadChannelConfig]).map { para =>
      val param: ReadChannelConfig = para.asInstanceOf[ReadChannelConfig]
      (param, List.tabulate(para.nChannels) { i =>
        TLClientNode(List(TLMasterPortParameters.v1(
          clients = List(TLMasterParameters.v1(
            name = s"ReadChannel_d${on_deviceID}_${systemParams.name}_core${core_id}_${para.name}$i",
            supportsGet = TransferSizes(blockBytes, blockBytes * platform.prefetchSourceMultiplicity),
            supportsProbe = TransferSizes(blockBytes, blockBytes * platform.prefetchSourceMultiplicity),
            sourceId = IdRange(0, param.maxInFlightTxs.getOrElse(platform.defaultReadTXConcurrency))
          )))))
      })
    }
  }
  val writers = (0 until nCores).map { core_id =>
    memParams.filter(_.isInstanceOf[WriteChannelConfig]).map { mcp =>
      val para = mcp.asInstanceOf[WriteChannelConfig]
      (para,
        List.tabulate(para.nChannels) { i =>
          TLClientNode(List(TLMasterPortParameters.v1(
            List(TLMasterParameters.v1(
              name = s"WriteChannel_${systemParams.name}_core${core_id}_${para.name}$i",
              sourceId = IdRange(0, para.maxInFlightTxs.getOrElse(platform.defaultWriteTXConcurrency)),
              supportsPutFull = TransferSizes(blockBytes, blockBytes * platform.prefetchSourceMultiplicity),
              supportsPutPartial = TransferSizes(blockBytes, blockBytes * platform.prefetchSourceMultiplicity),
              supportsProbe = TransferSizes(blockBytes, blockBytes * platform.prefetchSourceMultiplicity))))))
        })
    }
  }
  val scratch_mod = (0 until nCores).map(coreIdx => memParams.filter(_.isInstanceOf[ScratchpadConfig]).map(_.asInstanceOf[ScratchpadConfig]).map {
    param =>
      lazy val mod = LazyModuleWithFloorplan(
        param.make,
        slr_id = core2slr(coreIdx),
        name = s"${systemParams.name}_core${coreIdx}_${param.name}")
      (param, mod)
  })

  val (rocc_node, system_rocc_endpoints) = {
    val nodes = (0 until nCores).map { ci =>
      RoccManagerNode(RoCCSlaveParams.core(system_id, core_offset + ci))
    }

    (fanout_recursive(nodes.map { n =>
      val ri = RoccIdentityNode()
      n := ri
      ri
    }, 4), nodes)
  }

  val rocc_oc = {
    (0 until nCores) map { _ =>
      systemParams.canIssueCoreCommandsTo.map { target =>
        (target, RoccClientNode(RoccMasterParams()))
      }
    }
  }

  val readerNodes = (0 until nCores).map { coreIdx =>
    val extended_readers = (readers(coreIdx).map(_._2) :+
      scratch_mod(coreIdx).map(_._2.mem_reader).filter(_.isDefined).map(_.get)).flatMap {
      a => extend_eles_via_protocol_node[TLNode](a, make_tl_buffer, tl_assign, platform.interCoreMemReductionLatency)
    }
    // reduce the number of endpoints and ensure that the output is a nexus
    //   we do this because for multi-die devices, the endpoint might need to move in
    //   multiple directions (e.g., to another die, or to a physical interface)
    extend_eles_via_protocol_node(xbar_tree_reduce_sources[TLNode](extended_readers,
      platform.xbarMaxDegree,
      platform.maxMemEndpointsPerCore,
      make_tl_xbar,
      make_tl_buffer,
      tl_assign), make_tl_xbar, tl_assign)
  }

  val writerNodes = (0 until nCores).map { coreIdx =>
    val extended_writers = (writers(coreIdx).map(_._2) :+
      scratch_mod(coreIdx).map(_._2.mem_writer).filter(_.isDefined).map(_.get)).flatMap { a =>
      extend_eles_via_protocol_node[TLNode](a, make_tl_buffer, tl_assign, platform.interCoreMemReductionLatency)
    }
    extend_eles_via_protocol_node(xbar_tree_reduce_sources[TLNode](extended_writers,
      platform.xbarMaxDegree,
      platform.maxMemEndpointsPerCore,
      make_tl_xbar,
      make_tl_buffer,
      tl_assign), make_tl_xbar, tl_assign)
  }

  val (intraCoreMemSlaveIOs, intraCoreMemSlaveNode) = {
    val all = (0 until nCores) map { coreIdx =>
      memParams.filter(_.isInstanceOf[IntraCoreMemoryPortInConfig]).map {
        case mp: IntraCoreMemoryPortInConfig =>
          (0 until mp.nChannels) map { channel_id =>
            val ics_mod = LazyModule(new IntraCoreScratchpad(
              dataWidthBits = mp.dataWidthBits,
              nDatas = mp.nDatas,
              latency = mp.latency,
              nPorts = mp.portsPerChannel,
              readOnly = mp.readOnly,
              mp = mp,
              systemParams = systemParams,
              coreIdx = coreIdx,
              channel_id = channel_id))
            val identity: TLSlaveNode = TLSlaveIdentityNode()
            ics_mod.node := identity
            (mp, ics_mod, identity, (coreIdx, channel_id))
          }
      }
    }
    val only_tl_components = all.flatten.flatten.map(a => (a._1, a._3, a._4))
    // group by config
    val squish = only_tl_components.groupBy(_._1).map { case (para, para_groups) =>
      // group by core
      val core_squish: Seq[(TLSlaveNode, Int)] = para_groups.map(a => (a._2, a._3)).groupBy(_._2._1).map { case (core, core_group) =>
        // squish all the channels
        core_group.groupBy(_._2._2).map { case (_, channel_group) =>
          val reduce_lambda = para.communicationDegree match {
            case IntraCoreCommunicationDegree.BroadcastAllCoresChannels | IntraCoreCommunicationDegree.BroadcastAllChannels =>
              () => TLSlaveBufferedBroadcast(0)
            case _ =>
              // crossbar reduce instead of broadcast reduce
              () => TLSlaveXbar()
          }
          val sunk = xbar_tree_reduce_sinks[TLSlaveNode](channel_group.map(_._1), 4, 1, reduce_lambda, () => TLSlaveBuffer(),
            { case (a, b) => a.foreach(b := _) })
          (sunk(0), core)
        }
      }.toList.flatten

      // reduce cores to a point
      val reduce_lambda = para.communicationDegree match {
        case IntraCoreCommunicationDegree.BroadcastAllCoresChannels | IntraCoreCommunicationDegree.BroadcastAllCores =>
          () => TLSlaveBufferedBroadcast(0)
        case _ =>
          // crossbar reduce instead of broadcast reduce
          () => TLSlaveXbar()
      }
      val sunk_cores = xbar_tree_reduce_sinks[TLSlaveNode](core_squish.map(_._1), 2, 1, reduce_lambda, () => TLSlaveBuffer(),
        { case (a, b) => a.foreach(b := _) })

      val tl_head = TLIdentityNode()
      val tl2slave = TLToTLSlave()
      sunk_cores(0) := TLSlaveBuffer() := tl2slave := tl_head
      (para, tl_head)
    }.toList
    val node = if (squish.isEmpty) {
      None
    } else {
      Some(xbar_tree_reduce_sinks(squish.map(_._2), 2, 1, make_tl_xbar, make_tl_buffer, tl_assign)(p)(0))
    }
    (all, node)
  }

  val intraCoreMemMasters = memParams.filter(_.isInstanceOf[IntraCoreMemoryPortOutConfig]).map {
    case mp: IntraCoreMemoryPortOutConfig =>
      val (otherSystemParams, otherSPParams) = try {
        val otherS = p(AcceleratorSystems).filter(_.name == mp.toSystem)(0)
        val otherSP = otherS.memoryChannelConfig.filter(_.name == mp.toMemoryPort)(0)
        (otherS, otherSP.asInstanceOf[IntraCoreMemoryPortInConfig])
      } catch {
        case a: Exception =>
          System.err.println("Could not properly find system and memory port given by outward memory port params.")
          throw a
      }
      val memMasters = Seq.fill(otherSystemParams.nCores)(TLClientNode(Seq.fill(mp.nChannels)(TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          f"intraCoreMemPortOut_${mp.toSystem}_to_${mp.toMemoryPort}",
          supportsProbe = TransferSizes(otherSPParams.dataWidthBits.intValue() / 8),
          supportsPutFull = TransferSizes(otherSPParams.dataWidthBits.intValue() / 8)
        ))))))
      (mp, memMasters)
  }

  val Seq(r_nodes, w_nodes) = Seq(readerNodes, writerNodes).map { all_nodes =>
    xbar_tree_reduce_sources[TLNode](all_nodes.flatten,
      platform.xbarMaxDegree,
      platform.maxMemEndpointsPerSystem,
      make_tl_xbar,
      make_tl_buffer,
      tl_assign)
  }

  lazy val module = new AcceleratorSystemImp(this)
}
