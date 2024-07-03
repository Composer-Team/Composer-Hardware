package beethoven.Systems

import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import chipsalliance.rocketchip.config._
import beethoven.Floorplanning._
import beethoven._
import beethoven.Generation._
import beethoven.MemoryStreams._
import beethoven.Parameters._
import beethoven.Protocol.RoCC._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tilelink._

import scala.annotation.tailrec


class AcceleratorSystem(val nCores: Int, core_offset: Int)(implicit p: Parameters, val systemParams: AcceleratorSystemConfig, on_deviceID: Int) extends LazyModule {
  val system_id = p(AcceleratorSystems).indexWhere(_.name == systemParams.name)
  val memParams = systemParams.memoryChannelParams
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

    xbar_tree_reduce_sources[TLNode](extended_readers,
      platform.xbarMaxDegree,
      platform.maxMemEndpointsPerCore,
      make_tl_xbar,
      make_tl_buffer,
      tl_assign)
  }

  val writerNodes = (0 until nCores).map { coreIdx =>
    val extended_writers = (writers(coreIdx).map(_._2) :+
      scratch_mod(coreIdx).map(_._2.mem_writer).filter(_.isDefined).map(_.get)).flatMap { a =>
      extend_eles_via_protocol_node[TLNode](a, make_tl_buffer, tl_assign, platform.interCoreMemReductionLatency)
    }
    xbar_tree_reduce_sources[TLNode](extended_writers,
      platform.xbarMaxDegree,
      platform.maxMemEndpointsPerCore,
      make_tl_xbar,
      make_tl_buffer,
      tl_assign)
  }

  val intraCoreMemSlaveNodes = (0 until nCores) map { coreIdx =>
    memParams.filter(_.isInstanceOf[IntraCoreMemoryPortInConfig]).map {
      r =>
        val mp = r.asInstanceOf[IntraCoreMemoryPortInConfig]
        val sp = (0 until mp.nChannels) map { channel_id =>
          LazyModule(new IntraCoreScratchpad(
            dataWidthBits = mp.dataWidthBits,
            nDatas = mp.nDatas,
            latency = mp.latency,
            nPorts = mp.portsPerChannel,
            readOnly = mp.readOnly,
            mp = mp,
            systemParams = systemParams,
            coreIdx = coreIdx,
            channel_id = channel_id))
        }
        val outward_nodes = sp.map { a =>
          val node = TLIdentityNode()
          a.mem_slave_node := node
          node
        }
        (mp, outward_nodes, sp)
    }
  }

  val intraCoreMemMasters = memParams.filter(_.isInstanceOf[IntraCoreMemoryPortOutConfig]).map {
    r =>
      val mp = r.asInstanceOf[IntraCoreMemoryPortOutConfig]
      val (otherSystemParams, otherSPParams) = try {
        val otherS = p(AcceleratorSystems).filter(_.name == mp.toSystem)(0)
        val otherSP = otherS.memoryChannelParams.filter(_.name == mp.toMemoryPort)(0)
        (otherS, otherSP.asInstanceOf[IntraCoreMemoryPortInConfig])
      } catch {
        case a: Exception =>
          System.err.println("Could not properly find system and memory port given by outward memory port params.")
          throw a
      }
      val memMasters = Seq.fill(otherSystemParams.nCores)(TLClientNode(Seq.fill(otherSPParams.nChannels)(TLMasterPortParameters.v1(
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
