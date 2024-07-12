package beethoven.Systems

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import beethoven._
import beethoven.Floorplanning.LazyModuleImpWithSLRs.ModuleWithFloorplan
import beethoven.Generation._
import beethoven.MemoryStreams._
import beethoven.Parameters.ModuleBuilder
import freechips.rocketchip.diplomacy.LazyModuleImp


class AcceleratorSystemImp(val outer: AcceleratorSystem)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val cores = Seq.tabulate(outer.nCores) { core_idx: Int =>
    val impl = Module(outer.systemParams.moduleConstructor match {
      case mb: ModuleBuilder => mb.constructor(p.alterPartial {
        case OuterKey => outer
      })
      //      case bbc: BlackboxBuilderCustom => new AcceleratorBlackBoxCore(bbc)(p, outer.systemParams)
      //      case bbb: BlackboxBuilderRocc => new AcceleratorBlackBoxCore(bbb)(p, outer.systemParams)
    })
    impl.suggestName(f"System${outer.systemParams.name}_core${core_idx}_impl")
    impl
  }


  BeethovenBuild.requestModulePartition(outer.systemParams.name)

  /* handle all memory */
  cores.zipWithIndex.zip(outer.readers) foreach { case ((core, coreIdx), readSet) =>
    readSet.foreach { case (nodeParams, nodes) =>
      val (cParams, clients) = core.read_ios(nodeParams.name)
      (nodes zip clients).zipWithIndex foreach { case ((node, client), channel_idx) =>
        val readerModule = ModuleWithFloorplan(new SequentialReader(client._2.data.bits.getWidth,
          node.out(0)._1, node.out(0)._2,
          cParams.bufferSizeBytesMin),
          s"readerModule_${outer.baseName}_${nodeParams.name}_core${coreIdx}_channel$channel_idx")
        readerModule.io.channel <> client._2
        readerModule.io.req <> client._1
        readerModule.tl_out <> node.out(0)._1
      }
    }
  }
  cores.zipWithIndex.zip(outer.writers) foreach { case ((core, coreIdx), writeSet) =>
    writeSet foreach { case (nodeName, nodes) =>
      val (cParams, clients) = core.write_ios(nodeName.name)
      (nodes zip clients).zipWithIndex foreach { case ((node, client), channel_idx) =>
        val writerModule = ModuleWithFloorplan(new SequentialWriter(
          client._2.dWidth / 8,
          node.out(0)._1, node.out(0)._2,
          cParams.bufferSizeBytesMin),
          s"writerModule_${outer.baseName}_${nodeName.name}_core${coreIdx}_channel${channel_idx}")
        writerModule.io.channel <> client._2
        writerModule.io.req <> client._1
        writerModule.tl_out <> node.out(0)._1
      }

    }
  }
  cores.zip(outer.scratch_mod) foreach { case (core, smods) =>
    smods foreach { case (spad_name, smod) =>
      val (_, spi) = core.sp_ios(spad_name.name)
      spi._1 <> smod.module.req
      spi._2 zip smod.module.IOs foreach { case (a, b) => a <> b }
    }
  }
  cores.zip(outer.intraCoreMemSlaveIOs) foreach { case (core, sps) =>
    sps.flatten.foreach { case (config, spad, _, (core_idx, channel_idx)) =>
      core.intra_mem_ins(config.name)(channel_idx).zip(spad.module.IOs).foreach { case (a, b) => a <> b }
    }
  }

  cores.zip(outer.intraCoreMemMasters) foreach { case (core, (mp, nodeseq)) =>
    core.intra_mem_outs(mp).zip(nodeseq).foreach { case (core_out, node) =>
      core_out <> node.out(0)._1
    }
  }

  cores.zip(outer.system_rocc_endpoints) foreach { case (core, rocc) =>
    core.io_declaration <> rocc.in(0)._1
  }

  cores.zip(outer.rocc_oc) foreach { case (core, rocc_out) =>
    rocc_out foreach { case (target, node) =>
      core.beethoven_rocc_exchanges.find(_._1 == target).get._2 <> node.out(0)._1
    }
  }
}
