package composer.Systems

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer._
import composer.ComposerParams._
import composer.Floorplanning.{LazyFloorplan, LazyModuleImpWithSLRs}
import composer.Generation.{BuildMode, ComposerBuild}
import composer.MemoryStreams._
import composer.Platforms.{BuildModeKey, DieName, MultiDiePlatform, PlatformType}
import composer.RoccHelpers._
import composer.TLManagement._
import composer.common._

import scala.annotation.tailrec

class CommandSrcPair(nSources: Int) extends Bundle {
  val cmd = new AccelRoccCommand()
  val src = UInt(log2Up(nSources).W)
}

object CommandSrcPair {
  def apply(c: AccelRoccCommand, s: UInt): CommandSrcPair = {
    val res = Wire(new CommandSrcPair(1))
    res.cmd := c
    res.src := s
    res
  }
}

class ComposerSystemImp(val outer: ComposerSystem)(implicit p: Parameters) extends LazyModuleImpWithSLRs(outer) {
  val sw_io = if (outer.systemParams.canReceiveSoftwareCommands) Some(IO(new ComposerSystemIO())) else None

  val validSrcs = Seq(sw_io, outer.internalCommandManager).filter(_.isDefined)
  // if sources can come from multiple domains (sw,managerNode.portParams(0).endSinkId + 1 other systems), then we have to remember where cmds came from
  val cmdArbiter = ModuleWithSLR(new RRArbiter(new CommandSrcPair(outer.acc.sysNCmdSourceLookup(outer.systemParams.name)), validSrcs.length),
    DieName.getFrontBusSLR)
  val internalCmdSource = if (outer.internalCommandManager.isDefined) {
    val managerNode = outer.internalCommandManager.get
    val (b, e) = managerNode.in(0)
    val manager = ModuleWithSLR(new TLManagerModule(b, e), DieName.getFrontBusSLR)
    managerNode.in(0)._1 <> manager.tl
    val cmdIO = manager.io.map(r => hasAccessibleUserSubRegions[AccelRoccCommand](r, new AccelRoccCommand))
    // 0 is always reserved for software (even if it doesn't exist)
    Some((cmdIO, b.a.bits.source +& 1.U))
  } else None

  val swCmdSource = if (sw_io.isDefined) {
    Some((sw_io.get.cmd, 0.U))
  } else None

  val validCmdSrcs = Seq(internalCmdSource, swCmdSource).filter(_.isDefined).map(_.get)
  validCmdSrcs.zipWithIndex.foreach { case (src, idx) =>
    cmdArbiter.io.in(idx).bits.cmd := src._1.bits
    cmdArbiter.io.in(idx).bits.src := src._2
    cmdArbiter.io.in(idx).valid := src._1.valid
    src._1.ready := cmdArbiter.io.in(idx).ready
  }

  lazy val cmd = Queue(cmdArbiter.io.out)

  lazy val funct = cmd.bits.cmd.inst.funct

  lazy val coreSelect = cmd.bits.cmd.getCoreID

  val addressBits = CLog2Up(platform.extMem.nMemoryChannels * platform.extMem.master.size)

  val cores = Seq.tabulate(outer.nCores) { core_idx: Int =>
    val impl = ModuleWithSLR(outer.systemParams.moduleConstructor match {
      case mb: ModuleBuilder =>
        if (core_idx == 0) {
          //          LazyFloorplan.registerSystem(outer.nCores, mb.constructor(outer, p), Seq())
        }
        mb.constructor(outer, p)
      case bbc: BlackboxBuilderCustom => new AcceleratorBlackBoxCore(outer, bbc)
      case bbb: BlackboxBuilderRocc => new AcceleratorBlackBoxCore(outer, bbb)
    }, core2slr(core_idx))
    if (platform.platformType == PlatformType.FPGA && p(BuildModeKey) != BuildMode.Simulation) {
      impl.reset := ShiftReg(reset, 5)
    }
    impl
  }


  ComposerBuild.requestModulePartition(outer.systemParams.name)
  val coreGroups = cores.zipWithIndex.map(t => t.copy(_2 = core2slr(t._2))).groupBy(_._2)

  var gl_incrementer = 0
  val resp = {
    @tailrec
    def collapseResp(degree: Int, to_size: Int, resps: Seq[DecoupledIO[AccelRoccResponse]], slr_id: Int): Seq[DecoupledIO[AccelRoccResponse]] = {
      if (resps.length <= to_size) {
        val respQueues = Seq.tabulate(resps.length) { ridx =>
          val q = ModuleWithSLR(new Queue(new AccelRoccResponse, 2),
            slr_id,
            f"respQ_${outer.system_id}_${slr_id}_$gl_incrementer")
          gl_incrementer = gl_incrementer + 1
          q.io.enq <> resps(ridx)
          q.io.deq
        }
        respQueues
      } else {
        val subgroups = resps.grouped(degree).toSeq
        val subGroupsArb = subgroups map { sg =>
          val respArb = ModuleWithSLR(
            new RRArbiter(new AccelRoccResponse(), sg.length),
            slr_id,
            f"respArb_${outer.system_id}_${slr_id}_$gl_incrementer")
          val respQ = Module(new Queue(new AccelRoccResponse(), entries = 2))
          gl_incrementer = gl_incrementer + 1
          sg.zipWithIndex.foreach { case (core_resp, idx) =>
            respArb.io.in(idx) <> core_resp
          }
          respArb.io.out <> respQ.io.enq
          respQ.io.deq
        }
        collapseResp(degree, to_size, subGroupsArb, slr_id)
      }
    }

    if (coreGroups.keys.size == 1) {
      val coreResps = cores.zipWithIndex.map { case (c: AcceleratorCore, cidx: Int) =>
        val lastRecievedRd = Reg(UInt(5.W))
        when(c.io_declaration.req.fire) {
          lastRecievedRd := c.io_declaration.req.bits.inst.rd
        }
        val resp_queue = ModuleWithSLR(new Queue[AccelRoccResponse](
          new AccelRoccResponse(), entries = 2), DieName.getFrontBusSLR)
        resp_queue.io.enq.bits.system_id := outer.system_id.U
        resp_queue.io.enq.bits.core_id := cidx.U
        resp_queue.io.enq.bits.rd := lastRecievedRd
        resp_queue.io.enq.bits.getDataField := c.io_declaration.resp.bits.getDataField
        resp_queue.io.enq.valid := c.io_declaration.resp.valid
        c.io_declaration.resp.ready := resp_queue.io.enq.ready
        resp_queue.io.deq
      }
      collapseResp(DieName.SLRRespRoutingFanout, 1, coreResps, DieName.getFrontBusSLR)(0)
    } else {
      val respsCollapsed = coreGroups.map { case (slr_id, core_list) =>
        (slr_id, collapseResp(DieName.SLRRespRoutingFanout, DieName.RespEndpointsPerSLR, core_list.map {
          case (c: AcceleratorCore, coreIdx: Int) =>
            println(s"ComposerSystemImp: coreIdx: $coreIdx is in SLR $slr_id")
            c.io_declaration.resp.map { ioresp =>
              val composerRespWire = Wire(new AccelRoccResponse())
              composerRespWire.getDataField := ioresp.getDataField
              composerRespWire.rd := ioresp.rd
              composerRespWire.system_id := outer.system_id.U
              composerRespWire.core_id := coreIdx.U
              composerRespWire
            }
        }, slr_id))
      }
      DieName.getCmdRespPath() match {
        case None =>
          val topLevelResp = collapseResp(DieName.SLRRespRoutingFanout, 1, respsCollapsed.flatMap(_._2).toSeq, DieName.getFrontBusSLR)
          topLevelResp(0)
        case Some(path) =>
          collapseResp(DieName.SLRRespRoutingFanout, 1,
            path.foldRight(Seq[DecoupledIO[AccelRoccResponse]]()) { case (slr_idx, acc) =>
              collapseResp(DieName.SLRRespRoutingFanout, DieName.RespEndpointsPerSLR,
                acc ++ respsCollapsed(slr_idx),
                slr_idx)
            }, DieName.getFrontBusSLR)(0)
      }
    }
  }

  val respQ = Queue(resp)

  val internalRespDispatchModule = if (outer.canBeIntaCoreCommandEndpoint) {
    val internalReturnDestinations = Reg(Vec(outer.nCores, new Bundle() {
      val sys = UInt(SystemIDLengthKey.W)
      val core = UInt(CoreIDLengthKey.W)
    }))

    val a_in = outer.internalCommandManager.get.in(0)._1.a.bits.data
    val routingPayload = a_in(a_in.getWidth - 1, (new AccelRoccCommand).getWidth)
    val fromCore = routingPayload(CoreIDLengthKey - 1, 0)
    val fromSys = routingPayload(CoreIDLengthKey + SystemIDLengthKey - 1, CoreIDLengthKey)
    val intCmd = internalCmdSource.get._1

    // arbiter is choosing this one
    when(intCmd.fire && intCmd.bits.inst.xd) {
      internalReturnDestinations(intCmd.bits.getCoreID).sys := fromSys
      internalReturnDestinations(intCmd.bits.getCoreID).core := fromCore
      // arbiter is consuming input and we are expecting response, so mark as cmd destination
    }

    val wire = Wire(new AccelRoccResponse())
    wire.getDataField := respQ.bits.getDataField
    //    wire.rd := respQ.bits.rd
    wire.system_id := internalReturnDestinations(respQ.bits.core_id).sys
    wire.core_id := internalReturnDestinations(respQ.bits.core_id).core
    wire.rd := 0.U

    val respClient = outer.outgoingInternalResponseClient.get
    val internalRespDispatcher = Module(new TLClientModule(respClient))
    internalRespDispatcher.tl <> respClient.out(0)._1
    // Don't need RD internally so don't send it. Adding it would force use to use a _much_ wider interconnect...
    internalRespDispatcher.io.bits.dat := wire.packRocc
    internalRespDispatcher.io.bits.addr := ComposerConsts.getInternalCmdRoutingAddress(wire.system_id)
    Some(internalRespDispatcher)
  } else None


  if (outer.systemParams.canReceiveSoftwareCommands && outer.canBeIntaCoreCommandEndpoint) {
    val swio = sw_io.get

    val readyDisp = Wire(Bool())
    respQ.ready := readyDisp

    val reqCmdSourcedInternally = Reg(Vec(outer.nCores, Bool()))
    val internalCmd = internalCmdSource.get._1
    when(internalCmd.fire && internalCmd.bits.inst.xd) {
      reqCmdSourcedInternally(internalCmd.bits.getCoreID) := true.B
    }
    when(swio.cmd.fire && swio.cmd.bits.inst.xd) {
      reqCmdSourcedInternally(swio.cmd.bits.getCoreID) := false.B
    }


    swio.resp.valid := false.B
    internalRespDispatchModule.get.io.valid := false.B

    swio.resp.bits <> respQ.bits
    // internal resp module bits already set

    when(reqCmdSourcedInternally(respQ.bits.core_id)) {
      // command was sourced internally
      readyDisp := internalRespDispatchModule.get.io.ready
      internalRespDispatchModule.get.io.valid := respQ.valid
    }.otherwise {
      readyDisp := sw_io.get.resp.ready
      swio.resp.valid := respQ.valid
    }
  } else if (outer.systemParams.canReceiveSoftwareCommands) {
    // simple case!
    sw_io.get.resp <> respQ
  } else if (outer.canBeIntaCoreCommandEndpoint) {
    respQ.ready := internalRespDispatchModule.get.io.ready
    internalRespDispatchModule.get.io.valid := respQ.valid
  } else {
    throw new Exception("System unreachable!")
  }

  outer.systemParams.canIssueCoreCommandsTo.foreach { target =>
    val manager = outer.incomingInternalResponseHandlers(target)
    val managerNode = manager._1
    val (mBundle, mEdge) = managerNode.in(0)
    val responseManager = Module(new TLManagerModule(mBundle, mEdge))
    responseManager.tl <> managerNode.in(0)._1
    val response = hasRoccResponseFields[AccelRoccResponse](new AccelRoccResponse, responseManager.io.bits)


    cores.zipWithIndex.foreach { case (core, core_idx) =>
      core.composer_response_ios_(target).valid := responseManager.io.valid && response.core_id === core_idx.U
      core.composer_response_ios_(target).bits := response
    }

    responseManager.io.ready := VecInit(cores.map(_.composer_response_ios_(target).ready))(response.core_id)
  }

  if (platform.coreCommandLatency > 0) {

    // take in series of endpoints that recieve a commandSrc pair and return a smaller sequence (by degree radix) of commandSrc pairs
    def connectCoreGroup(degree: Int, endpoints: Seq[(DecoupledIO[CommandSrcPair], Seq[Int])], slr_id: Option[Int]):
    Seq[(DecoupledIO[CommandSrcPair], Seq[Int])] = {
      val core_group = endpoints.grouped(degree)
      core_group.map { cg =>
        val core_group_cmd_queue = slr_id match {
          case Some(slrId) => ModuleWithSLR(new Queue(new CommandSrcPair(outer.acc.sysNCmdSourceLookup(outer.systemParams.name)), 2), slrId)
          case None => Module(new Queue(new CommandSrcPair(outer.acc.sysNCmdSourceLookup(outer.systemParams.name)), 2))
        }
        core_group_cmd_queue.io.deq.ready := false.B
        val local_cselect = core_group_cmd_queue.io.deq.bits.cmd.getCoreID
        cg.foreach { case (core_q, core_id_set) =>
          val choose_queue = core_id_set.map(cid => cid.U === local_cselect).reduce(_ || _)
          core_q.valid := core_group_cmd_queue.io.deq.valid && choose_queue
          core_q.bits := core_group_cmd_queue.io.deq.bits
          when(choose_queue) {
            core_group_cmd_queue.io.deq.ready := core_q.ready
          }
        }
        (core_group_cmd_queue.io.enq, cg.flatMap(_._2))
      }.toSeq
    }

    @tailrec
    def recursivelyGroupCmds(degree: Int, to_size: Int, endpoints: Seq[(DecoupledIO[CommandSrcPair], Seq[Int])], slrId: Option[Int]): Seq[(DecoupledIO[CommandSrcPair], Seq[Int])] = {
      if (endpoints.size <= to_size) {
        endpoints
      } else {
        recursivelyGroupCmds(degree, to_size, connectCoreGroup(degree, endpoints, slrId), slrId)
      }
    }

    val slr_endpoints = (platform match {
      case multiDiePlatform: MultiDiePlatform => multiDiePlatform.platformDies.indices
      case _ => Seq(0)
    }) map { slr_id =>
      val slr_cores = cores.zipWithIndex.filter(pr => core2slr(pr._2) == slr_id)
      val core_cmd_endpoints = slr_cores.map { case (core, core_idx) =>
        val cmdsrcpr = Wire(Output(Decoupled(new CommandSrcPair(outer.acc.sysNCmdSourceLookup(outer.systemParams.name)))))
        core.io_declaration.req.bits := cmdsrcpr.bits.cmd
        core.io_source := cmdsrcpr.bits.src
        core.io_declaration.req.valid := cmdsrcpr.valid
        cmdsrcpr.ready := core.io_declaration.req.ready
        // queue in front of every core (no-fanout)
        connectCoreGroup(1, Seq((cmdsrcpr, Seq(core_idx))), Some(slr_id))
      }
      (slr_id, recursivelyGroupCmds(DieName.SLRCmdRoutingFanout, DieName.CmdEndpointsPerSLR, core_cmd_endpoints.map(pr => (pr(0)._1, pr(0)._2)), Some(slr_id)))
    }
    val origin = DieName.getCmdRespPath() match {
      case None => recursivelyGroupCmds(DieName.SLRCmdRoutingFanout, 1, slr_endpoints.flatMap(_._2), None)(0)._1
      case Some(path) =>
        recursivelyGroupCmds(DieName.SLRRespRoutingFanout, 1,
          path.foldRight(Seq[(DecoupledIO[CommandSrcPair], Seq[Int])]()) { case (slr_id, acc) =>
            recursivelyGroupCmds(DieName.SLRCmdRoutingFanout, DieName.CmdEndpointsPerSLR,
              connectCoreGroup(1, acc, None) ++
                (slr_endpoints.find(_._1 == slr_id) match {
                  case None => Seq()
                  case Some(q) => q._2
                }),
              Some(slr_id))

          }, Some(DieName.getFrontBusSLR))(0)._1
    }
    cmd <> origin
  } else {
    cores.zipWithIndex.map { case (core, coreIdx) =>
      core.io_declaration.req.valid := cmd.valid && coreSelect === coreIdx.U
      core.io_declaration.req.bits := cmd.bits.cmd
      core.io_source := cmd.bits.src
      core.io_declaration.req.ready
    }
    cmd.ready := VecInit(cores.map(_.io_declaration.req.ready))(coreSelect)
  }

  /* handle all memory */
  cores.zipWithIndex.zip(outer.readers) foreach { case ((core, coreIdx), readSet) =>
    readSet.foreach { case (nodeParams, nodes) =>
      val (cParams, clients) = core.read_ios(nodeParams.name)
      (nodes zip clients).zipWithIndex foreach { case ((node, client), channel_idx) =>
        val readerModule = ModuleWithSLR(new SequentialReader(client._2.data.bits.getWidth,
          node.out(0)._1, node.out(0)._2,
          cParams.bufferSizeBytesMin),
          core2slr(coreIdx),
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
        val writerModule = ModuleWithSLR(new SequentialWriter(
          client._2.dWidth / 8,
          node.out(0)._1, node.out(0)._2,
          cParams.bufferSizeBytesMin), core2slr(coreIdx),
          s"writerModule_${outer.baseName}_${nodeName.name}_core${coreIdx}_channel${channel_idx}")
        writerModule.io.channel <> client._2
        writerModule.io.req <> client._1
        writerModule.tl_out <> node.out(0)._1
      }

    }
  }
  cores.zip(outer.scratch_mod) foreach { case (core, smods) =>
    smods foreach { case (spad_name, smod) =>
      val (sparams, spi) = core.sp_ios(spad_name.name)
      spi._1 <> smod.module.req
      spi._2 zip smod.module.IOs foreach { case (a, b) => a <> b }
    }
  }
  tieClocks()
}
