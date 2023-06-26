package composer.Systems

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.ComposerParams._
import composer.Generation.LazyModuleImpWithSLRs
import composer.RoccHelpers._
import composer.TLManagement._
import composer._
import composer.common._

import scala.annotation.tailrec

class CommandSrcPair(nSources: Int)(implicit p: Parameters) extends Bundle {
  val cmd = new ComposerRoccCommand()
  val src = UInt(log2Up(nSources).W)
}

object CommandSrcPair {
  def apply(c: ComposerRoccCommand, s: UInt)(implicit p: Parameters): CommandSrcPair = {
    val res = Wire(new CommandSrcPair(1))
    res.cmd := c
    res.src := s
    res
  }
}

class ComposerSystemImp(val outer: ComposerSystem)(implicit p: Parameters) extends LazyModuleImpWithSLRs(outer) {
  val sw_io = if (outer.systemParams.canReceiveSoftwareCommands) Some(IO(new ComposerSystemIO())) else None
  val cores = outer.cores.map(_._2.module)

  val validSrcs = Seq(sw_io, outer.internalCommandManager).filter(_.isDefined)
  // if sources can come from multiple domains (sw,managerNode.portParams(0).endSinkId + 1 other systems), then we have to remember where cmds came from
  val cmdArbiter = ModuleWithSLR(new RRArbiter(new CommandSrcPair(outer.acc.sysNCmdSourceLookup(outer.systemParams.name)), validSrcs.length),
    SLRConstants.getFrontBusSLR)
  val internalCmdSource = if (outer.internalCommandManager.isDefined) {
    val managerNode = outer.internalCommandManager.get
    val (b, e) = managerNode.in(0)
    val manager = ModuleWithSLR(new TLManagerModule(b, e), SLRConstants.getFrontBusSLR)
    managerNode.in(0)._1 <> manager.tl
    val cmdIO = manager.io.map(r => hasAccessibleUserSubRegions[ComposerRoccCommand](r, new ComposerRoccCommand))
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

  val addressBits = outer.memory_nodes map { m =>
    m.out(0)._1.params.addressBits
  } match {
    case Seq() => 0
    case l: Seq[Int] => l.max
    case _ => 0
  }

  val resp = {
    @tailrec
    def collapseResp(degree: Int, resps: Seq[DecoupledIO[ComposerRoccResponse]], slr_id: Int): DecoupledIO[ComposerRoccResponse] = {
      if (resps.length == 1) {
        resps(0)
      } else {
        val subgroups = resps.grouped(degree)
        val subGroupsArb = subgroups map { sg =>
          val respArb = ModuleWithSLR(new RRArbiter(new ComposerRoccResponse(), sg.length), slr_id)
          val respQ = ModuleWithSLR(new Queue(new ComposerRoccResponse(), entries = 2), slr_id)
          sg.zipWithIndex.foreach { case (core_resp, idx) =>
            respArb.io.in(idx) <> core_resp
          }
          respArb.io.out <> respQ.io.enq
          respQ.io.deq
        }
        collapseResp(degree, subGroupsArb.toSeq, slr_id)
      }
    }

    val coreGroups = outer.cores.groupBy(_._1)
    if (coreGroups.keys.size == 1) {
      val coreResps = cores.map { c =>
        val lastRecievedRd = Reg(UInt(5.W))
        when(c.io_declaration.req.fire) {
          lastRecievedRd := c.io_declaration.req.bits.inst.rd
        }
        val resp_queue = ModuleWithSLR(new Queue[ComposerRoccResponse](new ComposerRoccResponse(), entries = 2), SLRConstants.getFrontBusSLR)
        resp_queue.io.enq.bits.system_id := outer.system_id.U
        resp_queue.io.enq.bits.core_id := c.composerConstructor.composerCoreWrapper.core_id.U
        resp_queue.io.enq.bits.rd := lastRecievedRd
        resp_queue.io.enq.bits.getDataField := c.io_declaration.resp.bits.getDataField
        resp_queue.io.enq.valid := c.io_declaration.resp.valid
        c.io_declaration.resp.ready := resp_queue.io.enq.ready
        resp_queue.io.deq
      }
      collapseResp(SLRConstants.SLRRoutingFanout, coreResps, SLRConstants.getFrontBusSLR)
    } else {
      val respsCollapsed = coreGroups.map { case (slr_id, core_list) =>
        collapseResp(SLRConstants.SLRRoutingFanout, core_list.map(_._2).map { c =>
          c.module.io_declaration.resp.map { ioresp =>
            val composerRespWire = Wire(new ComposerRoccResponse())
            composerRespWire.getDataField := ioresp.getDataField
            composerRespWire.rd := ioresp.rd
            composerRespWire.system_id := outer.system_id.U
            composerRespWire.core_id := c.core_id.U
            composerRespWire
          }
        }, slr_id)
      }
      val topLevelResp = collapseResp(1000, respsCollapsed.toSeq, SLRConstants.getFrontBusSLR)
      topLevelResp
    }
  }


  val respQ = Queue(resp)

  val internalRespDispatchModule = if (outer.canBeIntaCoreCommandEndpoint) {
    val internalReturnDestinations = Reg(Vec(outer.nCores, new Bundle() {
      val sys = UInt(SystemIDLengthKey.W)
      val core = UInt(CoreIDLengthKey.W)
    }))

    val a_in = outer.internalCommandManager.get.in(0)._1.a.bits.data
    val routingPayload = a_in(a_in.getWidth - 1, (new ComposerRoccCommand).getWidth)
    val fromCore = routingPayload(CoreIDLengthKey - 1, 0)
    val fromSys = routingPayload(CoreIDLengthKey + SystemIDLengthKey - 1, CoreIDLengthKey)
    val intCmd = internalCmdSource.get._1

    // arbiter is choosing this one
    when(intCmd.fire && intCmd.bits.inst.xd) {
      internalReturnDestinations(intCmd.bits.getCoreID).sys := fromSys
      internalReturnDestinations(intCmd.bits.getCoreID).core := fromCore
      // arbiter is consuming input and we are expecting response, so mark as cmd destination
    }

    val wire = Wire(new ComposerRoccResponse())
    wire.getDataField := respQ.bits.getDataField
    //    wire.rd := respQ.bits.rd
    wire.system_id := internalReturnDestinations(respQ.bits.core_id).sys
    wire.core_id := internalReturnDestinations(respQ.bits.core_id).core
    wire.rd := 0.U

    val respClient = outer.outgoingInternalResponseClient.get
    val internalRespDispatcher = ModuleWithSLR(new TLClientModule(respClient))
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
    val responseManager = ModuleWithSLR(new TLManagerModule(mBundle, mEdge))
    responseManager.tl <> managerNode.in(0)._1
    val response = hasRoccResponseFields[ComposerRoccResponse](new ComposerRoccResponse, responseManager.io.bits)

    cores.zipWithIndex.foreach { case (core, core_idx) =>
      core.composer_response_ios_(target).valid := responseManager.io.valid && response.core_id === core_idx.U
      core.composer_response_ios_(target).bits := response
    }

    responseManager.io.ready := VecInit(cores.map(_.composer_response_ios_(target).ready))(response.core_id)
  }

  if (p(CoreCommandLatency) > 0) {

    // take in series of endpoints that recieve a commandSrc pair and return a smaller sequence (by degree radix) of commandSrc pairs
    def connectCoreGroup(degree: Int, endpoints: Seq[(DecoupledIO[CommandSrcPair], Seq[Int])], slrId: Int):
    Seq[(DecoupledIO[CommandSrcPair], Seq[Int])] = {
      val core_group = endpoints.grouped(degree)
      core_group.map { cg =>
        val core_group_cmd_queue = ModuleWithSLR(new Queue(new CommandSrcPair(outer.acc.sysNCmdSourceLookup(outer.systemParams.name)), 2), slrId)
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

    val slr_groups = outer.cores.groupBy(_._1)

    @tailrec
    def recursivelyGroupCmds(degree: Int, endpoints: Seq[(DecoupledIO[CommandSrcPair], Seq[Int])], slrId: Int): (DecoupledIO[CommandSrcPair], Seq[Int]) = {
      if (endpoints.size == 1) {
        endpoints(0)
      } else {
        recursivelyGroupCmds(degree, connectCoreGroup(degree, endpoints, slrId), slrId)
      }
    }

    val slr_endpoints = slr_groups.map { case (slr_id, cwrap) =>
      val slr_cores = cwrap.map(_._2)
      val core_cmd_endpoints = slr_cores.map { core =>
        val cmdsrcpr = Wire(Output(Decoupled(new CommandSrcPair(outer.acc.sysNCmdSourceLookup(outer.systemParams.name)))))
        core.module.io_declaration.req.bits := cmdsrcpr.bits.cmd
        core.module.io_source := cmdsrcpr.bits.src
        core.module.io_declaration.req.valid := cmdsrcpr.valid
        cmdsrcpr.ready := core.module.io_declaration.req.ready
        connectCoreGroup(1, Seq((cmdsrcpr, Seq(core.core_id))), slr_id)
      }
      (slr_id, recursivelyGroupCmds(SLRConstants.SLRRoutingFanout, core_cmd_endpoints.map(pr => (pr(0)._1, pr(0)._2)), slr_id))
    }
    val origin = recursivelyGroupCmds(1000, slr_endpoints.values.toSeq, SLRConstants.getFrontBusSLR)._1

    cmd <> origin
  } else {
    cores.map { core =>
      core.io_declaration.req.valid := cmd.valid && coreSelect === core.getCoreID.U
      core.io_declaration.req.bits := cmd.bits.cmd
      core.io_source := cmd.bits.src
      core.io_declaration.req.ready
    }
    cmd.ready := VecInit(cores.map(_.io_declaration.req.ready))(coreSelect)
  }


  tieClocks()
}
