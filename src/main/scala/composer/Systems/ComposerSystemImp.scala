package composer.Systems

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._
import composer.ComposerParams._
import composer.Generation.LazyModuleImpWithSLRs
import composer.MemoryStreams.ChannelTransactionBundle
import composer.{common, _}
import composer.RoccHelpers._
import composer.TLManagement._
import composer.common._

import scala.annotation.tailrec

class CommandSrcPair(nSources: Int)(implicit p: Parameters) extends Bundle {
  val cmd = new ComposerRoccCommand()
  val src = UInt(log2Up(nSources).W)
}

class ComposerSystemImp(val outer: ComposerSystem)(implicit p: Parameters) extends LazyModuleImpWithSLRs(outer) {
  val sw_io = if (outer.systemParams.canReceiveSoftwareCommands) Some(IO(new ComposerSystemIO())) else None
  val respArbiter = ModuleWithSLR(new MultiLevelArbiter(new ComposerRoccResponse(), outer.systemParams.nCores))
  val cores = outer.cores.map(_._2.module)

  val validSrcs = Seq(sw_io, outer.internalCommandManager).filter(_.isDefined)
  // if sources can come from multiple domains (sw,managerNode.portParams(0).endSinkId + 1 other systems), then we have to remember where cmds came from
  val cmdArbiter = ModuleWithSLR(new RRArbiter(new CommandSrcPair(outer.acc.sysNCmdSourceLookup(outer.systemParams.name)), validSrcs.length))
  val internalCmdSource = if (outer.internalCommandManager.isDefined) {
    val managerNode = outer.internalCommandManager.get
    val (b, e) = managerNode.in(0)
    val manager = ModuleWithSLR(new TLManagerModule(b, e))
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

  val coreResps = cores.map { c =>
    val lastRecievedRd = Reg(UInt(5.W))
    when(c.io_declaration.req.fire) {
      lastRecievedRd := c.io_declaration.req.bits.inst.rd
    }
    val resp_queue = Module(new Queue[ComposerRoccResponse](new ComposerRoccResponse(), entries = 2))
    resp_queue.io.enq.bits.system_id := outer.system_id.U
    resp_queue.io.enq.bits.core_id := c.composerConstructor.composerCoreWrapper.core_id.U
    resp_queue.io.enq.bits.rd := lastRecievedRd
    resp_queue.io.enq.bits.getDataField := c.io_declaration.resp.bits.getDataField
    resp_queue.io.enq.valid := c.io_declaration.resp.valid
    c.io_declaration.resp.ready := resp_queue.io.enq.ready
    resp_queue.io.deq
  }

  respArbiter.io.in <> coreResps
  val resp = Wire(Decoupled(new ComposerRoccResponse()))
  resp.valid := respArbiter.io.out.valid
  resp.bits.rd := respArbiter.io.out.bits.rd
  resp.bits.core_id := respArbiter.io.chosen // .io.out.bits.core_id
  resp.bits.getDataField := respArbiter.io.out.bits.getDataField
  resp.bits.system_id := outer.system_id.U
  respArbiter.io.out.ready := resp.ready

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
  val core_readys = cores.map { core =>
    if (p(CoreCommandLatency) > 0) {
      val coreCmdQueue = Module(new Queue(new CommandSrcPair(outer.acc.sysNCmdSourceLookup(outer.systemParams.name)), 2))
      coreCmdQueue.io.enq.bits := cmd.bits
      coreCmdQueue.io.enq.valid := cmd.valid && coreSelect === core.getCoreID.U
      coreCmdQueue.io.deq.map(_.cmd) <> core.io_declaration.req
      core.io_source := coreCmdQueue.io.deq.bits.src
      coreCmdQueue.io.enq.ready
    } else {
      core.io_declaration.req.valid := cmd.valid && coreSelect === core.getCoreID.U
      core.io_declaration.req.bits := cmd.bits.cmd
      core.io_source := cmd.bits.src
      core.io_declaration.req.ready
    }
  }

  cmd.ready := VecInit(core_readys)(coreSelect)

  tieClocks()
}
