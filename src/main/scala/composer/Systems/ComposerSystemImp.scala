package composer.Systems

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._
import composer.ComposerParams._
import composer.Generation.LazyModuleImpWithSLRs
import composer.MemoryStreams.ChannelTransactionBundle
import composer._
import composer.RoccHelpers._
import composer.TLManagement._
import composer.common._

import scala.annotation.tailrec

class ComposerSystemImp(val outer: ComposerSystem)(implicit p: Parameters) extends LazyModuleImpWithSLRs(outer) {
  val sw_io = if (outer.systemParams.canReceiveSoftwareCommands) Some(IO(new ComposerSystemIO())) else None
  val respArbiter = ModuleWithSLR(new MultiLevelArbiter(new ComposerRoccResponse(), outer.systemParams.nCores))
  val cores = outer.cores.map(_._2.module)
  val busy = IO(Output(Bool()))
  busy := cores.map(_.io_declaration.busy).reduce(_ || _)

  val validSrcs = Seq(sw_io, outer.internalCommandManager).filter(_.isDefined)
  // if sources can come from multiple domains (sw, other systems), then we have to remember where cmds came from
  val cmdArbiter = ModuleWithSLR(new RRArbiter(new ComposerRoccCommand(), validSrcs.length))
  val icmAsCmdSrc = if (outer.internalCommandManager.isDefined) {
    val managerNode = outer.internalCommandManager.get
    val (b, e) = managerNode.in(0)
    val manager = ModuleWithSLR(new TLManagerModule(b, e))
    managerNode.in(0)._1 <> manager.tl

    val cmdIO = Wire(Flipped(Decoupled(new ComposerRoccCommand())))
    manager.io.ready := cmdIO.ready
    cmdIO.valid := manager.io.valid
    cmdIO.bits := ComposerRoccCommand(manager.io.bits)

    Some(cmdIO)
  } else None

  val ioAsCmdSrc = if (sw_io.isDefined) {
    val cmdIO = Wire(Flipped(Decoupled(new ComposerRoccCommand())))
    val io_ = sw_io.get
    cmdIO.valid := io_.cmd.valid
    io_.cmd.ready := cmdIO.ready
    cmdIO.bits := io_.cmd.bits
    Some(cmdIO)
  } else None

  val validCmdSrcs = Seq(icmAsCmdSrc, ioAsCmdSrc).filter(_.isDefined).map(_.get)
  validCmdSrcs.zipWithIndex.foreach { case (src, idx) =>
    cmdArbiter.io.in(idx) <> src
  }

  val internalReturnDestinations = if (p(RequireInternalCommandRouting)) Some(VecInit(Seq.fill(outer.nCores)(Reg(new Bundle() {
    val sys = UInt(SystemIDLengthKey.W)
    val core = UInt(CoreIDLengthKey.W)
  })))) else None

  if (p(RequireInternalCommandRouting)) {
    // a core must have only 1 ongoing command that can recieve a response. If the command was sourced from a core, then
    // the response must go to that same core, and if from software then back to software. Mark this bit whenever a
    // command is processed to remember where it came from
    val routingPayload = outer.internalCommandManager.get.in(0)._1.a.bits.data(
      ComposerRoccCommand.packLengthBytes * 8 + SystemIDLengthKey + CoreIDLengthKey - 1,
      ComposerRoccCommand.packLengthBytes * 8)
    val fromCore = routingPayload(CoreIDLengthKey - 1, 0)
    val fromSys = routingPayload(CoreIDLengthKey + SystemIDLengthKey - 1, CoreIDLengthKey)
    val intCmd = icmAsCmdSrc.get

    // arbiter is choosing this one
    when(intCmd.fire && intCmd.bits.inst.xd) {
      internalReturnDestinations.get(intCmd.bits.getCoreID).sys := fromSys
      internalReturnDestinations.get(intCmd.bits.getCoreID).core := fromCore
      // arbiter is consuming input and we are expecting response, so mark as cmd destination
    }
  }

  lazy val cmd = Queue(cmdArbiter.io.out)
  cmd.ready := VecInit(cores.map(_.io_declaration.req.ready))(coreSelect)

  lazy val funct = cmd.bits.inst.funct

  lazy val coreSelect = cmd.bits.getCoreID

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
    resp_queue.io.enq.bits.data_field := c.io_declaration.resp.bits.data_field
    resp_queue.io.enq.valid := c.io_declaration.resp.valid
    c.io_declaration.resp.ready := resp_queue.io.enq.ready
    resp_queue.io.deq
  }

  respArbiter.io.in <> coreResps
  val resp = Wire(Decoupled(new ComposerRoccResponse()))
  resp.valid := respArbiter.io.out.valid
  resp.bits.rd := respArbiter.io.out.bits.rd
  resp.bits.core_id := respArbiter.io.chosen // .io.out.bits.core_id
  resp.bits.data_field := respArbiter.io.out.bits.data_field
  resp.bits.system_id := outer.system_id.U
  respArbiter.io.out.ready := resp.ready

  val respQ = Queue(resp)

  val internalRespDispatchModule = if (p(RequireInternalCommandRouting)) {
    val wire = Wire(new ComposerRoccResponse())
    wire.data_field := respQ.bits.data_field
    wire.rd := respQ.bits.rd
    wire.system_id := internalReturnDestinations.get(respQ.bits.core_id).sys
    wire.core_id := internalReturnDestinations.get(respQ.bits.core_id).core

    val respClient = outer.outgoingInternalResponseClient.get
    val internalRespDispatcher = ModuleWithSLR(new TLClientModule(respClient))
    internalRespDispatcher.tl <> respClient.out(0)._1
    internalRespDispatcher.io.bits.dat := wire.pack()
    internalRespDispatcher.io.bits.addr := ComposerConsts.getInternalCmdRoutingAddress(wire.system_id)
    Some(internalRespDispatcher)
  } else None


  if (outer.systemParams.canReceiveSoftwareCommands && p(RequireInternalCommandRouting)) {
    val swio = sw_io.get

    val readyDisp = Wire(Bool())
    respQ.ready := readyDisp

    val reqCmdSourcedInternally = Reg(Vec(outer.nCores, Bool()))

    when(icmAsCmdSrc.get.fire && icmAsCmdSrc.get.bits.inst.xd) {
      reqCmdSourcedInternally(icmAsCmdSrc.get.bits.getCoreID) := true.B
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
  } else if (p(RequireInternalCommandRouting)) {
    respQ.ready := internalRespDispatchModule.get.io.ready
    internalRespDispatchModule.get.io.valid := respQ.valid
  } else {
    throw new Exception("System unreachable!")
  }

  if (outer.systemParams.canIssueCoreCommands) {
    val managerNode = outer.incomingInternalResponseManager.get
    val (mBundle, mEdge) = managerNode.in(0)
    val responseManager = ModuleWithSLR(new TLManagerModule(mBundle, mEdge))
    responseManager.tl <> outer.incomingInternalResponseManager.get.in(0)._1
    val response = ComposerRoccResponse(responseManager.io.bits)

    cores.zipWithIndex.foreach { case (core, core_idx) =>
      core.composer_response_io_.get.valid := responseManager.io.valid && response.core_id === core_idx.U
      core.composer_response_io_.get.bits := response
    }

    responseManager.io.ready := VecInit(cores.map(_.composer_response_io_.get.ready))(response.core_id)
  }

  cores.foreach { core =>
    if (p(CoreCommandLatency) > 0) {
      val coreCmdQueue = Module(new Queue[ComposerRoccCommand](new ComposerRoccCommand, 2))
      coreCmdQueue.io.enq <> cmd
      coreCmdQueue.io.deq <> core.io_declaration.req
    } else {
      core.io_declaration.req.valid := cmd.valid
      core.io_declaration.req.bits := cmd.bits
      cmd.ready := core.io_declaration.req.ready
    }
  }
  tieClocks()
}