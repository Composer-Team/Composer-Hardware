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
  val respArbiter = ModuleWithSLR(new MultiLevelArbiter(new ComposerRoccUserResponse(), outer.systemParams.nCores))
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
      internalReturnDestinations.get(intCmd.bits.core_id).sys := fromSys
      internalReturnDestinations.get(intCmd.bits.core_id).core := fromCore
      // arbiter is consuming input and we are expecting response, so mark as cmd destination
    }
  }

  lazy val cmd = Queue(cmdArbiter.io.out)
  cmd.ready := funct =/= ComposerFunc.START.U || VecInit(cores.map(_.io_declaration.req.ready))(coreSelect)

  lazy val funct = cmd.bits.inst.funct

  lazy val coreSelect = cmd.bits.core_id

  val addressBits = outer.memory_nodes map { m =>
    m.out(0)._1.params.addressBits
  } match {
    case Seq() => 0
    case l: Seq[Int] => l.max
    case _ => 0
  }

  val coreResps = if (p(CoreCommandLatency) == 0) cores.map(_.io_declaration.resp) else {
    cores.map{ c =>
      val resp_queue = Module(new Queue[ComposerRoccUserResponse](new ComposerRoccUserResponse(), 2))
      resp_queue.io.enq <> c.io_declaration.resp
      resp_queue.io.deq
    }
  }

  respArbiter.io.in <> coreResps
  val resp = Wire(Decoupled(new ComposerRoccResponse()))
  resp.valid := respArbiter.io.out.valid
  resp.bits.rd := respArbiter.io.out.bits.rd
  resp.bits.core_id := respArbiter.io.chosen // .io.out.bits.core_id
  resp.bits.data := respArbiter.io.out.bits.data
  resp.bits.system_id := outer.system_id.U
  respArbiter.io.out.ready := resp.ready

  val respQ = Queue(resp)

  val internalRespDispatchModule = if (p(RequireInternalCommandRouting)) {
    val wire = Wire(new ComposerRoccResponse())
    wire.data := respQ.bits.data
    wire.rd := respQ.bits.rd
    wire.system_id := internalReturnDestinations.get(respQ.bits.core_id).sys
    wire.core_id := internalReturnDestinations.get(respQ.bits.core_id).core

    val respClient = outer.outgoingInternalResponseClient.get
    val internalRespDispatcher = ModuleWithSLR(new TLClientModule(respClient))
    internalRespDispatcher.tl <> respClient.out(0)._1
    internalRespDispatcher.io.bits.dat := wire.pack
    internalRespDispatcher.io.bits.addr := ComposerConsts.getInternalCmdRoutingAddress(wire.system_id)
    Some(internalRespDispatcher)
  } else None


  if (outer.systemParams.canReceiveSoftwareCommands && p(RequireInternalCommandRouting)) {
    val swio = sw_io.get

    val readyDisp = Wire(Bool())
    respQ.ready := readyDisp

    val reqCmdSourcedInternally = Reg(Vec(outer.nCores, Bool()))

    when(icmAsCmdSrc.get.fire && icmAsCmdSrc.get.bits.inst.xd) {
      reqCmdSourcedInternally(icmAsCmdSrc.get.bits.core_id) := true.B
    }
    when(swio.cmd.fire && swio.cmd.bits.inst.xd) {
      reqCmdSourcedInternally(swio.cmd.bits.core_id) := false.B
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
      core.composer_response_io.valid := responseManager.io.valid && response.core_id === core_idx.U
      core.composer_response_io.bits := response
    }

    responseManager.io.ready := VecInit(cores.map(_.composer_response_io.ready))(response.core_id)
  }

  val channelSelect = Cat(cmd.bits.inst.rs2(2, 0), cmd.bits.inst.rs1)

  cores.zipWithIndex.foreach { case (core, i) =>
    // hopefully this maps across the SLR
    val coreStart = cmd.valid && funct === ComposerFunc.START.U && coreSelect === i.U
    if (p(CoreCommandLatency) > 0) {
      val coreCmdQueue = Module(new Queue[ComposerRoccCommand](new ComposerRoccCommand, 2))
      when (coreStart) {
        coreCmdQueue.io.enq <> cmd
      }.otherwise {
        coreCmdQueue.io.enq.valid := false.B
        coreCmdQueue.io.enq.bits := DontCare
      }
      coreCmdQueue.io.deq <> core.io_declaration.req
    } else {
      core.io_declaration.req.valid := coreStart
      core.io_declaration.req.bits := cmd.bits
    }
  }

  // scope to separate out read channel stuff
  val cmdFireLatch = RegNext(cmd.fire)
  val cmdBitsLatch = RegNext(cmd.bits)
  val functLatch = cmdBitsLatch.inst.funct
  val coreSelectLatch = cmdBitsLatch.core_id

  val addr_func_live = cmd.bits.inst.funct === ComposerFunc.ADDR.U && cmd.fire

  if (cores(0).read_ios.nonEmpty) {
    cores(0).read_ios(0)._2.valid := false.B
    cores(0).write_ios(0)._2.valid := true.B
  }
  val txLenFromCmd = if (addressBits > 0) cmd.bits.payload1(addressBits - 1, 0) else 0.U

  case class CChannelIdentifier(system_name: String, core_idx: Int, channel_name: String, channel_subidx: Int, io_idx: Int)

  @tailrec
  private def assign_channel_addresses(coreId: Int, channelList: List[((String, Int), DecoupledIO[ChannelTransactionBundle])],
                                       assignment: Int = 0,
                                       // core name, core_id, channel name, channel idx, io_idx
                                       io_map: List[CChannelIdentifier] = List()): List[CChannelIdentifier] = {
    channelList match {
      case (channel_identifier, txio) :: rst =>
        val tx_len = Reg(UInt(addressBits.W))
        val tx_addr_start = Reg(UInt(addressBits.W))
        when(addr_func_live && coreSelect === coreId.U && channelSelect === assignment.U) {
          tx_len := txLenFromCmd
          tx_addr_start := cmd.bits.payload2(addressBits - 1, 0)
        }
        txio.valid := cmdFireLatch && functLatch === ComposerFunc.START.U && coreSelectLatch === coreId.U
        txio.bits.addr := tx_addr_start
        txio.bits.len := tx_len
        assign_channel_addresses(coreId, rst, assignment + 1,
          CChannelIdentifier(
            system_name = outer.systemParams.name,
            core_idx = coreId,
            channel_name = channel_identifier._1,
            channel_subidx = channel_identifier._2,
            io_idx = assignment) :: io_map)
      case _ => io_map
    }
  }

  val core_io_mappings = cores.zipWithIndex.map { case (co, id) =>
    assign_channel_addresses(id, co.read_ios ++ co.write_ios)
  }

  tieClocks()
}
