package composer

import chipsalliance.rocketchip.config._
import chisel3.util._
import chisel3._
import composer.MemoryStreams._
import composer.TLManagement.{TLClientModule, TLManagerModule}
import composer.common._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import scala.annotation.tailrec

class ComposerSystem(val systemParams: ComposerSystemParams, val system_id: Int)(implicit p: Parameters) extends LazyModule {
  val nCores = systemParams.nCores
  val coreParams = systemParams.coreParams

  val cores = List.tabulate(nCores) { idx: Int =>
    LazyModule(new ComposerCoreWrapper(systemParams, idx, system_id))
  }

  // we want to avoid high-degree xbars. Recursively make multi-stage xbar network
  val memory_nodes = {
    val core_mems = cores.flatMap(_.mem_nodes).map { mn =>
      val tli = TLIdentityNode()
      tli := mn
      tli
    }

    def recursivelyReduceXBar(grp: Seq[TLIdentityNode]): Seq[TLIdentityNode] = {
      if (grp.length <= p(CChannelXBarWidth)) grp
      else (grp.grouped(p(CChannelXBarWidth)) map { subgroup =>
        val sub_xbar = TLXbar()
        subgroup foreach (sub_xbar := _)
        val out_node = TLIdentityNode()
        out_node := TLBuffer() := sub_xbar
        out_node
      }).toSeq
    }

    recursivelyReduceXBar(core_mems)
  }

  /* SEND OUT STUFF*/

  // ROUTE OUTGOING COMMANDS THROUGH HERE
  val outgoingCommandXbar = if (systemParams.canIssueCoreCommands) {
    val xbar = TLXbar()
    cores.foreach(xbar := TLBuffer() := _.externalCoreCommNodes.get)
    Some(xbar)
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
    val xbar = TLXbar()
    xbar := client
    (Some(client), Some(xbar))
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
    val xbar = TLXbar()
    manager := TLBuffer() := xbar

    (Some(manager), Some(xbar))
  } else (None, None)

  val (incomingInternalResponseManager, incomingInternalResponseXBar) = if (systemParams.canIssueCoreCommands) {
    val manager = TLManagerNode(
      Seq(TLSlavePortParameters.v1(
        managers = Seq(TLSlaveParameters.v1(
          Seq(ComposerConsts.getInternalCmdRoutingAddressSet(system_id)),
          supportsPutFull = TransferSizes(ComposerRoccResponse.getWidthBytes)
        )), beatBytes = ComposerRoccResponse.getWidthBytes
      )))
    val xbar = TLXbar()
    manager := xbar
    (Some(manager), Some(xbar))
  } else (None, None)

  lazy val module = new ComposerSystemImp(this)
}

case class CChannelIdentifier(system_name: String, core_idx: Int, channel_name: String, channel_subidx: Int, io_idx: Int)

class ComposerSystemImp(val outer: ComposerSystem) extends LazyModuleImp(outer) {
  val sw_io = if (outer.systemParams.canReceiveSoftwareCommands) Some(IO(new ComposerSystemIO())) else None
  val respArbiter = Module(new RRArbiter(new ComposerRoccUserResponse(), outer.systemParams.nCores))
  val cores = outer.cores.map(_.module)
  val busy = IO(Output(Bool()))
  busy := cores.map(_.io.busy).reduce(_ || _)

  val validSrcs = Seq(sw_io, outer.internalCommandManager).filter(_.isDefined)
  // if sources can come from multiple domains (sw, other systems), then we have to remember where cmds came from
  val cmdArbiter = Module(new RRArbiter(new ComposerRoccCommand(), validSrcs.length))
  val icmAsCmdSrc = if (outer.internalCommandManager.isDefined) {
    val managerNode = outer.internalCommandManager.get
    val (b, e) = managerNode.in(0)
    val manager = Module(new TLManagerModule(b, e))
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

  val validCmdSrcs = Seq(icmAsCmdSrc, ioAsCmdSrc).filter(_.isDefined)
  validCmdSrcs.zipWithIndex.foreach { case (src, idx) =>
    cmdArbiter.io.in(idx) <> src.get
  }

  val internalReturnDestinations = if (p(RequireInternalCommandRouting)) Some(VecInit(Seq.fill(outer.nCores)(Reg(new Bundle() {
    val sys = UInt(p(SystemIDLengthKey).W)
    val core = UInt(p(CoreIDLengthKey).W)
  })))) else None

  if (p(RequireInternalCommandRouting)) {
    // a core must have only 1 ongoing command that can recieve a response. If the command was sourced from a core, then
    // the response must go to that same core, and if from software then back to software. Mark this bit whenever a
    // command is processed to remember where it came from
    val routingPayload = outer.internalCommandManager.get.in(0)._1.a.bits.data(
      ComposerRoccCommand.packLengthBytes * 8 + p(SystemIDLengthKey) + p(CoreIDLengthKey) - 1,
      ComposerRoccCommand.packLengthBytes * 8)
    val fromCore = routingPayload(p(CoreIDLengthKey) - 1, 0)
    val fromSys = routingPayload(p(CoreIDLengthKey) + p(SystemIDLengthKey) - 1, p(CoreIDLengthKey))
    val intCmd = icmAsCmdSrc.get

    // arbiter is choosing this one
    when(intCmd.fire && intCmd.bits.inst.xd) {
      internalReturnDestinations.get(intCmd.bits.core_id).sys := fromSys
      internalReturnDestinations.get(intCmd.bits.core_id).core := fromCore
      // arbiter is consuming input and we are expecting response, so mark as cmd destination
    }
  }

  lazy val cmd = Queue(cmdArbiter.io.out)
  cmd.ready := funct =/= ComposerFunc.START.U || VecInit(cores.map(_.io.req.ready))(coreSelect)

  lazy val funct = cmd.bits.inst.funct

  lazy val coreSelect = cmd.bits.core_id

  val addressBits = outer.memory_nodes map { m =>
    m.out(0)._1.params.addressBits
  } match {
    case Seq() => 0
    case l: Seq[Int] => l.max
    case _ => 0
  }


  respArbiter.io.in <> cores.map(_.io.resp)
  val resp = Wire(Decoupled(new ComposerRoccResponse()))
  resp.valid := respArbiter.io.out.valid
  resp.bits.rd := respArbiter.io.out.bits.rd
  resp.bits.core_id := respArbiter.io.chosen // .io.out.bits.core_id
  resp.bits.data := respArbiter.io.out.bits.data
  resp.bits.system_id := outer.system_id.U
  respArbiter.io.out.ready := resp.ready

  val respQ = Queue(resp)

  val internalRespDispatchModule = if (p(RequireInternalCommandRouting)) {
    val dests = internalReturnDestinations.get
    val wire = Wire(new ComposerRoccResponse())
    wire.data := respQ.bits.data
    wire.rd := respQ.bits.rd
    wire.system_id := dests(respQ.bits.core_id).sys
    wire.core_id := dests(respQ.bits.core_id).core

    val respClient = outer.outgoingInternalResponseClient.get
    val internalRespDispatcher = Module(new TLClientModule(respClient))
    internalRespDispatcher.tl <> respClient.out(0)._1
    internalRespDispatcher.io.bits.dat := wire.pack
    internalRespDispatcher.io.bits.addr := ComposerConsts.getInternalCmdRoutingAddress(wire.system_id)
    Some(internalRespDispatcher)
  } else None


  if (outer.systemParams.canReceiveSoftwareCommands && p(RequireInternalCommandRouting)) {
    val readyDisp = Wire(Bool())
    respQ.ready := readyDisp

    val reqCmdSourcedInternally = VecInit(Seq.fill(outer.nCores)(Reg(Bool())))

    when(icmAsCmdSrc.get.fire && icmAsCmdSrc.get.bits.inst.xd) {
      reqCmdSourcedInternally(icmAsCmdSrc.get.bits.core_id) := true.B
    }


    sw_io.get.resp.valid := false.B
    internalRespDispatchModule.get.io.valid := false.B
    sw_io.get.resp.bits <> respQ.bits

    when(reqCmdSourcedInternally(respQ.bits.core_id)) {
      // command was sourced internally
      readyDisp := internalRespDispatchModule.get.io.ready
      internalRespDispatchModule.get.io.valid := respQ.valid
    }.otherwise {
      readyDisp := sw_io.get.resp.ready
      sw_io.get.resp.valid := respQ.valid
    }
  } else if (outer.systemParams.canReceiveSoftwareCommands) {
    // simple case!
    sw_io.get.resp <> respQ
  } else if (p(RequireInternalCommandRouting)) {
    respQ.ready := internalRespDispatchModule.get.io.ready
    internalRespDispatchModule.get.io.valid := resp.valid
    internalRespDispatchModule.get.io.bits.dat := respQ.bits.pack
    internalRespDispatchModule.get.io.bits.addr := ComposerConsts.getInternalCmdRoutingAddress(respQ.bits.system_id)
  } else {
    throw new Exception("System unreachable!")
  }

  if (outer.systemParams.canIssueCoreCommands) {
    val managerNode = outer.incomingInternalResponseManager.get
    val (mBundle, mEdge) = managerNode.in(0)
    val responseManager = Module(new TLManagerModule(mBundle, mEdge))
    responseManager.tl <> outer.incomingInternalResponseManager.get.in(0)._1
    val response = ComposerRoccResponse(responseManager.io.bits)

    cores.zipWithIndex.foreach { case (core, core_idx) =>
      core.composer_response_io.get.valid := responseManager.io.valid && response.core_id === core_idx.U
      core.composer_response_io.get.bits := response
    }

    responseManager.io.ready := VecInit(cores.map(_.composer_response_io.get.ready))(response.core_id)
  }
  val lenBits = log2Up(p(MaxChannelTransactionLenKey))

  val channelSelect = Cat(cmd.bits.inst.rs2(2, 0), cmd.bits.inst.rs1)

  cores.zipWithIndex.foreach { case (core, i) =>
    val coreStart = cmd.fire && funct === ComposerFunc.START.U && coreSelect === i.U
    core.io.req.valid := coreStart
    core.io.req.bits := cmd.bits
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
  val txLenFromCmd = cmd.bits.payload1(lenBits - 1, 0)

  @tailrec
  private def assign_channel_addresses(coreId: Int, channelList: List[((String, Int), DecoupledIO[ChannelTransactionBundle])],
                                       assignment: Int = 0,
                                       // core name, core_id, channel name, channel idx, io_idx
                                       io_map: List[CChannelIdentifier] = List()): List[CChannelIdentifier] = {
    channelList match {
      case (channel_identifier, txio) :: rst =>
        val tx_len = Reg(UInt(log2Up(p(MaxChannelTransactionLenKey)).W))
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
}