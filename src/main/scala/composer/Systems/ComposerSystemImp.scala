package composer.Systems

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer._
import composer.ComposerParams._
import composer.Floorplanning._
import composer.Generation._
import composer.MemoryStreams._
import composer.Platforms._
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
  val (cmdArbiter, cmdArbiter_dN) = (ModuleWithSLR(
    new RRArbiter(new CommandSrcPair(
      outer.acc.sysNCmdSourceLookup(outer.systemParams.name)),
      validSrcs.length),
    DieName.getFrontBusSLR,
    f"CommandArbiter${outer.systemParams.name}"), DotGen.addNode(f"${outer.systemParams.name}.cmdArb", DieName.getFrontBusSLR))

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
  val perDieReset = if (ConstraintGeneration.canDistributeOverSLRs()) {
    val nDies = platform.asInstanceOf[MultiDiePlatform].platformDies.length
    val resetRootIdx = platform.asInstanceOf[MultiDiePlatform].platformDies.indexWhere(_.resetRoot)
    val resets = Seq.fill(nDies) {
      Wire(Reset())
    }

    def treeReset(die: Int, r: Reset): Unit = {
      if (die < 0 || die >= nDies) return
      val resetBridge = ModuleWithSLR(new ResetBridge(r, 5), die, f"__resetBridge_${outer.system_id}_${die}")
      resetBridge.io.reset := r
      resetBridge.io.clock := clock
      die match {
        case x: Int if x == resetRootIdx =>
          resets(x) := resetBridge.io.dut_reset
          treeReset(die + 1, RegNext(resetBridge.io.dut_reset))
          treeReset(die - 1, RegNext(resetBridge.io.dut_reset))
        case x: Int if x < resetRootIdx =>
          resets(x) := resetBridge.io.dut_reset
          treeReset(die - 1, RegNext(resetBridge.io.dut_reset))
        case x: Int if x > resetRootIdx =>
          resets(x) := resetBridge.io.dut_reset
          treeReset(die + 1, RegNext(resetBridge.io.dut_reset))
      }
    }

    treeReset(resetRootIdx, reset)
    resets
  } else Seq(reset)
  val cores = Seq.tabulate(outer.nCores) { core_idx: Int =>
    println(s"Core ${core_idx} on slr ${core2slr(core_idx)}")
    val impl = ModuleWithSLR(outer.systemParams.moduleConstructor match {
      case mb: ModuleBuilder => mb.constructor(outer, p)
      case bbc: BlackboxBuilderCustom => new AcceleratorBlackBoxCore(outer, bbc)
      case bbb: BlackboxBuilderRocc => new AcceleratorBlackBoxCore(outer, bbb)
    }, core2slr(core_idx), f"System${outer.systemParams.name}_core${core_idx}_impl")
    val dotName = DotGen.addModuleNode(f"${outer.systemParams.name}.core$core_idx.impl", core2slr(core_idx))
    if (ConstraintGeneration.canDistributeOverSLRs()) {
      impl.reset := RegNext(perDieReset(core2slr(core_idx)))
    } else {
      impl.reset := perDieReset(0)
    }
    (impl, dotName)
  }


  ComposerBuild.requestModulePartition(outer.systemParams.name)
  val coreGroups = cores.zipWithIndex.map { case (co, idx) => (co, core2slr(idx), idx) }.groupBy(_._2)

  var gl_incrementer = 0
  val (resp, resp_dN) = {
    def collapseResp(resps: Seq[(DecoupledIO[AccelRoccResponse], String)], slr_id: Int): (DecoupledIO[AccelRoccResponse], String) = {
      @tailrec
      def extend(d: DecoupledIO[AccelRoccResponse], l: Int, dn: String): (DecoupledIO[AccelRoccResponse], String) = {
        if (l == 0) (d, dn)
        else {
          val (q, q_dN) = (
            ModuleWithSLR(new Queue(new AccelRoccResponse, 1), slr_id, f"__${outer.systemParams.name}_rExtend$gl_incrementer"),
            DotGen.addNode(f"${outer.systemParams.name}.respQ_extend"))
          DotGen.addEdge(dn, q_dN)
          gl_incrementer += 1
          q.io.enq <> d
          extend(q.io.deq, l-1, q_dN)
        }
      }

      val extendeds = resps.map(a => extend(a._1, 3, a._2))
      val (respArb, respArb_dN) = (ModuleWithSLR(
        new RRArbiter(new AccelRoccResponse(), extendeds.length),
        slr_id,
        f"respArb_${outer.system_id}_${slr_id}_$gl_incrementer"),
        DotGen.addNode(f"${outer.systemParams.name}.respArbit"))
      respArb.io.in zip extendeds foreach { case (a, (b, b_dN)) =>
        a <> b
        DotGen.addEdge(b_dN, respArb_dN)
      }
      extend(respArb.io.out, 3, respArb_dN)
    }

    if (coreGroups.keys.size == 1) {
      val coreResps = cores.zipWithIndex.map { case ((c: AcceleratorCore, cn: String), cidx: Int) =>
        val lastRecievedRd = Reg(UInt(5.W))
        when(c.io_declaration.req.fire) {
          lastRecievedRd := c.io_declaration.req.bits.inst.rd
        }
        val (resp_queue, resp_queue_dN) = (ModuleWithSLR(new Queue[AccelRoccResponse](
          new AccelRoccResponse(), entries = 1), DieName.getFrontBusSLR),
          DotGen.addNode(f"${outer.systemParams.name}.respQHead"))
        DotGen.addEdge(cn, resp_queue_dN)
        resp_queue.io.enq.bits.system_id := outer.system_id.U
        resp_queue.io.enq.bits.core_id := cidx.U
        resp_queue.io.enq.bits.rd := lastRecievedRd
        resp_queue.io.enq.bits.getDataField := c.io_declaration.resp.bits.getDataField
        resp_queue.io.enq.valid := c.io_declaration.resp.valid
        c.io_declaration.resp.ready := resp_queue.io.enq.ready
        (resp_queue.io.deq, resp_queue_dN)
      }
      collapseResp(coreResps, DieName.getFrontBusSLR)
    } else {
      val respsCollapsed = coreGroups.map { case (slr_id, core_list) =>
        (slr_id, collapseResp(core_list.map {
          case ((c: AcceleratorCore, cn: String), slr_id: Int, core_idx: Int) =>
            val respEPs = c.io_declaration.resp.map { ioresp =>
              val composerRespWire = Wire(new AccelRoccResponse())
              composerRespWire.getDataField := ioresp.getDataField
              composerRespWire.rd := ioresp.rd
              composerRespWire.system_id := outer.system_id.U
              composerRespWire.core_id := core_idx.U
              composerRespWire
            }
            (respEPs, cn)
        }, slr_id))
      }
      platform match {
        case pd: Platform with MultiDiePlatform =>
          val frontDie = pd.platformDies.indexWhere(_.frontBus)

          def respTree(slr_id: Int): Option[(DecoupledIO[AccelRoccResponse], String)] = {
            if (slr_id < 0 || slr_id >= pd.platformDies.length) None
            else {
              val up = if (slr_id >= frontDie) respTree(slr_id + 1) else None
              val down = if (slr_id <= frontDie) respTree(slr_id - 1) else None
              val me = Some(respsCollapsed(slr_id))
              Some(collapseResp(Seq(up, down, me).filter(_.isDefined).map(_.get), slr_id))
            }
          }

          respTree(frontDie).get
        case _ =>
          respsCollapsed(0)
      }
    }
  }

  val (respQ, respQ_dN) = (Queue(resp),
    DotGen.addNode(f"${outer.systemParams.name}.respTop"))
  DotGen.addEdge(resp_dN, respQ_dN)

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


    cores.zipWithIndex.foreach { case ((core, _), core_idx) =>
      core.composer_response_ios_(target).valid := responseManager.io.valid && response.core_id === core_idx.U
      core.composer_response_ios_(target).bits := response
    }

    responseManager.io.ready := VecInit(cores.map(a => a._1.composer_response_ios_(target).ready))(response.core_id)
  }

  var qCntr = 0

  // take in series of endpoints that recieve a commandSrc pair and return a smaller sequence (by degree radix) of commandSrc pairs
  def collapseCoreGroup(srcs: Seq[(DecoupledIO[CommandSrcPair], Seq[Int], String)],
                        slr_id: Option[Int],
                        extend_length: Int): (DecoupledIO[CommandSrcPair], Seq[Int], String) = {
    def constructQ(name: String, slri: Option[Int]): Queue[CommandSrcPair] = {
      val qConstructor: () => Queue[CommandSrcPair] =
        () => new Queue(new CommandSrcPair(outer.acc.sysNCmdSourceLookup(outer.systemParams.name)), 1)
      slri match {
        case Some(idx) =>
          val m = ModuleWithSLR(qConstructor(), idx, name)
          m
        case None =>
          val m = Module(qConstructor())
          m.suggestName(name)
      }
    }

    def extend_endpoint(d: DecoupledIO[CommandSrcPair], l: Int, dn: String): (DecoupledIO[CommandSrcPair], String) = {
      if (l == 0) (d, dn)
      else {
        val extender = constructQ(s"${outer.systemParams.name}_extender_$qCntr", None)
        val dotN = DotGen.addNode(f"${outer.systemParams.name}.cmd_extend", slr_id.getOrElse(-1))
        DotGen.addEdge(dotN, dn)
        extender.io.deq <> d
        extend_endpoint(extender.io.enq, l - 1, dotN)
      }
    }

    val fanIn = constructQ(s"${outer.systemParams.name}_fanIn_$qCntr", slr_id)
    val fanInDN = DotGen.addNode(f"${outer.systemParams.name}.fanIn", slr_id.getOrElse(-1))
    val (fanInEntrance, fanInEnN) = extend_endpoint(fanIn.io.enq, 3, fanInDN)
    fanIn.io.deq.ready := false.B
    qCntr += 1

    val extenders = srcs.map { case (cg, cgids, dN) =>
      @tailrec
      val (extendedEndpoint, eeN) = extend_endpoint(cg, extend_length, dN)
      DotGen.addEdge(fanInDN, eeN)
      val isSelected = cgids.map { id: Int => fanIn.io.deq.bits.cmd.getCoreID === id.U }.reduce(_ || _)
      extendedEndpoint.valid := fanIn.io.deq.valid && isSelected
      extendedEndpoint.bits := fanIn.io.deq.bits
      when(isSelected) {
        fanIn.io.deq.ready := extendedEndpoint.ready
      }
      qCntr += 1
      (extendedEndpoint, cgids)
    }
    (fanInEntrance, extenders.flatMap(_._2), fanInEnN)
  }

  def core2CmdEndpoint(core: AcceleratorCore): DecoupledIO[CommandSrcPair] = {
    val cmdsrcpr = Wire(Output(Decoupled(new CommandSrcPair(outer.acc.sysNCmdSourceLookup(outer.systemParams.name)))))
    core.io_declaration.req.bits := cmdsrcpr.bits.cmd
    core.io_source := cmdsrcpr.bits.src
    core.io_declaration.req.valid := cmdsrcpr.valid
    cmdsrcpr.ready := core.io_declaration.req.ready
    // queue in front of every core (no-fanout)
    cmdsrcpr
  }

  val (cmd_origin, cmd_origin_dN) = platform match {
    case pd: Platform with MultiDiePlatform =>
      val cmdRoot = pd.platformDies.indexWhere(_.frontBus)

      def treeCmd(slr_id: Int): Option[(DecoupledIO[CommandSrcPair], Seq[Int], String)] = {
        if (slr_id < 0 || slr_id >= pd.platformDies.length) None
        else {
          val up = if (slr_id >= cmdRoot) treeCmd(slr_id + 1) else None
          val down = if (slr_id <= cmdRoot) treeCmd(slr_id - 1) else None
          val me = coreGroups.getOrElse(slr_id, Seq.empty).map { case ((core, cn), _, core_idx) =>
            (core2CmdEndpoint(core), Seq(core_idx), cn)
          }
          val me_col = if (me.isEmpty) None else Some(collapseCoreGroup(me, Some(slr_id), 3))
          val all_group = Seq(me_col, up, down).filter(_.isDefined).map(_.get)
          if (all_group.isEmpty) None
          else Some(collapseCoreGroup(all_group, Some(slr_id), 1))
        }
      }

      val tc = treeCmd(cmdRoot).get
      (tc._1, tc._3)
    case _ =>
      val tc = collapseCoreGroup(coreGroups(0).map { a => (core2CmdEndpoint(a._1._1), Seq(a._3), a._1._2) }, None, 4)
      (tc._1, tc._3)
  }
  cmd <> cmd_origin
  DotGen.addEdge(cmdArbiter_dN, cmd_origin_dN)

  /* handle all memory */
  cores.zipWithIndex.zip(outer.readers) foreach { case (((core, cN), coreIdx), readSet) =>
    readSet.foreach { case (nodeParams, nodes) =>
      val (cParams, clients) = core.read_ios(nodeParams.name)
      (nodes zip clients).zipWithIndex foreach { case (((node, nn), client), channel_idx) =>
        val readerModule = ModuleWithSLR(new SequentialReader(client._2.data.bits.getWidth,
          node.out(0)._1, node.out(0)._2,
          cParams.bufferSizeBytesMin),
          core2slr(coreIdx),
          s"readerModule_${outer.baseName}_${nodeParams.name}_core${coreIdx}_channel$channel_idx")
        DotGen.addEdge(cN, nn)
        println(s"Connecting reader $cN to $nn")
        readerModule.io.channel <> client._2
        readerModule.io.req <> client._1
        readerModule.tl_out <> node.out(0)._1
      }
    }
  }
  cores.zipWithIndex.zip(outer.writers) foreach { case (((core, cN), coreIdx), writeSet) =>
    writeSet foreach { case (nodeName, nodes) =>
      val (cParams, clients) = core.write_ios(nodeName.name)
      (nodes zip clients).zipWithIndex foreach { case (((node, nn), client), channel_idx) =>
        val writerModule = ModuleWithSLR(new SequentialWriter(
          client._2.dWidth / 8,
          node.out(0)._1, node.out(0)._2,
          cParams.bufferSizeBytesMin), core2slr(coreIdx),
          s"writerModule_${outer.baseName}_${nodeName.name}_core${coreIdx}_channel${channel_idx}")
        DotGen.addEdge(cN, nn)
        writerModule.io.channel <> client._2
        writerModule.io.req <> client._1
        writerModule.tl_out <> node.out(0)._1
      }

    }
  }
  cores.zip(outer.scratch_mod) foreach { case ((core, cN), smods) =>
    smods foreach { case (spad_name, smod) =>
      val (sparams, spi) = core.sp_ios(spad_name.name)
      spi._1 <> smod.module.req
      spi._2 zip smod.module.IOs foreach { case (a, b) => a <> b }
      smod.mem_reader.foreach { case (_, a) =>
        DotGen.addEdge(cN, a)
      }
      smod.mem_writer.foreach { case (_, a) =>
        DotGen.addEdge(cN, a)
      }
//      println(s"Connecting spad to $cN")
    }
  }
  tieClocks()
}
