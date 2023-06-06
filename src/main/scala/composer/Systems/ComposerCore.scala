package composer.Systems

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import composer._
import composer.ComposerParams.{CoreIDLengthKey, SystemIDLengthKey}
import composer.MemoryStreams._
import composer.RoccHelpers._
import composer.TLManagement.{ComposerIntraCoreIOModule, TLClientModule}
import composer.common._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._

class CustomIO[T1 <: Bundle, T2 <: Bundle](bundleIn: T1, bundleOut: T2) extends Bundle {
  val req: DecoupledIO[T1] = DecoupledIO(bundleIn)
  val resp: DecoupledIO[T2] = Flipped(DecoupledIO(bundleOut))
}

private[composer] object CustomCommandUsage extends Enumeration {
  //noinspection ScalaUnusedSymbol
  type custom_usage = Value
  val unused, default, custom = Value
}

class CustomIOWithRouting[T1 <: Bundle, T2 <: Bundle](bundleIn: T1, bundleOut: T2) extends Bundle {
  val req: DecoupledIOWithCRouting[T1] = DecoupledIOWithCRouting(bundleIn)
  val resp: DecoupledIO[T2] = Flipped(DecoupledIO(bundleOut))
}

class ComposerCoreIO(implicit p: Parameters) extends CustomIO[ComposerRoccCommand, ComposerRoccUserResponse](new ComposerRoccCommand, new ComposerRoccUserResponse)

class DataChannelIO(dataBytes: Int, vlen: Int = 1) extends Bundle {
  val data = Decoupled(Vec(vlen, UInt((dataBytes * 8).W)))
  val in_progress = Output(Bool())
}


class ComposerCoreWrapper(val composerSystemParams: ComposerSystemParams, val core_id: Int, val system_id: Int, val systemRef: ComposerSystem)(implicit p: Parameters) extends LazyModule {
  lazy val module = composerSystemParams.buildCore(ComposerConstructor(composerSystemParams.coreParams, this), p)
  val coreParams = composerSystemParams.coreParams.copy(core_id = core_id, system_id = system_id)
  val blockBytes = p(CacheBlockBytes)
  val unCachedReaders = coreParams.memoryChannelParams.filter(_.isInstanceOf[CReadChannelParams]).map { para =>
    val param: CReadChannelParams = para.asInstanceOf[CReadChannelParams]
    (param.name, List.tabulate(para.nChannels) { i =>
      TLClientNode(List(TLMasterPortParameters.v1(
        clients = List(TLMasterParameters.v1(
          name = s"ReadChannel_sys${system_id}_core${core_id}_${para.name}$i",
          supportsGet = TransferSizes(1, blockBytes * p(PrefetchSourceMultiplicity)),
          supportsProbe = TransferSizes(1, blockBytes * p(PrefetchSourceMultiplicity)),
          sourceId = IdRange(0, param.maxInFlightTxs)
        )))))
    })
  }
  val writers = coreParams.memoryChannelParams.filter(_.isInstanceOf[CWriteChannelParams]).map { mcp =>
    val para = mcp.asInstanceOf[CWriteChannelParams]
    (para.name, List.tabulate(para.nChannels) { i =>
      TLClientNode(List(TLMasterPortParameters.v1(
        List(TLMasterParameters.v1(
          name = s"WriteChannel_sys${system_id}_core${core_id}_${para.name}$i",
          sourceId = IdRange(0, para.maxInFlightTxs),
          supportsPutFull = TransferSizes(1, p(CacheBlockBytes)),
          supportsPutPartial = TransferSizes(1, p(CacheBlockBytes)),
          supportsProbe = TransferSizes(1, p(CacheBlockBytes)))))))
    })
  }
  val scratch_mod = coreParams.memoryChannelParams.filter(_.isInstanceOf[CScratchpadParams]).map(_.asInstanceOf[CScratchpadParams]).map {
    param =>
      lazy val mod = LazyModule(param.make)
      mod.suggestName(param.name)
      (param.name, mod)
  }
  val readerNodes = unCachedReaders
  // these go to external memory
  val mem_nodes = (unCachedReaders ++
    writers ++
    scratch_mod.map(i => (i._1, List(i._2.mem_master_node).filter(_.isDefined).map(_.get)))
    ).flatMap(_._2)
  val externalCoreCommNodes = Map.from(composerSystemParams.canIssueCoreCommandsTo.map { targetSys =>
    (targetSys, TLClientNode(Seq(TLMasterPortParameters.v1(clients = Seq(TLMasterParameters.v1(
      s"${composerSystemParams.name}_core${core_id}_to$targetSys",
      supportsProbe = TransferSizes(1 << log2Up(ComposerRoccCommand.packLengthBytes)),
      supportsPutFull = TransferSizes(1 << log2Up(ComposerRoccCommand.packLengthBytes))
    ))))))
  })
  val intraCoreMemSlaveNodes = coreParams.memoryChannelParams.filter(_.isInstanceOf[CIntraCoreMemoryPortIn]).map {
    r =>
      val mp = r.asInstanceOf[CIntraCoreMemoryPortIn]
      val memManagerParams = Seq.fill(mp.nChannels)(TLSlavePortParameters.v1(
        managers = Seq(TLSlaveParameters.v1(
          address = Seq(AddressSet(0, mp.nDatas.intValue() * mp.dataWidthBits.intValue() / 8 - 1)),
          regionType = RegionType.IDEMPOTENT,
          supportsPutFull = TransferSizes(mp.dataWidthBits.intValue() / 8)
        )), beatBytes = mp.dataWidthBits.intValue() / 8
      ))
      val xbar_sp_pairs = memManagerParams map { mm_param =>
        val intraCoreMemXbar = TLXbar()
        val sp = LazyModule(new CScratchpad(
          supportWriteback = false,
          asMemorySlave = Some(mm_param),
          dataWidthBits = mp.dataWidthBits,
          nDatas = mp.nDatas,
          latency = mp.latency,
          nPorts = 2,
          specialization = CScratchpadSpecialization.flatPacked)(p.alterPartial {
          case SimpleDRAMHintKey => true
        }))
        sp.mem_slave_node.get := intraCoreMemXbar
        (intraCoreMemXbar, sp)
      }
      val xbars = xbar_sp_pairs.map(_._1)
      val sps = xbar_sp_pairs.map(_._2)

      (mp.name, xbars, mp, sps)
  }
  val intraCoreMemMasters = coreParams.memoryChannelParams.filter(_.isInstanceOf[CIntraCoreMemoryPortOut]).map {
    r =>
      val mp = r.asInstanceOf[CIntraCoreMemoryPortOut]
      val (otherSystemParams, otherSPParams) = try {
        val otherS = p(ComposerSystemsKey).filter(_.name == mp.toSystem)(0)
        val otherSP = otherS.coreParams.memoryChannelParams.filter(_.name == mp.toMemoryPort)(0)
        (otherS, otherSP.asInstanceOf[CIntraCoreMemoryPortIn])
      } catch {
        case a: Exception =>
          System.err.println("Could not properly find system and memory port given by outward memory port params.")
          throw a
      }
      val memMasters = Seq.fill(otherSystemParams.nCores, otherSPParams.nChannels)(TLClientNode(Seq(TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          f"intraCoreMemPortOut_${mp.toSystem}_to_${mp.toMemoryPort}",
          supportsProbe = TransferSizes(otherSPParams.dataWidthBits.intValue() / 8),
          supportsPutFull = TransferSizes(otherSPParams.dataWidthBits.intValue() / 8)
        ))))))
      (mp, memMasters, otherSPParams)
  }
}

class ComposerCore(val composerConstructor: ComposerConstructor)(implicit p: Parameters) extends LazyModuleImp(composerConstructor.composerCoreWrapper) {
  private val outer = composerConstructor.composerCoreWrapper
  val composer_response_ios_ = Map.from(outer.composerSystemParams.canIssueCoreCommandsTo.map { target =>
    (target, {
      val io = IO(Flipped(Decoupled(new ComposerRoccResponse())))
      io.suggestName(s"ComposerIntraCoreResponsePort_$target")
      io
    })
  })
  private val composer_command_ios_ = outer.externalCoreCommNodes.map { case (target, node) =>
    val mod = Module(new TLClientModule(node))
    node.out(0)._1 <> mod.tl
    val wire = Wire(Decoupled(new ComposerRoccCommand))
    wire.ready := mod.io.ready
    mod.io.valid := wire.valid
    // NEED TO TELL OTHER CORE HOW TO SEND RESPONSE BACK
    val returnRoutingPayload = Cat(outer.system_id.U(SystemIDLengthKey.W), getCoreID.U(CoreIDLengthKey.W))
    mod.io.bits.dat := wire.bits.pack(withRoutingPayload = Some(returnRoutingPayload))
    val permAddress = getSystemID(target)
    when(mod.io.fire) {
      assert(permAddress === wire.bits.getSystemID, "system provided through intra-core comm hardware does not match system used to fetch interface")
    }
    mod.io.bits.addr := ComposerConsts.getInternalCmdRoutingAddress(getSystemID(target))
    (target, wire)
  }
  private[composer] val io_declaration = IO(Flipped(new ComposerCoreIO()))
  private[composer] val io_source = IO(Input(UInt(log2Up(outer.systemRef.acc.sysNCmdSourceLookup(outer.composerSystemParams.name)).W)))
  private[composer] var using_custom = CustomCommandUsage.unused
  private[composer] var custom_rocc_cmd_nbeats = -1

  def getCoreID: Int = composerConstructor.composerCoreWrapper.core_id

  def getIntraCoreMemOuts(name: String): CCoreChannelMultiAccessBundle[MemWritePort] = {
    val params = try {
      outer.intraCoreMemMasters.filter(_._1.name == name)(0)
    } catch {
      case e: Exception =>
        System.err.println("You may be trying to access a intra core mem port by the wrong name. Check your config.")
        throw e
    }

    val q = params._2.indices map (getIntraCoreMemOut(name, _))
    CCoreChannelMultiAccessBundle(q)
  }

  def getIntraCoreMemOut(name: String, core_idx: Int = 0): Seq[MemWritePort] = {
    val params = try {
      outer.intraCoreMemMasters.filter(_._1.name == name)(0)
    } catch {
      case e: Exception =>
        System.err.println("You may be trying to access a intra core mem port by the wrong name. Check your config.")
        throw e
    }

    params._2(core_idx).zipWithIndex.map { case(master_singleton, channelIdx) =>
      val master = master_singleton.out(0)
      val port = master._1
      val edge = master._2
      val w = Wire(new MemWritePort(log2Up(params._3.nDatas.intValue()), params._3.dataWidthBits.intValue()))
      w.suggestName(s"intraCoreWritePort_for$name" + "_" + channelIdx)
      port.a.valid := w.valid
      w.ready := port.a.ready
      port.a.bits := edge.Put(
        fromSource = 0.U,
        toAddress = if (params._3.dataWidthBits.intValue() == 8) w.bits.addr else Cat(w.bits.addr, 0.U(log2Up(params._3.dataWidthBits.intValue() / 8).W)),
        lgSize = CLog2Up(params._3.dataWidthBits.intValue() / 8).U,
        data = w.bits.data
      )._2
      port.d.ready := true.B
      w
    }
  }

  def getIntraCoreMemIns(name: String)(implicit valName: ValName): Seq[CScratchpadDualPortAccess] = {
    val params = try {
      outer.intraCoreMemSlaveNodes.filter(_._1 == name)(0)
    } catch {
      case e: Exception =>
        System.err.println(s"You may be trying to access a intra core mem port by the wrong name($name). Check your config.")
        throw e
    }
    implicit val nameHint = Some(valName.name)
    VecInit((0 until params._3.nChannels) map (getIntraCoreMemIn(name, _)))
  }

  def getIntraCoreMemIn(name: String, idx: Int): CScratchpadDualPortAccess = {
    val params = outer.intraCoreMemSlaveNodes.filter(_._1 == name)
    if (params.isEmpty) throw new Exception(s"Attempting to access intraCoreMem \"$name\" which we can't find in the config.")
    val ic_scratchpad = params(0)
    ic_scratchpad._4(idx).module.dual_port_IOs(0)
  }

  def getReaderModule(name: String,
                      dataBytes: Int,
                      vlen: Int,
                      idx: Int): (DecoupledIO[ChannelTransactionBundle], DataChannelIO) = {
    val a = getReaderModules(name, dataBytes, vlen, Some(idx))
    (a._1(0), a._2(0))
  }

  /**
   * Declare reader module implementations associated with a certain channel name.
   * Data channel will read out a vector of UInts of dimension (vlen, dataBytes*8 bits)
   *
   * @param name      name of channel to instantiate readers for
   * @param dataBytes width of the data channel to the user module
   * @param vlen      dimension of the data channel
   * @param idx       optionally instantiate an implementation for only a single channel in the name group. This may be useful
   *                  when different channels need to be parameterized differently
   * @return List of transaction information bundles (address and length in bytes) and then a data channel. For
   *         sparse readers, we give back both interfaces and for non-sparse, addresses are provided through separate address
   *         commands in software.
   */
  def getReaderModules(name: String,
                       dataBytes: Int,
                       vlen: Int,
                       idx: Option[Int] = None): (List[DecoupledIO[ChannelTransactionBundle]], List[DataChannelIO]) = {
    val mod = idx match {
      case Some(id_unpack) =>
        val clients = getTLClients(name, outer.readerNodes)
        List(Module(new CReader(dataBytes, vlen, clients(id_unpack), debugName = Some(name))))
      case None => getTLClients(name, outer.readerNodes).map(tab_id => Module(new CReader(dataBytes, vlen, tab_id, debugName = Some(name))))
    }
    //noinspection DuplicatedCode
    mod foreach { m =>
      m.tl_out <> m.tl_outer
      m.suggestName(name)
    }
    val ret = (mod.map(_.io.req), mod.map(_.io.channel))
    // initially tie off everything to false and DontCare. Saves some pain down the line
    ret._2.foreach { dat =>
      dat.data.ready := false.B
    }
    ret._1.foreach { dat =>
      dat.bits := DontCare
      dat.valid := false.B
    }
    ret
  }

  def getWriterModule(name: String,
                      dataBytes: Int,
                      idx: Int): (DecoupledIO[ChannelTransactionBundle], WriterDataChannelIO) = {
    val a = getWriterModules(name, dataBytes, Some(idx))
    (a._1(0), a._2(0))
  }

  def getScratchpad(name: String): (CScratchpadInitReqIO, Seq[CScratchpadAccessPort]) = {
    val outer = composerConstructor.composerCoreWrapper
    val lm = outer.scratch_mod.filter(_._1 == name)(0)._2
    lm.suggestName(name)
    val mod = lm.module

    (mod.req, mod.access)
  }

  def getWriterModules(name: String,
                       dataBytes: Int,
                       idx: Option[Int] = None): (List[DecoupledIO[ChannelTransactionBundle]], List[WriterDataChannelIO]) = {

    val params = outer.coreParams.memoryChannelParams.filter(_.name == name)
    require(params.length == 1, "Found writer descriptions (" + params.length + "). If > 1, then you have defined the" +
      " same group multiple times. If =0, then you have not described this writer group.")
    //    val param = params(0).asInstanceOf[CWriteChannelParams]
    val mod = idx match {
      case Some(id) => List(Module(new SequentialWriter(dataBytes, getTLClients(name, outer.writers)(id))))
      case None => getTLClients(name, outer.writers).map(tab_id => Module(new SequentialWriter(dataBytes, tab_id)))
    }
    //noinspection DuplicatedCode
    mod foreach { m =>
      m.tl_out <> m.tl_outer
      m.suggestName(name)
    }

    val ret = (mod.map(_.io.req), mod.map(_.io.channel))
    ret._2.foreach { dat =>
      dat.data.valid := false.B
      dat.data.bits := DontCare
    }
    ret._1.foreach { req =>
      req.valid := false.B
      req.bits := DontCare
    }
    ret
  }

  def ComposerIntraCoreIO[Tcmd <: ComposerCommand, Tresp <: ComposerUserResponse](endpoint: String,
                                                                                  genCmd: Tcmd = new ComposerRoccCommand,
                                                                                  genResp: Tresp = new ComposerRoccUserResponse): CustomIOWithRouting[Tcmd, Tresp] = {
    val converter = Module(new ComposerIntraCoreIOModule(endpoint, genCmd, genResp))
    converter.respIO <> composer_response_ios_(endpoint)
    converter.cmdIO <> composer_command_ios_(endpoint)
    converter.out
  }

  def ComposerIO[T1 <: ComposerCommand](bundleIn: T1): CustomIO[T1, ComposerRoccUserResponse] = {
    ComposerIO[T1, ComposerRoccUserResponse](bundleIn, new ComposerRoccUserResponse)
  }

  def getSystemID(name: String): UInt = p(SystemName2IdMapKey)(name).U

  def addrBits: Int = log2Up(p(ExtMem).get.master.size)

  def ComposerIO[T1 <: ComposerCommand, T2 <: ComposerUserResponse](bundleIn: T1, bundleOut: T2): CustomIO[T1, T2] = {
    if (using_custom == CustomCommandUsage.default) {
      throw new Exception("Cannot use custom io after using the default io")
    }
    using_custom = CustomCommandUsage.custom
    val composerCustomCommandManager = Module(new ComposerCommandBundler[T1, T2](bundleIn, bundleOut, composerConstructor.composerCoreWrapper, outer.systemRef.acc.sysNCmdSourceLookup(outer.composerSystemParams.name)))
    composerCustomCommandManager.suggestName(composerConstructor.composerCoreWrapper.composerSystemParams.name + "CustomCommand")
    composerCustomCommandManager.cio.cmd <> io_declaration
    composerCustomCommandManager.cio.cmd_in_source := io_source
    composerCustomCommandManager.io.resp.bits.rd := 0.U
    custom_rocc_cmd_nbeats = bundleIn.getNBeats
    composerCustomCommandManager.io
  }

  private def getTLClients(name: String, listList: List[(String, List[TLClientNode])]): List[TLClientNode] = {
    listList.filter(_._1 == name) match {
      case first :: rst =>
        require(rst.isEmpty)
        first._2
      case _ =>
        throw new Exception(s"getReaderModules failed. Tried to fetch a channel set with a name($name) that doesn't exist. Declared names: " +
          outer.unCachedReaders.map(_._1))
    }
  }

  def ComposerIO(): ComposerCoreIO = {
    if (using_custom == CustomCommandUsage.custom) {
      throw new Exception("Cannot use io after generating a custom io")
    }
    using_custom = CustomCommandUsage.default
    io_declaration
  }

  case class CCoreChannelMultiAccessBundle[T](dats: Seq[Seq[T]]) {
    def getCoreSlice(core: Int): Seq[T] = dats(core)

    def getChannelSlice(channel: Int): Seq[T] = dats.map(_(channel))
  }

  class CCoreChannelMultiAccessBundleChannelMajor[T](dat: Seq[Seq[T]]) extends CCoreChannelMultiAccessBundle(dat.transpose) {}

  object CCoreChannelMultiAccessBundleChannelMajor {
    def apply[T](dat: Seq[Seq[T]]): CCoreChannelMultiAccessBundleChannelMajor[T] =
      new CCoreChannelMultiAccessBundleChannelMajor(dat)
  }

}

class ComposerSystemIO(implicit p: Parameters) extends Bundle {
  val cmd = Flipped(Decoupled(new ComposerRoccCommand))
  val resp = Decoupled(new ComposerRoccResponse())
}