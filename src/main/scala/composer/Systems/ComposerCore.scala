package composer.Systems

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import composer.MemoryStreams._
import composer.RoccHelpers._
import composer.TLManagement.{ComposerRespConverter, MultiBeatCommandEmitter, TLClientModule}
import composer.common._
import composer.{ComposerConstructor, ComposerSystemParams, SystemName2IdMapKey}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class CustomIO[T1 <: Bundle, T2 <: Bundle](bundleIn: T1, bundleOut: T2) extends Bundle {
  val req: DecoupledIO[T1] = DecoupledIO(bundleIn.cloneType)
  val resp: DecoupledIO[T2] = Flipped(DecoupledIO(bundleOut.cloneType))
  val busy = Input(Bool())
}

class ComposerCoreIO(implicit p: Parameters) extends CustomIO[ComposerRoccCommand, ComposerRoccUserResponse](new ComposerRoccCommand, new ComposerRoccUserResponse)

class DataChannelIO(dataBytes: Int, vlen: Int = 1) extends Bundle {
  val data = Decoupled(Vec(vlen, UInt((dataBytes * 8).W)))
  val in_progress = Output(Bool())
}

class ComposerCoreWrapper(val composerSystemParams: ComposerSystemParams, val core_id: Int, val system_id: Int)(implicit p: Parameters) extends LazyModule {
  val coreParams = composerSystemParams.coreParams.copy(core_id = core_id, system_id = system_id)
  val blockBytes = p(CacheBlockBytes)

  val CacheNodes = coreParams.memoryChannelParams.filter(_.channelType == CChannelType.CacheChannel) map (_.asInstanceOf[CCachedReadChannelParams]) map {
    par =>
      val param = par.cacheParams
      val cache = TLCache(param.sizeBytes,
        nClients = par.nChannels,
        associativity = param.associativity).suggestName("TLCache")
      val req_xbar = LazyModule(new TLXbar())
      val rnodes = List.tabulate(par.nChannels)(i => TLClientNode(List(TLMasterPortParameters.v1(
        clients = List(TLMasterParameters.v1(
          name = s"CachedReadChannel_sys${system_id}_core${core_id}_${par.name}$i",
          supportsGet = TransferSizes(1, p(CacheBlockBytes)),
          supportsProbe = TransferSizes(1, p(CacheBlockBytes))
        ))))))

      rnodes foreach (req_xbar.node := _)
      cache.mem_reqs :=
        //        TLBuffer() := TLWidthWidget(blockBytes) :=
        TLBuffer() := req_xbar.node
      (par.name, (cache, rnodes))
  }
  val unCachedReaders = coreParams.memoryChannelParams.filter(_.channelType == CChannelType.ReadChannel).map { para =>
    val param: CReadChannelParams = para.asInstanceOf[CReadChannelParams]
    (param.name, List.tabulate(para.nChannels) { i =>
      TLClientNode(List(TLMasterPortParameters.v1(
        clients = List(TLMasterParameters.v1(
          name = s"ReadChannel_sys${system_id}_core${core_id}_${para.name}$i",
          supportsGet = TransferSizes(1, p(CacheBlockBytes)),
          supportsProbe = TransferSizes(1, p(CacheBlockBytes)),
          sourceId = IdRange(0, param.maxInFlightTxs)
        )))))
    })
  }
  val writers = coreParams.memoryChannelParams.filter(_.channelType == CChannelType.WriteChannel).map { mcp =>
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
  val scratch_mod = coreParams.memoryChannelParams.filter(_.channelType == CChannelType.Scratchpad).map(_.asInstanceOf[CScratchpadChannelParams]).map {
    param =>
      lazy val mod = LazyModule(param.make)
      mod.suggestName(param.name)
      (param.name, mod)
  }
  val readerNodes = unCachedReaders ++ CacheNodes.map(i => (i._1, i._2._2))

  val externalCoreCommNodes = if (composerSystemParams.canIssueCoreCommands) {
    Some(TLClientNode(Seq(TLMasterPortParameters.v1(clients = Seq(TLMasterParameters.v1(
      s"${composerSystemParams.name}_core${core_id}_toOtherCores",
      supportsProbe = TransferSizes(1 << log2Up(ComposerRoccCommand.packLengthBytes)),
      supportsPutFull = TransferSizes(1 << log2Up(ComposerRoccCommand.packLengthBytes))
    ))))))
  } else None

  val mem_nodes = (
    CacheNodes.map(i => (i._1, i._2._2)) ++
      unCachedReaders ++
      writers ++
      scratch_mod.map(i => (i._1, List(i._2.mem)))
    ).flatMap(_._2)

  lazy val module = composerSystemParams.buildCore(ComposerConstructor(composerSystemParams.coreParams, this), p)
}

class ComposerCore(val composerConstructor: ComposerConstructor)(implicit p: Parameters) extends
  LazyModuleImp(composerConstructor.composerCoreWrapper) {

  private val outer = composerConstructor.composerCoreWrapper

  def getCoreID: Int = composerConstructor.composerCoreWrapper.core_id


  private def getTLClients(name: String, listList: List[(String, List[TLClientNode])]): List[TLClientNode] = {
    listList.filter(_._1 == name) match {
      case first :: rst =>
        require(rst.isEmpty)
        first._2
      case _ =>
        throw new Exception(s"getReaderModules failed. Tried to fetch a channel set with a name($name) that doesn't exist. Declared names: " +
          (outer.unCachedReaders.map(_._1) ++ outer.CacheNodes.map(_._1)))
    }
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
                       useSoftwareAddressing: Boolean,
                       dataBytes: Int,
                       vlen: Int,
                       idx: Option[Int] = None): (List[DecoupledIO[ChannelTransactionBundle]], List[DataChannelIO]) = {
    val mod = idx match {
      case Some(id_unpack) =>
        val clients = getTLClients(name, outer.readerNodes)
        List(Module(new CReader(dataBytes, vlen, clients(id_unpack))))
      case None => getTLClients(name, outer.readerNodes).map(tab_id => Module(new CReader(dataBytes, vlen, tab_id)))
    }
    //noinspection DuplicatedCode
    mod foreach { m =>
      m.tl_out <> m.tl_outer
      m.suggestName(name)
    }
    val ret = (if (useSoftwareAddressing) List()
    else mod.map(_.io.req),
      mod.map(_.io.channel))
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

  def getReaderModule(name: String,
                      useSoftwareAddressing: Boolean,
                      dataBytes: Int,
                      vlen: Int,
                      idx: Int): (DecoupledIO[ChannelTransactionBundle], DataChannelIO) = {
    val a = getReaderModules(name, useSoftwareAddressing, dataBytes, vlen, Some(idx))
    (a._1(0), a._2(0))
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

  def getWriterModule(name: String,
                      dataBytes: Int,
                      idx: Int): (DecoupledIO[ChannelTransactionBundle], WriterDataChannelIO) = {
    val a = getWriterModules(name, dataBytes, Some(idx))
    (a._1(0), a._2(0))
  }


  def getScratchpad(name: String): (CScratchpadInitReqIO, CScratchpadAccessBundle) = {
    val outer = composerConstructor.composerCoreWrapper
    val lm = outer.scratch_mod.filter(_._1 == name)(0)._2
    lm.suggestName(name)
    val mod = lm.module

    (mod.req, mod.access)
  }

  private[composer] val composer_response_io_ = if (outer.composerSystemParams.canIssueCoreCommands) {
    Some(IO(Flipped(Decoupled(new ComposerRoccResponse()))))
  } else None

  

  def composer_response_io[T <: ComposerUserResponse](gen: T = new ComposerUserResponse): DecoupledIO[T] = {
    val raw_io = composer_response_io_.getOrElse {
      throw new Exception("Attempted to get internal response IO but core was declared as not being able to issue core commands")
    }
    if (gen.isInstanceOf[ComposerRoccResponse]) {
      raw_io.asInstanceOf[DecoupledIO[T]]
    } else {
      val conv_module = Module(new ComposerRespConverter[T](gen))
      conv_module.in <> raw_io
      conv_module.out
    }
  }


  private val composer_command_io_ = if (outer.composerSystemParams.canIssueCoreCommands) {
    val node = outer.externalCoreCommNodes.get
    val mod = Module(new TLClientModule(node))
    node.out(0)._1 <> mod.tl
    val wire = Wire(Decoupled(new ComposerRoccCommand))
    wire.ready := mod.io.ready
    mod.io.valid := wire.valid
    mod.io.bits.dat := wire.bits.pack()
    mod.io.bits.addr := ComposerConsts.getInternalCmdRoutingAddress(wire.bits.inst.system_id)
    Some(wire)
  } else None

  def composer_command_io[T <: ComposerCommand](gen: T = new ComposerRoccCommand): DecoupledIO[T] = {
    val raw_io = composer_command_io_.get
    val multiBeatCommandMod = Module(new MultiBeatCommandEmitter(gen, outer.externalCoreCommNodes.get.out(0)._1))
    raw_io <> multiBeatCommandMod.out
    multiBeatCommandMod.in
  }

  def getSystemID(name: String): UInt = p(SystemName2IdMapKey)(name).U

  def addrBits: Int = log2Up(p(ExtMem).get.master.size)

  private object custom_usage extends Enumeration {
    //noinspection ScalaUnusedSymbol
    type custom_usage = Value
    val unused, default, custom = Value
  }

  private var using_custom = custom_usage.unused
  private[composer] val io_declaration = IO(Flipped(new ComposerCoreIO()))

  def ComposerIO[T1 <: ComposerCommand, T2 <: ComposerUserResponse](bundleIn: T1, bundleOut: T2): CustomIO[T1, T2] = {
    if (using_custom == custom_usage.default) {
      throw new Exception("Cannot use custom io after using the default io")
    }
    using_custom = custom_usage.custom
    val m = Module(new ComposerCommandBundler[T1, T2](bundleIn, bundleOut, composerConstructor.composerCoreWrapper))
    m.cio <> io_declaration
    m.io
  }

  def ComposerIO[T1 <: ComposerCommand](bundleIn: T1): CustomIO[T1, ComposerRoccUserResponse] = {
    ComposerIO[T1, ComposerRoccUserResponse](bundleIn, new ComposerRoccUserResponse)
  }


  def ComposerIO(): ComposerCoreIO = {
    if (using_custom == custom_usage.custom) {
      throw new Exception("Cannot use io after generating a custom io")
    }
    using_custom = custom_usage.default
    io_declaration
  }
}

class ComposerSystemIO extends Bundle {
  val cmd = Flipped(Decoupled(new ComposerRoccCommand))
  val resp = Decoupled(new ComposerRoccResponse())
}