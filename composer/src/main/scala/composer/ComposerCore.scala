package composer

import chisel3._
import chisel3.util._
import composer.MemoryStreams._
import freechips.rocketchip.util._
import freechips.rocketchip.config._
import composer.common._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._

class ComposerCoreIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val req = Flipped(DecoupledIO(new ComposerRoccCommand))
  val resp = DecoupledIO(new ComposerRoccResponse)
  val busy = Output(Bool())
}

class DataChannelIO(dataBytes: Int, vlen: Int = 1) extends Bundle {
  val data = Decoupled(Vec(vlen, UInt((dataBytes * 8).W)))
  val stop = Input(Bool())
  val finished = Output(Bool())
}

class ComposerCoreWrapper(val composerSystemParams: ComposerSystemParams, core_id: Int, system_id: Int)(implicit p: Parameters) extends LazyModule {
  val coreParams = composerSystemParams.coreParams.copy(core_id = core_id, system_id = system_id)
  val blockBytes = p(CacheBlockBytes)

  val CacheNodes = coreParams.memoryChannelParams.filter(_.channelType == CChannelType.CacheChannel) map (_.asInstanceOf[CCachedReadChannelParams]) map {
    par =>
      val param = par.cacheParams
      val cache = TLCache(param.sizeBytes,
        nClients = par.nChannels,
        associativity = param.associativity).suggestName("TLCache_" + param.id)
      val req_xbar = TLXbar()
      val rnodes = List.tabulate(par.nChannels)(i => TLClientNode(List(TLMasterPortParameters.v1(
        clients = List(TLMasterParameters.v1(
          name = s"CachedReadChannel_sys${system_id}_core${core_id}_${par.name}$i",
          supportsGet = TransferSizes(1, blockBytes),
          supportsProbe = TransferSizes(1, blockBytes)
        ))))))

      rnodes foreach (req_xbar := _)
      cache.mem_reqs := TLBuffer() := TLWidthWidget(blockBytes) := TLBuffer() := req_xbar
      (par.name, (cache, rnodes))
  }

  val unCachedReaders = coreParams.memoryChannelParams.filter(_.channelType == CChannelType.ReadChannel).map { para =>
    (para.name, List.tabulate(para.nChannels) { i =>
      TLClientNode(List(TLMasterPortParameters.v1(
        clients = List(TLMasterParameters.v1(
          name = s"ReadChannel_sys${system_id}_core${core_id}_${para.name}$i",
          supportsGet = TransferSizes(1, blockBytes),
          supportsProbe = TransferSizes(1, blockBytes)
        )))))
    })
  }

  val writers = coreParams.memoryChannelParams.filter(_.channelType == CChannelType.WriteChannel).map(para =>
    (para.name, List.tabulate(para.nChannels) { i =>
      TLClientNode(List(TLMasterPortParameters.v1(
        List(TLMasterParameters.v1(
          name = s"WriteChannel_sys${system_id}_core${core_id}_${para.name}$i",
          sourceId = IdRange(0, 4),
          supportsPutFull = TransferSizes(1, blockBytes),
          supportsPutPartial = TransferSizes(1, blockBytes),
          supportsProbe = TransferSizes(1, blockBytes))))))
    }))

  val scratch_mod = coreParams.memoryChannelParams.filter(_.channelType == CChannelType.Scratchpad).map(_.asInstanceOf[CScratchpadChannelParams]).map {
    param =>
      lazy val mod = LazyModule(param.make)
      (param.name, mod)
  }

  val readerNodes = unCachedReaders ++ CacheNodes.map(i => (i._1, i._2._2))

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

  private val composerCoreParams = composerConstructor.composerCoreParams
  private val outer = composerConstructor.composerCoreWrapper
  val io = IO(new ComposerCoreIO())

  var read_ios: List[(String, DecoupledIO[ChannelTransactionBundle])] = List()
  var write_ios: List[(String, DecoupledIO[ChannelTransactionBundle])] = List()

  io.resp.bits.system_id := composerCoreParams.system_id.U
  io.resp.bits.core_id := composerCoreParams.core_id.U

  val cache_invalidate_ios = composerConstructor.composerCoreWrapper.CacheNodes.map(_._2._1.module.io_invalidate)

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
  def getSparseReaderModules(name: String,
                             dataBytes: Int,
                             vlen: Int,
                             prefetchRows: Int = 0,
                             idx: Option[Int] = None): (List[DecoupledIO[ChannelTransactionBundle]], List[DataChannelIO]) = {
    (List(), List())
    val mod = idx match {
      case Some(id_unpack) =>
        val clients = getTLClients(name, outer.readerNodes)
        List(Module(new SequentialReader(dataBytes, vlen, prefetchRows, clients(id_unpack))))
      case None => getTLClients(name, outer.readerNodes).map(tab_id => Module(new SequentialReader(dataBytes, vlen,
        prefetchRows, tab_id)))
    }
    mod foreach {m =>
      m.tl_out <> m.tl_outer
      m.suggestName(name)
    }
    (mod.map(_.io.req), mod.map(_.io.channel))
  }


  /**
    * See @getSparseReaderModules for description
    */
  def getSequentialReaderModules(name: String,
                                 dataBytes: Int,
                                 vlen: Int,
                                 prefetchRows: Int = 0,
                                 idx: Option[Int] = None): List[DataChannelIO] = {
    val mods = getSparseReaderModules(name, dataBytes, vlen, prefetchRows, idx)
    mods._1.zipWithIndex foreach { case (reqio, m_id) =>
      val chosen_name = s"Reader_$name" + (idx match {
        case Some(real_id) => real_id
        case None => m_id
      })
      val newio = IO(Flipped(Decoupled(new ChannelTransactionBundle))).suggestName(chosen_name)
      newio <> reqio
      read_ios = (List((name, newio)) ++ read_ios).sortBy(_._1)
    }
    mods._2
  }

  def getSparseWriterModules(name: String,
                             dataBytes: Int,
                             idx: Option[Int] = None): (List[DecoupledIO[ChannelTransactionBundle]], List[WriterDataChannelIO]) = {
    val mod = idx match {
      case Some(id) => List(Module(new SequentialWriter(dataBytes, getTLClients(name, outer.writers)(id))))
      case None => getTLClients(name, outer.writers).map(tab_id => Module(new SequentialWriter(dataBytes, tab_id)))
    }
    mod foreach { m =>
      m.tl_out <> m.tl_outer
      m.suggestName(name)
    }

    (mod.map(_.io.req), mod.map(_.io.channel))
  }

  def getSequentialWriterModules(name: String,
                                 dataBytes: Int,
                                 idx: Option[Int] = None): List[WriterDataChannelIO] = {
    val mods = getSparseWriterModules(name, dataBytes, idx)
    mods._1.zipWithIndex.foreach { case (reqio, m_id) =>
      val chosen_name = s"Writer_$name" + (idx match {
        case Some(real_id) => real_id
        case None => m_id
      })
      val newio = IO(Flipped(Decoupled(new ChannelTransactionBundle))).suggestName(chosen_name)
      newio <> reqio
      write_ios = (List((name, newio)) ++ write_ios).sortBy(_._1)
    }
    mods._2
  }

  def getScratchpad(name: String): (DecoupledIO[CScratchpadInitReq], CScratchpadAccessBundle, Bool) = {
    val outer = composerConstructor.composerCoreWrapper
    val lm = outer.scratch_mod.filter(_._1 == name)(0)._2
    lm.suggestName(name)
    val mod = lm.module

    (mod.scratchpad_req_io, mod.scratchpad_access_io, mod.scratchpad_req_idle_io)
  }

}

class ComposerSystemIO(implicit p: Parameters) extends Bundle {
  val cmd = Flipped(Decoupled(new ComposerRoccCommand))
  val resp = Decoupled(new ComposerRoccResponse())
  val busy = Output(Bool())
}