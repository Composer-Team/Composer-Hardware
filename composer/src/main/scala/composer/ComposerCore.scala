package composer

import chisel3._
import chisel3.util._
import composer.MemoryStreams.{ChannelTransactionBundle, SequentialReader, SequentialWriter, TLCache, WriterDataChannelIO}
import freechips.rocketchip.util._
import freechips.rocketchip.config._
import composer.common._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.{CacheBlockBytes, ExtMem}
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

  val CacheNodes = coreParams.readChannelParams.filter(_.isInstanceOf[ComposerCachedReadChannelParams]).
    map {
      case b: ComposerCachedReadChannelParams => b
    }.map {
    param =>
      println("Making cache")
      val cache = TLCache(param.sizeBytes, param.idxMask).suggestName("TLCache_" + param.id)
      val req_xbar = TLXbar()
      val rnodes = Seq.tabulate(param.nChannels)(i => TLClientNode(Seq(TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          name = s"CachedReadChannel_sys${system_id}_core${core_id}_cache${param.id}_channel$i",
          supportsGet = TransferSizes(1, blockBytes),
          supportsProbe = TransferSizes(1, blockBytes)
        ))))))

      rnodes foreach (req_xbar := _)
      cache.mem_reqs := req_xbar
      (cache, rnodes, param)
  }

  val unCachedReaders = coreParams.readChannelParams.filter(_.isInstanceOf[ComposerUncachedChannelParams]).indices.map { rch =>
    TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = s"ReadChannel_sys${system_id}_core${core_id}_$rch",
        supportsGet = TransferSizes(1, blockBytes),
        supportsProbe = TransferSizes(1, blockBytes)
      )))))
  }

  val writers = coreParams.writeChannelParams.indices.map(wch =>
    TLClientNode(Seq(TLMasterPortParameters.v1(
      Seq(TLMasterParameters.v1(
        name = s"WriteChannel_sys${system_id}_core${core_id}_$wch",
        sourceId = IdRange(0, 1),
        supportsPutFull = TransferSizes(1, blockBytes),
        supportsProbe = TransferSizes(1, blockBytes)))))))

  lazy val module = composerSystemParams.buildCore(ComposerConstructor(composerSystemParams.coreParams, this), p)
}

class ComposerCore(val composerConstructor: ComposerConstructor)(implicit p: Parameters) extends
  LazyModuleImp(composerConstructor.composerCoreWrapper) {

  private val composerCoreParams = composerConstructor.composerCoreParams
  val io = IO(new ComposerCoreIO())

  var read_ios: Seq[(Int, DecoupledIO[ChannelTransactionBundle])] = Seq()
  var write_ios: Seq[(Int, DecoupledIO[ChannelTransactionBundle])] = Seq()

  // (id, is_reader)
  private val activeChannelIds: Seq[(Int, Bool)] = Seq()

  io.resp.bits.system_id := composerCoreParams.system_id.U
  io.resp.bits.core_id := composerCoreParams.core_id.U

  private var unnamedId = 0
  private var readChannelCheckIn = 0
  private var writeChannelCheckIn = 0
  private val cacheCheck = scala.collection.mutable.Map[Int, Int]()

  val cache_invalidate_ios = composerConstructor.composerCoreWrapper.CacheNodes.map(_._1.module.io_invalidate)

  private def exposeReaderModule(dataBytes: Int, vlen: Int,
                                 seqId: Option[Int],
                                 useCacheWithID: Option[Int],
                                 name: Option[String]):
  (DataChannelIO, DecoupledIO[ChannelTransactionBundle]) = {
    val (tl, edge) = {
      useCacheWithID match {
        case None =>
          val list = composerConstructor.composerCoreWrapper.unCachedReaders
          require(readChannelCheckIn < list.length, "This channel has been declared with an ID that is greater than the number of channels" +
            "declared in the config file. Please update your config file to reflect the number of channels you need.")
          val q = list(readChannelCheckIn).out(0)
          readChannelCheckIn = readChannelCheckIn + 1
          q
        case Some(cacheID) =>
          val outer = composerConstructor.composerCoreWrapper
          val channelID = cacheCheck.get(cacheID) match {
            case None =>
              cacheCheck.update(cacheID, 1)
              0
            case Some(cid) =>
              cacheCheck.update(cacheID, cid + 1)
              cid
          }
          require(channelID < outer.CacheNodes(cacheID)._3.nChannels, "This channel is oversubscribed. Increase the number of channels for this" +
            " cache in the config if you wish to assign more channels.")
          outer.CacheNodes(cacheID)._2(channelID).out(0)
      }
    }

    println(edge)
    val addressBits = log2Up(edge.manager.maxAddress)

    val m = Module(new SequentialReader(dataBytes, tl.params, edge, vlen))
    m.suggestName(name match {
      case Some(n) => n
      case None =>
        unnamedId = unnamedId + 1
        "Reader_unnamed_" + unnamedId
    })
    //noinspection DuplicatedCode
    seqId match {
      case Some(id) =>
        require(!activeChannelIds.contains((id, true)),
          "Error: It appears that you're re-declaring a sequential reader/writer under the same channel ID. " +
            "Use another channel ID.")
        val newio = IO(Flipped(Decoupled(new ChannelTransactionBundle(addressBits)))).suggestName(
          "Reader_seq_txbundle" + id
        )
        m.io.req <> newio
        read_ios = (Seq((id, newio)) ++ read_ios).sortBy(_._1)
      case None =>

    }

    tl <> m.tl
    (m.io.channel, m.io.req)
  }

  private def exposeWriterModule(dataBytes: Int,
                                 seqId: Option[Int],
                                 name: Option[String]): (WriterDataChannelIO, DecoupledIO[ChannelTransactionBundle]) = {
    val (tl, edge) = {
      val list = composerConstructor.composerCoreWrapper.writers
      require(writeChannelCheckIn < list.length, "This channel has been declared with an ID that is greater than the number of channels" +
        "declared in the config file. Please update your config file to reflect the number of channels you need.")
      val q = list(writeChannelCheckIn).out(0)
      writeChannelCheckIn = writeChannelCheckIn + 1
      q
    }
    val addressBits = log2Up(edge.manager.maxAddress)

    val m = Module(new SequentialWriter(dataBytes, tl.params, edge))
    m.suggestName(name match {
      case Some(n) => n
      case None =>
        unnamedId = unnamedId + 1
        "Writer_unnamed_" + unnamedId
    })
    //noinspection DuplicatedCode
    seqId match {
      case Some(id) =>
        require(!activeChannelIds.contains((id, false)),
          "Error: It appears that you're re-declaring a sequential reader/writer under the same channel ID. " +
            "Use another channel ID.")
        val newio = IO(Flipped(Decoupled(new ChannelTransactionBundle(addressBits)))).suggestName(
          "Writer_seq_txbundle" + id
        )
        m.io.req <> newio
        write_ios = (Seq((id, newio)) ++ write_ios).sortBy(_._1)
      case None =>
    }

    tl <> m.tl
    (m.io.channel, m.io.req)
  }

  def declareSequentialReader(usingReadChannelID: Int,
                              dataBytes: Int,
                              vLen: Int = 1,
                              useCacheWithID: Option[Int] = None,
                              name: Option[String] = None)(implicit p: Parameters): DataChannelIO = {
    exposeReaderModule(dataBytes, vLen, Some(usingReadChannelID), useCacheWithID, name)._1
  }

  def declareSequentialWriter(usingWriteChannelID: Int,
                              dataBytes: Int,
                              name: Option[String] = None)(implicit p: Parameters): WriterDataChannelIO = {
    exposeWriterModule(dataBytes, Some(usingWriteChannelID), name)._1
  }

  def declareSparseReader(dataBytes: Int, vLen: Int = 1,
                          useCacheWithID: Option[Int] = None,
                          name: Option[String] = None)(implicit p: Parameters):
  (DataChannelIO, DecoupledIO[ChannelTransactionBundle]) = {
    exposeReaderModule(dataBytes, vLen, None, useCacheWithID, name)
  }

  def declareSparseWriter(dataBytes: Int,
                          name: Option[String] = None)(implicit p: Parameters):
  (WriterDataChannelIO, DecoupledIO[ChannelTransactionBundle]) = {
    exposeWriterModule(dataBytes, None, name)
  }

}

class ComposerSystemIO(implicit p: Parameters) extends Bundle {
  val cmd = Flipped(Decoupled(new ComposerRoccCommand))
  val resp = Decoupled(new ComposerRoccResponse())
  val busy = Output(Bool())
}