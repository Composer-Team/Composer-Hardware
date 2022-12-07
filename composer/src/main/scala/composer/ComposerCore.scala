package composer

import chisel3._
import chisel3.util._
import composer.MemoryStreams.{ChannelTransactionBundle, SequentialReader, SequentialWriter, WriterDataChannelIO}
import freechips.rocketchip.util._
import freechips.rocketchip.config._
import composer.common._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.CacheBlockBytes
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
  val readers = coreParams.readChannelParams.indices.map { rch =>
    TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = s"ReadChannel_sys${system_id}_core${core_id}_$rch",
        sourceId = IdRange(0, 1),
        supportsProbe = TransferSizes(1, blockBytes),
        supportsGet = TransferSizes(1, blockBytes)
      )))))
  }

  val writers = coreParams.writeChannelParams.indices.map(wch =>
    TLClientNode(Seq(TLMasterPortParameters.v1(
      Seq(TLMasterParameters.v1(
        name = s"WriteChannel_sys${system_id}_core${core_id}_$wch",
        sourceId = IdRange(0, p(MaxMemTxsKey)),
        supportsProbe = TransferSizes(1, blockBytes),
        supportsPutFull = TransferSizes(1, blockBytes)
      )))))
  )
  lazy val module = composerSystemParams.buildCore(ComposerConstructor(composerSystemParams.coreParams, this), p)
}

class ComposerCore(val composerConstructor: ComposerConstructor)(implicit p: Parameters) extends
  LazyModuleImp(composerConstructor.composerCoreWrapper) {

  val composerCoreParams = composerConstructor.composerCoreParams
  val io = IO(new ComposerCoreIO())

  var read_ios: Seq[(Int, DecoupledIO[ChannelTransactionBundle])] = Seq()
  var write_ios: Seq[(Int, DecoupledIO[ChannelTransactionBundle])] = Seq()

  // (id, is_reader)
  var activeChannelIds: Seq[(Int, Bool)] = Seq()

  io.resp.bits.system_id := composerCoreParams.system_id.U
  io.resp.bits.core_id := composerCoreParams.core_id.U

  private def exposeReaderModule(connectToAddrFile: Boolean, id: Int, dataBytes: Int, vlen: Int): (DataChannelIO, DecoupledIO[ChannelTransactionBundle]) = {
    require(!activeChannelIds.contains((id, true)),
      "Error: It appears that you're re-declaring a sequential reader/writer under the same channel ID. " +
        "Use another channel ID.")
    val (tl, edge) = {
      val list = composerConstructor.composerCoreWrapper.readers
      require(id < list.length, "This channel has been declared with an ID that is greater than the number of channels" +
        "declared in the config file. Please update your config file to reflect the number of channels you need.")
      list(id).out(0)
    }
    val addressBits = log2Up(edge.manager.maxAddress)

    val ioname = s"reader_io_$id"
    val newio = if (connectToAddrFile)
      Some(IO(Flipped(Decoupled(new ChannelTransactionBundle(addressBits)))).suggestName(ioname))
    else None
    val m = Module(new SequentialReader(dataBytes, tl.params, edge, vlen))
    m.suggestName("Reader_" + (if (connectToAddrFile) "contig_" else "sparse_") + "id_" + id)
    //noinspection DuplicatedCode
    if (connectToAddrFile) {
      m.io.req <> newio.get
      read_ios = (Seq((id, newio.get)) ++ read_ios).sortBy(_._1)
    }

    tl <> m.tl
    (m.io.channel, m.io.req)
  }

  private def exposeWriterModule(connectToAddrFile: Boolean, id: Int, dataBytes: Int, vlen: Int): (WriterDataChannelIO, DecoupledIO[ChannelTransactionBundle]) = {
    require(!activeChannelIds.contains((id, false)),
      "Error: It appears that you're re-declaring a sequential reader/writer under the same channel ID. " +
        "Use another channel ID.")
    val (tl, edge) = {
      val list = composerConstructor.composerCoreWrapper.writers
      require(id < list.length, "This channel has been declared with an ID that is greater than the number of channels" +
        "declared in the config file. Please update your config file to reflect the number of channels you need.")
      list(id).out(0)
    }
    val addressBits = log2Up(edge.manager.maxAddress)

    val ioname = s"writer_io_$id"
    val newio = if (connectToAddrFile)
      Some(IO(Flipped(Decoupled(new ChannelTransactionBundle(addressBits)))).suggestName(ioname))
    else None
    val m = Module(new SequentialWriter(dataBytes * vlen, tl.params, edge))
    //noinspection DuplicatedCode
    if (connectToAddrFile) {
      m.io.req <> newio.get
      write_ios = (Seq((id, newio.get)) ++ write_ios).sortBy(_._1)
    }
    (m.tl, m.io.channel, m.io.req)
    tl <> m.tl
    (m.io.channel, m.io.req)
  }

  def declareSequentialReader(usingReadChannelID: Int, dataBytes: Int, vLen: Int = 1)(implicit p: Parameters): DataChannelIO = {
    exposeReaderModule(connectToAddrFile = true, usingReadChannelID, dataBytes, vLen)._1
  }

  def declareSequentialWriter(usingWriteChannelID: Int, dataBytes: Int)(implicit p: Parameters): WriterDataChannelIO = {
    exposeWriterModule( connectToAddrFile = true, usingWriteChannelID, dataBytes, 1)._1
  }

  def declareSparseReader(usingReadChannelID: Int, dataBytes: Int, vLen: Int = 1)(implicit p: Parameters):
  (DataChannelIO, DecoupledIO[ChannelTransactionBundle]) = {
    exposeReaderModule(connectToAddrFile = false, usingReadChannelID, dataBytes, vLen)
  }

  def declareSparseWriter(usingWriteChannelID: Int, dataBytes: Int)(implicit p: Parameters):
  (WriterDataChannelIO, DecoupledIO[ChannelTransactionBundle]) = {
    exposeWriterModule(connectToAddrFile = false, usingWriteChannelID, dataBytes, 1)
  }

}

class ComposerSystemIO(implicit p: Parameters) extends Bundle {
  val cmd = Flipped(Decoupled(new ComposerRoccCommand))
  val resp = Decoupled(new ComposerRoccResponse())
  val busy = Output(Bool())
}