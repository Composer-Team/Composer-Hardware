package composer

import chisel3._
import chisel3.util._
import composer.MemoryStreams.{ChannelTransactionBundle, FixedSequentialWriteChannel, SequentialReader}
import freechips.rocketchip.util._
import freechips.rocketchip.config.{Field, Parameters}
import composer.common._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tilelink._

class ComposerCoreIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val req = Flipped(DecoupledIO(new ComposerRoccCommand))
  val resp = DecoupledIO(new ComposerRoccResponse)
  val busy = Output(Bool())
}

class DataChannelIO(maxBytes: Int) extends Bundle {
  val data = Decoupled(UInt((maxBytes * 8).W))
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

  private def exposeRWModule(read: Boolean, connectToAddrFile: Boolean, id: Int, maxBytes: Int): (DataChannelIO, DecoupledIO[ChannelTransactionBundle]) = {
    require(!activeChannelIds.contains((id, read)),
      "Error: It appears that you're re-declaring a sequential reader/writer under the same channel ID. " +
        "Use another channel ID.")
    val (tl, edge) = {
      val list = if (read) composerConstructor.composerCoreWrapper.readers
      else composerConstructor.composerCoreWrapper.writers
      require(id < list.length, "This channel has been declared with an ID that is greater than the number of channels" +
        "declared in the config file. Please update your config file to reflect the number of channels you need.")
      list(id).out(0)
    }
    val addressBits = log2Up(edge.manager.maxAddress)

    val ioname = s"${if (read) "reader" else "writer"}_io_$id"
    val newio = if (connectToAddrFile)
      Some(IO(Flipped(Decoupled(new ChannelTransactionBundle(addressBits)))).suggestName(ioname))
    else None
    val (modtl, modchannel, modreq) = if (read) {
      val m = Module(new SequentialReader(maxBytes, tl.params, edge))
      //noinspection DuplicatedCode
      if (connectToAddrFile) {
        m.io.req <> newio.get
        read_ios = (Seq((id, newio.get)) ++ read_ios).sortBy(_._1)
      }
      (m.tl, m.io.channel, m.io.req)
    } else {
      val m = Module(new FixedSequentialWriteChannel(maxBytes, tl.params, edge))
      //noinspection DuplicatedCode
      if (connectToAddrFile) {
        m.io.req <> newio.get
        write_ios = (Seq((id, newio.get)) ++ write_ios).sortBy(_._1)
      }
      (m.tl, m.io.channel, m.io.req)
    }
    tl <> modtl
    (modchannel, modreq)
  }

  def declareSequentialReader(usingReadChannelID: Int, maxBytes: Int)(implicit p: Parameters): DataChannelIO = {
    exposeRWModule(read = true, connectToAddrFile = true, usingReadChannelID, maxBytes)._1
  }

  def declareSequentialWriter(usingWriteChannelID: Int, maxBytes: Int)(implicit p: Parameters): DataChannelIO = {
    exposeRWModule(read = false, connectToAddrFile = true, usingWriteChannelID, maxBytes)._1
  }

  def declareSparseReader(usingReadChannelID: Int, maxBytes: Int)(implicit p: Parameters):
  (DataChannelIO, DecoupledIO[ChannelTransactionBundle]) = {
    exposeRWModule(read = true, connectToAddrFile = false, usingReadChannelID, maxBytes)
  }

  def declareSparseWriter(usingWriteChannelID: Int, maxBytes: Int)(implicit p: Parameters):
  (DataChannelIO, DecoupledIO[ChannelTransactionBundle]) = {
    exposeRWModule(read = false, connectToAddrFile = false, usingWriteChannelID, maxBytes)
  }

}

class ComposerSystemIO(implicit p: Parameters) extends Bundle {
  val cmd = Flipped(Decoupled(new ComposerRoccCommand))
  val resp = Decoupled(new ComposerRoccResponse())
  val busy = Output(Bool())
}