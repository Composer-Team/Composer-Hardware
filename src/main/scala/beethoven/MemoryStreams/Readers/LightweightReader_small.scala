package beethoven.MemoryStreams.Readers

import beethoven.BeethovenBuild
import beethoven.common.{CLog2Up, splitIntoChunks}
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{TLBundle, TLBundleA, TLEdgeOut}

/**
 * All user-facing memory endpoints need to guarantee storage. This reader implementation does not guarantee storage
 * and relies on the endpoint to ensure storage (and therefore deadlock prevention)
 *
 * This implementation is specifically meant for ultra-lightweight implementations
 */
class LightweightReader_small(val dWidth: Int,
                              val tl_bundle: TLBundle,
                              tl_edge: TLEdgeOut,
                              sp_sz_bytes: Int)(implicit p: Parameters) extends Module with ReaderModuleIO{
  override val desiredName = "LightReader_w" + dWidth.toString
  BeethovenBuild.requestModulePartition(this.desiredName)
  val fabricBytes = tl_edge.manager.beatBytes
  val fabricBits = fabricBytes * 8
  val addressBits = log2Up(tl_edge.manager.maxAddress)
  val channelBytes = dWidth / 8
  // Because we're instantiating this module, we already know `largestNecessaryPrefetch <= platform prefetchSourceMultiplicity`
  // This module will emit one transaction of this length (assumption is
  val largestNecessaryPrefetch = {
    // TL only supports pow2-sized transactions
    val nonp2 = (sp_sz_bytes.toFloat / fabricBytes).ceil.toInt
    val p2 = 1 << log2Ceil(nonp2)
    p2
  }
  require(tl_edge.client.endSourceId == 1)
  require(isPow2(channelBytes))

  // io goes to user, TL connects with AXI4
  val io = IO(new ReadChannelIO(dWidth))
  val tl_out = IO(new TLBundle(tl_bundle.params))
  val tl_reg = Module(new Queue(new TLBundleA(tl_out.params), 2, false, false, false))
  tl_out.a <> tl_reg.io.deq

  val channelBeatsToPerform = RegInit(0.U(log2Up(largestNecessaryPrefetch * fabricBytes / channelBytes + 1).W))
  val beatPerFabricDat = fabricBytes / channelBytes
  val fabricBeatCtr = Reg(UInt(log2Up(beatPerFabricDat).W))

  tl_reg.io.enq.valid := false.B
  tl_reg.io.enq.bits := DontCare

  // has to be pow2 to ensure OHToUInt works like we want

  assert(dWidth <= fabricBits)
  io.channel.in_progress := channelBeatsToPerform =/= 0.U

  require(channelBytes < fabricBytes, "Channel width must be less than fabric width")
  io.channel.data.valid := tl_out.d.valid
  val data_split = splitIntoChunks(tl_out.d.bits.data, channelBytes * 8)
  io.channel.data.bits := data_split(fabricBeatCtr)
  val drain_fabric = channelBeatsToPerform === 0.U
  tl_out.d.ready := fabricBeatCtr === (beatPerFabricDat - 1).U || drain_fabric

  when(!drain_fabric && io.channel.data.fire) {
    channelBeatsToPerform := channelBeatsToPerform - 1.U
  }

  io.req.ready := tl_reg.io.enq.ready && drain_fabric
  tl_reg.io.enq.valid := io.req.valid && drain_fabric
  tl_reg.io.enq.bits := tl_edge.Get(
    fromSource = 0.U,
    toAddress = io.req.bits.addr,
    lgSize = log2Up(largestNecessaryPrefetch * fabricBytes).U
  )._2
  when(tl_out.d.fire) {
    fabricBeatCtr := 0.U
  }.elsewhen(io.channel.data.fire) {
    fabricBeatCtr := fabricBeatCtr + 1.U
  }.elsewhen(io.req.fire) {
    fabricBeatCtr := 0.U
    channelBeatsToPerform := io.req.bits.len >> CLog2Up(channelBytes)
  }
}
