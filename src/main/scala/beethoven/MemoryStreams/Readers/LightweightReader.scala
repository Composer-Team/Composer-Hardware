package beethoven.MemoryStreams.Readers

import beethoven.common.CLog2Up
import beethoven.{BeethovenBuild, platform}
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
class LightweightReader(val dWidth: Int,
                        val tl_bundle: TLBundle,
                        tl_edge: TLEdgeOut,
                        minSizeBytes: Option[Int] = None)(implicit p: Parameters) extends Module with ReaderModuleIO {
  override val desiredName = "LightReader_w" + dWidth.toString
  val beatBytes = tl_edge.manager.beatBytes
  val beatBits = beatBytes * 8
  val addressBits = log2Up(tl_edge.manager.maxAddress)
  val maxBytes = dWidth / 8
  val largeTxNBeats = Math.max(platform.prefetchSourceMultiplicity, maxBytes / beatBytes)
  val nSources = tl_edge.client.endSourceId
  val prefetchRows = Math.max(
    nSources * platform.prefetchSourceMultiplicity,
    minSizeBytes.getOrElse(0) / beatBytes)
  val rowsAvailableToAlloc = RegInit(prefetchRows.U(log2Up(prefetchRows + 1).W))
  require(isPow2(maxBytes))


  // io goes to user, TL connects with AXI4
  val io = IO(new ReadChannelIO(dWidth))
  val tl_out = IO(new TLBundle(tl_bundle.params))
  val tl_reg = Module(new Queue(new TLBundleA(tl_out.params), 2, false, false, false))
  tl_out.a <> tl_reg.io.deq

  val storedDataWidthBytes = Math.max(beatBytes, dWidth / 8)
  val storedDataWidth = storedDataWidthBytes * 8
  val channelsPerStorage = storedDataWidthBytes / maxBytes

  val addr = Reg(UInt(addressBits.W))
  val len = RegInit(0.U(addressBits.W))

  val data_channel_read_idx = RegInit(0.U((log2Up(channelsPerStorage) + 1).W))

  val s_idle :: s_send_mem_request :: s_read_memory :: Nil = Enum(3)
  val state = RegInit(s_idle)

  tl_reg.io.enq.valid := false.B
  tl_reg.io.enq.bits := DontCare

  // has to be pow2 to ensure OHToUInt works like we want

  assert(nSources == 1, "Lightweight reader should only support a single source.")
  assert(dWidth >= beatBits)
  val sourceBusy = RegInit(false.B)
  io.channel.in_progress := state =/= s_idle

  val largestRead = beatBytes * largeTxNBeats
  val lgLargestRead = log2Up(largestRead)

  val beatsPerDat = dWidth / beatBits
  val storage = Reg(Vec(beatsPerDat, UInt(beatBits.W)))
  val storageFill = Reg(UInt(log2Up(beatsPerDat).W))
  val storageFilled = RegInit(false.B)
  val beatsLeft = Reg(UInt(log2Up(beatsPerDat+1).W))
  io.channel.data.valid := storageFilled
  io.channel.data.bits := Cat(storage.reverse)

  tl_out.d.ready := !storageFilled
  when (tl_out.d.fire) {
    storage(storageFill) := tl_out.d.bits.data
    storageFill := storageFill + 1.U
    beatsLeft := beatsLeft - 1.U
    when (beatsLeft === 1.U) {
      beatsLeft := beatsPerDat.U
      storageFilled := true.B
      storageFill := 0.U
    }
  }

  io.req.ready := false.B

  switch(state) {
    is(s_idle) {
      io.req.ready := !sourceBusy
      when(io.req.fire) {
        addr := io.req.bits.addr.address
        len := io.req.bits.len
        assert(io.req.bits.len(log2Up(beatBytes) - 1, 0) === 0.U,
          f"The provided length is not aligned to the data bus size. Please align to $beatBytes.\n" +
            f"Just make sure that you always flush through the data when you're done to make the reader usable again.")
        state := s_send_mem_request
        when(io.req.bits.len === 0.U) {
          state := s_idle
        }
      }
    }
    is(s_send_mem_request) {
      when(len < largestRead.U) {
        tl_reg.io.enq.valid := !sourceBusy
        tl_reg.io.enq.bits := tl_edge.Get(
          fromSource = 0.U,
          toAddress = addr,
          lgSize = CLog2Up(beatBytes).U
        )._2
        when(tl_reg.io.enq.fire) {
          storageFill := 0.U
          addr := addr + beatBytes.U
          len := len - beatBytes.U
          sourceBusy := true.B
        }
      }.otherwise {
        tl_reg.io.enq.valid := !sourceBusy
        tl_reg.io.enq.bits := tl_edge.Get(
          fromSource = 0.U,
          toAddress = addr,
          lgSize = lgLargestRead.U
        )._2
        when(tl_reg.io.enq.fire) {
          addr := addr + largestRead.U
          len := len - largestRead.U
          storageFill := 0.U
          sourceBusy := true.B
        }
      }

      when(tl_reg.io.enq.fire) {
        state := s_read_memory
        beatsLeft := beatsPerDat.U
      }
    }
    // wait for responses from s_send_mem_request
    is(s_read_memory) {
      // when we get a message back store it into a buffer
      when(len === 0.U) {
        state := s_idle
      }.otherwise {
        state := s_send_mem_request
      }
    }
  }
}
