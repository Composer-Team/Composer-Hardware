package beethoven.MemoryStreams.Loaders

import chipsalliance.rocketchip.config._


import chisel3._
import chisel3.util._

abstract class CScratchpadLoader(datOutWidth: Int, idxWidth: Int, beatSizeBits: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle() {
    val cache_block_in = Flipped(Decoupled(new Bundle() {
      val dat = UInt((beatSizeBits).W)
      val len = UInt((log2Up(beatSizeBits / 8) + 1).W)
      val idxBase = UInt(idxWidth.W)
    }))
    val sp_write_out = Decoupled(new Bundle() {
      val dat = UInt(datOutWidth.W)
      val idx = UInt(idxWidth.W)
    })
  })
  val spEntriesPerBeat: Int
}

