package composer.MemoryStreams.Loaders

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._

class CScratchpadPackedSubwordLoader(datOutWidth: Int, idxWidth: Int, wordSizeBits: Int, datsPerSubword: Int,
                                     val beatSize: Int)(implicit p: Parameters)
  extends CScratchpadLoader(datOutWidth, idxWidth, beatSize) {
  require(beatSize * 8 % wordSizeBits == 0)
  val subwordCounter = Counter(beatSize)
  val datCounter = Counter(datsPerSubword)
  override val spEntriesPerBeat: Int = (beatSize * 8) / wordSizeBits

  val beat = Reg(UInt((beatSize * 8).W))
  val idxBase = Reg(UInt(idxWidth.W))
  val lenRemainingFromReq = Reg(UInt((log2Up(beatSize)+1).W))

  val s_idle :: s_loading :: Nil = Enum(2)
  val state = RegInit(s_idle)
  io.cache_block_in.ready := state === s_idle
  io.sp_write_out.bits := DontCare
  io.sp_write_out.valid := false.B

  val datSelection = VecInit((0 until datsPerSubword) map { sw_idx =>
    val start = sw_idx * datOutWidth
    val end = (sw_idx + 1) * datOutWidth - 1
    beat(end, start)
  })

  switch(state) {
    is(s_idle) {
      when(io.cache_block_in.fire) {
        state := s_loading
        beat := io.cache_block_in.bits.dat
        idxBase := io.cache_block_in.bits.idxBase
        lenRemainingFromReq := io.cache_block_in.bits.len
        datCounter.reset()
        subwordCounter.reset()
      }
    }

    is(s_loading) {
      io.sp_write_out.valid := true.B
      io.sp_write_out.bits.dat := datSelection(datCounter.value)
      io.sp_write_out.bits.idx := idxBase
      when(io.sp_write_out.fire) {
        datCounter.inc()
        idxBase := idxBase + 1.U
        when(datCounter.value === (datsPerSubword - 1).U) {
          subwordCounter.inc()
          lenRemainingFromReq := lenRemainingFromReq - (wordSizeBits/8).U
          when(lenRemainingFromReq === (wordSizeBits/8).U) {
            state := s_idle
          }
          beat := beat >> wordSizeBits

        }
      }
    }
  }
}
