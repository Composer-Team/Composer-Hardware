package composer.RoccHelpers

import chipsalliance.rocketchip.config._


import chisel3.util._
import chisel3._
import composer.AXILSlaveBeatBytes
import freechips.rocketchip.tile.RoCCResponse

class RoccToAXIL(implicit val p: Parameters) extends Module {

  val bus_bits = 32

  val io = IO(new Bundle {
    val out = Decoupled(UInt(bus_bits.W))
    val rocc = Flipped(Decoupled(new RoCCResponse))
  })

  val nBeats = 3 // (io.rocc.bits.getWidth.toFloat / bus_bits).ceil.toInt

  private def padTo(a: UInt, l: Int): UInt = {
    require(a.getWidth <= l, f"${a.getWidth}, $l")
    if (a.getWidth == l) a
    else Cat(0.U((l - a.getWidth).W), a)
  }

  val buffer = Reg(UInt(io.rocc.bits.data.getWidth.W))

  val rd = Reg(UInt(5.W))
  val wholePayload = padTo(Cat(padTo(rd, 32), buffer(31, 0), buffer(63, 32)), bus_bits * nBeats)
  val beats = VecInit((0 until nBeats) map { i =>
    wholePayload((i + 1) * bus_bits - 1, i * bus_bits)
  })
  val beatCounter = Reg(UInt(log2Up(nBeats).W))

  val sIdle :: sSend :: Nil = Enum(2)
  val state = RegInit(sIdle)
  io.out.valid := false.B
  io.out.bits := DontCare
  io.rocc.ready := (state === sIdle)
  switch(state) {
    is(sIdle) {
      when(io.rocc.fire) {
        buffer := io.rocc.bits.data
        rd := io.rocc.bits.rd
        state := sSend
        beatCounter := 0.U
      }
    }
    is(sSend) {
      io.out.valid := true.B
      io.out.bits := beats(beatCounter)
      when(io.out.fire) {
        when(beatCounter === (nBeats - 1).U) {
          state := sIdle
        }
        beatCounter := beatCounter + 1.U
      }
    }
  }
}
