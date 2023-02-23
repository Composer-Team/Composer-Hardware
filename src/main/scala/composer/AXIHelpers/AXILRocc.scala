package composer.AXIHelpers

import chisel3._
import chisel3.util._
import composer.{AXILSlaveBeatBytes, CmdRespBusWidthBytes}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.MStatus
import freechips.rocketchip.tile._

//Takes nasti bits and converts to rocc

class AXILRoccConverter(implicit val p: Parameters) extends Module {

  val bus_bits = p(CmdRespBusWidthBytes) * 8

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(bus_bits.W)))
    val rocc = Decoupled(new RoCCCommand)
  })

  //this should eventually made parameterizable to the nasti width, but the nasti width is currently way wider
  //than the data width we get
  val nBeats = ((5 * 32).toFloat / bus_bits).ceil.toInt
  val counter = RegInit(0.U((1 + log2Up(nBeats)).W))
  val bitsBuffer = Reg(Vec(nBeats, UInt(bus_bits.W)))

  val rocc = Wire(new RoCCCommand)
  val bitsCat = Cat(bitsBuffer)

  rocc.inst := bitsCat(31, 0).asTypeOf(new RoCCInstruction)
  rocc.rs1 := bitsCat(95, 32)
  rocc.rs2 := bitsCat(159, 96)
  rocc.status := 0.U.asTypeOf(new MStatus)
  io.rocc.bits <> rocc
  io.in.ready := (counter < nBeats.U) && io.rocc.ready
  io.rocc.valid := (counter === nBeats.U)
  when(io.in.fire) {
    bitsBuffer(counter) := io.in.bits
    counter := counter + 1.U
  }

  when(io.rocc.fire) {
    counter := 0.U
  }
}

class RoccAXILConverter(implicit val p: Parameters) extends Module {

  val bus_bits = p(AXILSlaveBeatBytes) * 8

  val io = IO(new Bundle {
    val out = Decoupled(UInt(bus_bits.W))
    val rocc = Flipped(Decoupled(new RoCCResponse))
  })

  val nBeats = (io.rocc.bits.getWidth.toFloat / bus_bits).ceil.toInt


  def padTo(a: UInt, l: Int): UInt = {
    require(a.getWidth <= l, f"${a.getWidth}, $l")
    if (a.getWidth == l) a
    else Cat(0.U((l - a.getWidth).W), a)
  }

  val buffer = Reg(UInt(io.rocc.bits.data.getWidth.W))

  val rd = Reg(UInt(5.W))
  val wholePayload = padTo(Cat(padTo(buffer, 64), padTo(rd, 32)), bus_bits * nBeats)
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
        beatCounter := 0.U
      }
    }
  }

}
