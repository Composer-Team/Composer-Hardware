package composer.RoccHelpers

import chisel3._
import chisel3.util._
import composer.{FrontBusBeatBytes, CmdRespBusWidthBytes}
import chipsalliance.rocketchip.config._

import freechips.rocketchip.rocket.MStatus
import freechips.rocketchip.tile._

//Takes nasti bits and converts to rocc

class AXILToRocc(implicit val p: Parameters) extends Module {

  val bus_bits = p(CmdRespBusWidthBytes) * 8

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(bus_bits.W)))
    val rocc = Decoupled(new RoCCCommand)
  })

  //this should eventually made parameterizable to the nasti width, but the nasti width is currently way wider
  //than the data width we get
  val nBeats = 5 // ((5 * 32).toFloat / bus_bits).ceil.toInt
  val counter = RegInit(0.U((1 + log2Up(nBeats)).W))
  val bitsBuffer = Reg(Vec(nBeats, UInt(bus_bits.W)))

  val rocc = Wire(new RoCCCommand)

  rocc.inst := bitsBuffer(0).asTypeOf(new RoCCInstruction)
  rocc.rs1 := Cat(bitsBuffer(1), bitsBuffer(2))
  rocc.rs2 := Cat(bitsBuffer(3), bitsBuffer(4))
  rocc.status := 0.U.asTypeOf(new MStatus)
  io.rocc.bits <> rocc
  io.in.ready := (counter < nBeats.U) && io.rocc.ready
  io.rocc.valid := (counter === nBeats.U)
  when(io.in.fire) {
    bitsBuffer(counter) := io.in.bits(31, 0)
    counter := counter + 1.U
  }

  when(io.rocc.fire) {
    counter := 0.U
  }
}

