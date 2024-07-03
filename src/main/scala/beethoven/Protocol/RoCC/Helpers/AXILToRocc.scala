package beethoven.Protocol.RoCC.Helpers

import chisel3._
import chisel3.util._
import beethoven.common.AccelRoccCommand
import chipsalliance.rocketchip.config._
import freechips.rocketchip.rocket.MStatus
import freechips.rocketchip.tile._
import chisel3._
import chisel3.util._
import beethoven.Parameters.CmdRespBusWidthBytes

//Takes nasti bits and converts to rocc

class AXILToRocc(implicit val p: Parameters) extends Module {

  val bus_bits = p(CmdRespBusWidthBytes) * 8

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(bus_bits.W)))
    val rocc = Decoupled(new AccelRoccCommand)
  })

  //this should eventually made parameterizable to the nasti width, but the nasti width is currently way wider
  //than the data width we get
  val nBeats = 5 // ((5 * 32).toFloat / bus_bits).ceil.toInt
  val counter = RegInit(0.U((1 + log2Up(nBeats)).W))
  val bitsBuffer = Reg(Vec(nBeats, UInt(bus_bits.W)))

  val rocc = Wire(new AccelRoccCommand)

  rocc := AccelRoccCommand.fromUInt(bitsBuffer)
  io.rocc.bits := rocc
  io.in.ready := (counter < nBeats.U) && io.rocc.ready
  io.rocc.valid := (counter === nBeats.U)
  when(io.in.fire) {
    bitsBuffer(counter) := (io.in.bits)(31, 0)
    counter := counter + 1.U
  }

  when(io.rocc.fire) {
    counter := 0.U
  }
}