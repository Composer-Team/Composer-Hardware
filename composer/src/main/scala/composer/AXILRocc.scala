package composer

import Chisel._
//import chisel3._
//import chisel3.util._

import freechips.rocketchip.tile.{RoCCCommand, RoCCResponse, RoCCInstruction}
import freechips.rocketchip.tile.XLen
import freechips.rocketchip.rocket.MStatus
import freechips.rocketchip.config.Parameters

//Takes nasti bits and converts to rocc

class AXILRoccConverter(implicit val p: Parameters) extends Module {
  
  val nastiXDataBits = 32

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(nastiXDataBits.W)))
    val rocc = Decoupled(new RoCCCommand)
  })

  //this should eventually made parameterizable to the nasti width, but the nasti width is currently way wider
  //than the data width we get
  val counter = Reg(init = 0.U(3.W))
  val bitsBuffer = Reg(Vec(5, UInt(nastiXDataBits.W)))
  val rocc = Wire(new RoCCCommand)
  val roccInst = (new RoCCInstruction).fromBits(bitsBuffer(0))
  rocc.inst := roccInst
  rocc.rs1 := Cat(bitsBuffer(1), bitsBuffer(2))
  rocc.rs2 := Cat(bitsBuffer(3), bitsBuffer(4))
  rocc.status := new MStatus().fromBits(Bits(0))
  io.rocc.bits <> rocc
  io.in.ready := (counter < 5.U) && io.rocc.ready
  io.rocc.valid := (counter === 5.U)
  when (io.in.fire) {
    bitsBuffer(counter) := io.in.bits(31, 0)
    counter := counter + 1.U
  }

  when (io.rocc.fire) {
    counter := 0.U
  }
}

class RoccAXILConverter(implicit val p: Parameters) extends Module {

  val nastiXDataBits = 32

  val io = IO(new Bundle {
    val out = Decoupled(UInt(nastiXDataBits.W))
    val rocc = Decoupled(new RoCCResponse).flip
  })

  val xlen = p(XLen)
  val buffer = Reg(UInt(xlen.W))
  val rd = Reg(UInt(5.W))
  val sIdle :: sSend1 :: sSend2 :: sSend3 :: Nil = Enum(4)
  val state = RegInit(sIdle)
  io.rocc.ready := (state === sIdle)
  switch (state) {
    is (sIdle) {
      when (io.rocc.fire) {
        buffer := io.rocc.bits.data
        rd := io.rocc.bits.rd
        state := sSend1
      }
    }
    is (sSend1) {
      io.out.bits := buffer(xlen - 1, xlen - nastiXDataBits)
      when (io.out.fire) {
        state := sSend2
      }
    }
    is (sSend2) {
      io.out.bits := buffer(xlen - nastiXDataBits - 1, 0)
      when (io.out.fire) {
        state := sSend3
      }
    }
    is (sSend3) {
      io.out.bits := rd
      when (io.out.fire) {
        state := sIdle
      }
    }    
  }
  io.out.valid := (state === sSend1) || (state === sSend2) || (state === sSend3)

}
