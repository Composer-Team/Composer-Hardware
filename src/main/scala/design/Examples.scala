package design

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import composer.MemoryStreams.SequentialReader
import composer.{ComposerConstructor, ComposerCore, ComposerCoreParams}

import scala.util.Random

case class LFSRConfig(length: Int, taps: Seq[Int])

// galois lfsr
// note to self: look at dramsim2
class LFSRCore(composerCoreParams: ComposerConstructor)(implicit p: Parameters) extends ComposerCore(composerCoreParams) {
  val s_idle :: s_working :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val conf = p(LFSRConfigKey)
  val taps = conf.taps map (_ - 1)
  val rand_gen = new Random()

  val lfsr = Seq.fill(conf.length)(RegInit(rand_gen.nextBoolean().B))
  val outputbit = lfsr(0)
  val outputCache = Seq.fill(conf.length)(RegInit(false.B))
  val count = RegInit(0.U(log2Up(conf.length).W))

  io.req.ready := false.B
  io.resp.valid := false.B
  io.resp.bits.data := 0.U
  io.resp.bits.rd := 0.U
  io.busy := true.B

  when(state === s_idle) {
    io.busy := false.B
    io.req.ready := true.B
    when(io.req.fire) {
      state := s_working
      count := (conf.length - 1).U
    }
  }.elsewhen(state === s_working) {
    (0 until conf.length - 1).foreach { i =>
      lfsr(i) := lfsr(i + 1)
      outputCache(i) := outputCache(i + 1)
    }
    outputCache(conf.length - 1) := outputbit
    taps foreach { tap =>
      if (tap == (conf.length - 1)) {
        lfsr(tap) := outputbit
      } else {
        lfsr(tap) := lfsr(tap + 1) ^ outputbit
      }
    }
    count := count - 1.U
    when(count === 0.U) {
      state := s_finish
    }
  }.elsewhen(state === s_finish) {
    io.resp.bits.data := VecInit(outputCache).asUInt
    io.resp.valid := true.B
    when(io.resp.fire) {
      state := s_idle
    }
  }
}

// * * * * * * * * * * * * * * * * * * * * * * * * * * Simple ALU Implementation * * * * * * * * * * * * * * * * * * *

class ALUInput extends Bundle {
  val inputA = Wire(UInt(3.W))
  val inputB = Wire(UInt(3.W))
  val op = Wire(Bool())
}
class SimpleALU(composerCoreParams: ComposerConstructor)(implicit p: Parameters) extends ComposerCore(composerCoreParams) {
  val s_idle :: s_working :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val op = RegInit(0.U(io.req.bits.inst.funct.getWidth.W))
  val a = RegInit(0.U(io.req.bits.rs1.getWidth.W))
  val b = RegInit(0.U(io.req.bits.rs2.getWidth.W))
  val result = RegInit(0.U(io.resp.bits.data.getWidth.W))

  io.req.ready := false.B
  io.resp.valid := false.B
  io.resp.bits.data := 0.U
  io.resp.bits.rd := 0.U
  io.busy := true.B

  when(state === s_idle) {
    io.busy := false.B
    io.req.ready := true.B
    when(io.req.fire) {
      state := s_working
      op := io.req.bits.inst.rs1
      a := io.req.bits.rs1
      b := io.req.bits.rs2
    }
  }.elsewhen(state === s_working) {
    switch(op) {
      is(0.U) {
        result := a +& b
      }
      is(1.U) {
        result := a -& b
      }
      is(2.U) {
        result := a * b
      }
    }
    state := s_finish
  }.elsewhen(state === s_finish) {
    io.resp.bits.data := result
    io.resp.valid := true.B
    when(io.resp.fire) {
      state := s_idle
    }
  }
}

/*
 * * * * * * * * * * * * * * * * * * * * * VECTOR ADDER * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 */

case class VectorConfig(dWidth: Int, // what is the datatype width?
                       )

class VectorAdder(composerCoreParams: ComposerConstructor)(implicit p: Parameters) extends ComposerCore(composerCoreParams) {
  val s_idle :: s_load :: s_add :: s_store :: s_finish :: Nil = Enum(5)
  val vConfig = p(VectorAdderKey)
  require(isPow2(vConfig.dWidth), "The vector data width must be a power of two!")
  require(vConfig.dWidth % 8 == 0)
  val vDivs = 8
  val myReader = declareSequentialReader(usingReadChannelID = 0, vConfig.dWidth / 8, vDivs)
  val myWriter = declareSequentialWriter(usingWriteChannelID = 0, vConfig.dWidth / 8 * vDivs)
  val state = RegInit(s_idle)
  val toAdd = Reg(UInt(vConfig.dWidth.W))
  val vLen = Reg(UInt(io.req.bits.rs1.getWidth.W))
  val rfinish = RegInit(false.B)

  io.req.ready := state === s_idle
  io.busy := state =/= s_idle
  io.resp.valid := false.B // set again later under s_finish
  io.resp.bits.data := 0.U
  val rdreg = Reg(UInt(io.resp.bits.rd.getWidth.W))
  io.resp.bits.rd := rdreg


  val dArray = Seq.fill(vDivs)(Reg(UInt(vConfig.dWidth.W)))

  // user reverse to maintain order
  myWriter.data.bits := Cat(dArray.reverse)
  myWriter.finishEarly := state === s_idle
  myWriter.data.valid := false.B
  myReader.stop := false.B
  myReader.data.ready := false.B

  when(state === s_idle) {
    // don't start if it's a 0-lengthed segment
    when(io.req.fire && io.req.bits.rs1 =/= 0.U) {
      // start address has already been loaded into reader, just need to start reading
      state := s_load
      toAdd := io.req.bits.rs2(vConfig.dWidth - 1, 0)
      vLen := io.req.bits.rs1
      rfinish := false.B
      rdreg := io.req.bits.inst.rd
    }
  }.elsewhen(state === s_load) {
    myReader.data.ready := true.B
    when(myReader.data.fire) {
      // break the input into data-sized segments defined by vConfig.dwidth
      dArray zip myReader.data.bits foreach { case (reg: UInt, dat: UInt) =>
        reg := dat
      }
      state := s_add
      vLen := vLen - vDivs.U
      when(vLen === vDivs.U) {
        rfinish := true.B
      }
    }
  }.elsewhen(state === s_add) {
    dArray.foreach { a: UInt => a := a + toAdd }
    state := s_store
  }.elsewhen(state === s_store) {
    myWriter.data.valid := true.B
    when(myWriter.data.fire) {
      when(rfinish) {
        state := s_finish
      }.otherwise {
        state := s_load
      }
    }
  }.elsewhen(state === s_finish) {
    io.resp.valid := true.B
    when(io.resp.fire) {
      state := s_idle
    }
  }
}
