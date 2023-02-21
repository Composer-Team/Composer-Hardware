package design.machsuite.sorts

import chipsalliance.rocketchip.config.Parameters
import composer.{ComposerConstructor, ComposerCore, DataChannelIO}

import scala.math.pow
import chisel3._
import chisel3.util._
import composer.MemoryStreams.CScratchpadAccessBundle
import fpnewWrapper.{FPFloatFormat, FPNewOpClass, FPOperation, FPRoundingMode, FPUNew}

/**
  * Parameters for the merge sort class
  *
  * @param dataWidthBytes The width of the data in bytes
  *                       (e.g. 4 for 32-bit data)
  * @param maxArrayLength The maximum length of the array to be sorted
  * @param arraySublength The length of the subarrays to be sorted within BRAM
  */
case class SortParams(dtype: FPFloatFormat.Type,
                      maxArrayLength: Int,
                      arraySublength: Int
                     )

// Credit for base implementation to Mason Ma & Entropy Xu @ Duke Apex Lab
// Modified by Chris Kjellqvist to support FP32 and be multi-stage
class BitonicSorter(sp: SortParams) extends Module {
  val n = sp.arraySublength
  val w = FPFloatFormat.toWidth(sp.dtype)

  assert(log2Ceil(n) == log2Floor(n), "n must be a power of 2")

  val io = IO(new Bundle {
    val arrayIn = Input(Vec(n, UInt(w.W)))
    val arrayOut = Output(Vec(n, UInt(w.W)))
  })
  private val debug = false
  private val nRounds = log2Ceil(n)
  val nStages = Array.tabulate(nRounds) { i => i + 1 }.sum
  // Create wires for each stage
  private val stages: Array[Vec[UInt]] =
    Array.fill(nStages + 1)(Reg(Vec(n, UInt(w.W))))
  stages(0) := io.arrayIn
  if (debug) Console.println("nStages: " + nStages)

  private var stageCounter = 0
  for (iRound <- 0 until nRounds) {
    // Every round would make 2x entries in monotonic order
    // compared to previous round
    var wireOffset = pow(2, iRound).toInt
    for (iSubround <- 0 to iRound) {
      // Operations within a subround is completely independent
      // and can be parallelized
      stageCounter += 1
      for (iCompare <- 0 until ((n / 2) / wireOffset)) {
        val inverseDirection = iCompare / (pow(2, iSubround).toInt) % 2 == 1
        for (iWire <- 0 until wireOffset) {
          val aIdx = iCompare * wireOffset * 2 + iWire
          val bIdx = aIdx + wireOffset
          val a = stages(stageCounter - 1)(aIdx)
          val b = stages(stageCounter - 1)(bIdx)
          val comparator = Module(new FPUNew(
            FPFloatFormat.Fp32,
            lanes = 1,
            stages = 0,
            supportedOps = Seq(FPNewOpClass.NONCOMP),
            tagWidth = 1))
          // overset to tie off things we dont care about and then set with values we _do_ care about
          comparator.io.req.bits := DontCare
          comparator.io.req.bits.op := FPOperation.CMP
          comparator.io.req.bits.operands(0)(0) := a
          comparator.io.req.bits.operands(1)(0) := b
          comparator.io.req.bits.roundingMode := FPRoundingMode.RTZ // encodes < operator

          val result = comparator.io.resp.bits.result(0)(0).asBool
          if (inverseDirection) {
            stages(stageCounter)(bIdx) := Mux(result, a, b)
            stages(stageCounter)(aIdx) := Mux(result, b, a)
          } else {
            stages(stageCounter)(aIdx) := Mux(result, a, b)
            stages(stageCounter)(bIdx) := Mux(result, b, a)
          }
        }
      }
      wireOffset /= 2
    }
  }
  // Connect the last stage to the output
  io.arrayOut := stages(nStages)
}

class MultiChooseMerger(p: Int, dtype: FPFloatFormat.Type) extends Module {
  require(isPow2(p))
  val sp_io = IO(Vec(p, Flipped(new DataChannelIO(FPFloatFormat.toWidth(dtype) / 8, vlen = 1))))
  val currentHold = Reg(Vec(p, FPFloatFormat.toWidth(dtype)))


}