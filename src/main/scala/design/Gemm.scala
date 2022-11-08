package design
// 32 fused mulAdd - integer version - reading 32 elements at each time - writing 8 elems each cycle

import chisel3._
import chisel3.util._
import composer.common.{ComposerRoccCommand, ComposerRoccResponse}
import freechips.rocketchip.config.{Field, Parameters}
import composer.{ComposerConstructor, ComposerCore, CoreController}
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.ExtMem

/**
  * @param dataWidthBytes    width of signed integer data type
  * @param rowColDim         size of the square matrix
  * @param columnParallelism Given row of matrix B, split the row to this many arithmetic units to process in parallel
  * @param rowParallelism    How many independent rows of matrix A to handle simultaneously
  * @param rowCachesize      How many elements from row of matrix A to cache
  */
case class GemmNcubedBramMem32Config(dataWidthBytes: Int,
                                     rowColDim: Int,
                                     columnParallelism: Int,
                                     rowParallelism: Int,
                                     rowCachesize: Int)

case object GemmNcubedBramMem32Key extends Field[GemmNcubedBramMem32Config]


class GemmCore(composerCoreParams: ComposerConstructor)(implicit p: Parameters) extends ComposerCore(composerCoreParams)(p) {
  val coreP = p(GemmNcubedBramMem32Key)
  val dataWidthBytes = coreP.dataWidthBytes
  val dataWidthBits = dataWidthBytes * 8
  val arithUnits = coreP.columnParallelism
  // split rows between each arith unit via striping
  val elemsPerArithUnit = coreP.rowColDim / arithUnits
  val rowBits = log2Up(coreP.rowColDim)
  val maxRowColDim = coreP.rowColDim - 1
  require(isPow2(coreP.rowColDim))
  require(isPow2(arithUnits))

  val input_B = declareSequentialReader(0, dataWidthBytes * arithUnits)
  val output = Seq.tabulate(coreP.rowParallelism)(declareSparseWriter(_, dataWidthBytes * arithUnits))
  val output_valids = Seq.fill(coreP.rowParallelism)(RegInit(false.B))
  val input_A = Seq.tabulate(coreP.rowParallelism) { a: Int =>
    declareSparseReader(1 + a, dataWidthBytes)
  }

  val s_idle :: s_load :: s_reg_prep :: s_acc :: s_writeback_1 :: s_writeback_2 :: s_commit :: Nil = Enum(7)
  val state = RegInit(s_idle)

  // TODO for parallelism, reduce number of bits of types that skip
  val currentBRow = Reg(UInt(rowBits.W))

  // Matrix A state for each duplicated accumulator system
  val ACache = Seq.fill(coreP.rowParallelism)(SyncReadMem(coreP.rowCachesize, SInt(dataWidthBits.W)))
  val current_a = Seq.fill(coreP.rowParallelism)(Reg(SInt(dataWidthBits.W)))
  val a_loaded = Seq.fill(coreP.rowParallelism)(Reg(Bool()))
  val max_a_load_idx = coreP.rowCachesize-1
  val aCacheIdxBits = log2Up(max_a_load_idx)
  val a_loadidx = Seq.fill(coreP.rowParallelism)(Reg(UInt(aCacheIdxBits.W)))
  val currentACacheIdx = Reg(UInt(aCacheIdxBits.W))
  // given from instruction
  val currentARow = Seq.fill(coreP.rowParallelism)(Reg(UInt(rowBits.W)))
  val currentAIdx = Reg(UInt((rowBits-aCacheIdxBits).W))
  val lastAIteration = currentAIdx.andR
  ACache zip current_a foreach { case (mem, a) =>
    val wirea = mem.read(currentACacheIdx, state === s_reg_prep)
    when(RegNext(state === s_reg_prep)) {
      a := wirea
    }
  }

  // global address information
  val addrWidth = log2Up(p(ExtMem).get.master.size)
  val addrABase = Reg(UInt(addrWidth.W))
  val addrBBase = Reg(UInt(addrWidth.W))

  // cache row of B in BCache and intermediate results in OCache "output cache"
  // B is also shared across all rows
  val BCache = Seq.fill(arithUnits)(SyncReadMem(elemsPerArithUnit, SInt(dataWidthBits.W)))
  val b_loadidx = Reg(UInt(log2Up(elemsPerArithUnit).W))
  val maxBCounter = elemsPerArithUnit - 1
  val b_loaded = Reg(Bool())

  // accumulated values cache
  // these get co-opted for storing the output values while they're being written
  val OCache = Seq.fill(coreP.rowParallelism, arithUnits)(SyncReadMem(elemsPerArithUnit, SInt(dataWidthBits.W)))
  val OAccs = Seq.fill(coreP.rowParallelism, arithUnits)(Reg(SInt(dataWidthBits.W)))
  val OAccs_valid = Reg(Bool())
  // 🍞🥖
  val bread = Reg(UInt(log2Up(elemsPerArithUnit).W))
  val active_bread = Reg(Bool())
  OAccs zip output foreach { case (acc, out) =>
    out._1.data.bits := Cat(acc)
  }
  OCache.zip(OAccs).foreach { case (OCache_row: Seq[SyncReadMem[SInt]], OAcc_row: Seq[SInt]) =>
    OCache_row zip OAcc_row map { case (oc: SyncReadMem[SInt], oa: SInt) =>
      val cache_read = oc.read(b_loadidx, (state === s_acc && OAccs_valid && active_bread) || (state === s_writeback_1)) // 1 cy
      oa := Mux(OAccs_valid, cache_read, 0.S) // 2 cy
    }
  }

  // multiple stages within s_acc state
  // 1) read bram for current acc
  // 2) mult b * a + acc => new acc
  // 3) store new acc

  /*
   *       DATA PIPELINE
   */
  val all_a_loaded = a_loaded.reduce(_ && _)
  // set defaults
  input_B.data.ready := false.B

  // after 1 cy
  val bread_val = Seq.fill(arithUnits)(Wire(SInt(dataWidthBits.W)))
  bread_val zip BCache foreach { case (bwire, mem) =>
    bwire := mem.read(bread, active_bread)
  }

  // avail after 2 cy
  val o_mul = current_a.flatMap { row_a =>
    bread_val map { arith_b =>
      RegNext(row_a * arith_b)
    }
  }


  // on 3rd cycle
  val o_acc = o_mul zip OAccs.flatten map { case (o_mul_dat, o_acc_old_dat) =>
    o_mul_dat + o_acc_old_dat
  }
  val addr = Pipe(active_bread, b_loadidx, 2)
  val write_en = Pipe(true.B, active_bread, 2)
  o_acc zip OCache.flatten foreach { case (o_acc_dat, ocache) =>
    when(write_en.bits) {
      ocache.write(addr.bits, o_acc_dat)
    }
  }

  val on_last = bread === maxBCounter.U
  when(!on_last && state === s_load) {
    bread := bread + 1.U
  }
  active_bread := active_bread && !on_last
  val isDone = Pipe(true.B, on_last, 2).bits

  /*
   * END DATA PIPELINE
   */

  // TODO init all wires!
  input_A foreach { a => a._1.data.ready := false.B }
  output foreach { a => a._1.data.valid := false.B }
  input_B.data.ready := false.B

  io.req.ready := state === s_idle && (input_A map { ioa => ioa._2.ready} reduce(_ && _))
  switch(state) {
    is(s_idle) {
      when(io.req.fire) {
        // get ready to load in row of matrix B
        currentBRow := 0.U
        b_loadidx := 0.U
        b_loaded := false.B
        input_A foreach { arow_input =>
          // set up start address for each matrix A row read transaction
          arow_input._2 := ???
        }

        // get ready to load in subset of row(s) of matrix A
        a_loaded foreach {_ := false.B}
        a_loadidx foreach {_ := 0.U }
        currentAIdx := 0.U
        currentACacheIdx := 0.U

        state := s_load
        OAccs_valid := false.B
      }
    }
    is(s_load) {
      // load matrix A
      (input_A, (a_loaded zip a_loadidx), ACache).zipped foreach { case (row_input, (aloaded, idx), acache) =>
        row_input._1.data.ready := !aloaded
        when (row_input._1.data.fire) {
          when (idx === max_a_load_idx.U) {
            aloaded := true.B
          }
          acache.write(idx, row_input._1.data.bits.asSInt)
          idx := idx + 1.U
        }
      }

      // load matrix B
      input_B.data.ready := !b_loaded
      when(input_B.data.fire) {
        b_loadidx := b_loadidx + 1.U
        BCache.zipWithIndex.foreach { case (mem, a) =>
          mem.write(b_loadidx, input_B.data.bits(input_B.data.bits((a + 1) * dataWidthBits - 1, a * dataWidthBits)).asSInt)
        }
        when(b_loadidx === maxBCounter.U) {
          b_loaded := true.B
        }
      }

      when(b_loaded && all_a_loaded) {
        state := s_reg_prep
        bread := 0.U
        current_a zip input_A map { case (a, io) => a := io._1.data.bits }
      }
    }
    is(s_reg_prep) {
      // load matrix A values into registers
      state := s_acc
      currentACacheIdx := currentACacheIdx + 1.U
    }
    is(s_acc) {
      when(isDone) {
        OAccs_valid := true.B
        // first step is do ACache re-use
        val done_entire_acache = currentACacheIdx === 0.U // implies roll-over on previous time in s_prep_reg
        when (!done_entire_acache) {
          // not done looking at ACache for this row of matrix B
          // load next element of ACache and re-use row from B
          state := s_reg_prep
        }.otherwise {
          // otherwise we're done looking at this pair of ACache data and matrix B row
          when (currentBRow === maxRowColDim.U) {
            // if it's the last row of matrix B, then we start back over in matrix B with a new subset of matrix A
            when (lastAIteration) {
              // there's no more of matrix A row to look at, so write back
              state := s_writeback_1
              bread := 0.U
            }.otherwise {
              // otherwise there's more A to look at
              // load new subset of A row
              a_loaded foreach {_ := false.B}
              a_loadidx foreach {_ := 0.U}
              b_loaded := false.B
              b_loadidx := 0.U
              currentAIdx := currentAIdx + 1.U
              currentBRow := 0.U
              state := s_load
            }
          }.otherwise {
            // otherwise it's not the last row of matrix B and we can load another row with the same matrix A subset
            // done with ACache, but not whole B matrix, so start over in ACache with new row of B
            b_loadidx := 0.U
            b_loaded := false.B
            currentBRow := currentBRow + 1.U
            currentACacheIdx := 0.U
            state := s_load
          }
        }
      }
    }
    is (s_writeback_1) {
      // split this into two stages for read from banks and then handing to memory
      state := s_writeback_2
      // reading from accumulator
    }
    is (s_writeback_2) {
      // putting result from .read() into reg
      state := s_commit
      output_valids foreach {_ := true.B}
    }
    is (s_commit) {
      output zip output_valids foreach { case (out, valid) =>
        when (out._1.data.fire) {
          valid := false.B
        }
      }
      when (!output_valids.reduce(_ || _)) {
        when (bread === maxBCounter.U) {
          state := s_idle
        }
      }
    }
  }
}