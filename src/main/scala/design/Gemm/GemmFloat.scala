package design.Gemm
// 32 fused mulAdd - integer version - reading 32 elements at each time - writing 8 elems each cycle

import chipsalliance.rocketchip.config.Config
import chisel3.{UInt, _}
import chisel3.util._
import composer.MemoryStreams._
import freechips.rocketchip.config.Parameters
import composer._
import composer.common.splitIntoChunks
import fpnewWrapper._
import freechips.rocketchip.subsystem.ExtMem

//noinspection DuplicatedCode
class GemmFloatCore(composerCoreParams: ComposerConstructor, coreP: GemmParam)(implicit p: Parameters)
  extends ComposerCore(composerCoreParams)(p) {
  CppGeneration.addUserCppDefinition("int", "FPGA_RC_DIM", coreP.rowColDim)
  val dataWidthBytes = coreP.dataWidthBytes
  val submatrixRowSizeBytes = coreP.rowColDim * dataWidthBytes
  val dataWidthBits = dataWidthBytes * 8
  val arithUnits = coreP.columnParallelism
  // split rows between each arith unit via striping
  val elemsPerArithUnit = coreP.rowColDim / arithUnits
  val rowBits = log2Up(coreP.rowColDim)
  val addrWidth = log2Up(p(ExtMem).get.master.size)
  val maxRowColDim = coreP.rowColDim - 1

  require(isPow2(coreP.rowColDim))
  require(isPow2(arithUnits))
  require(isPow2(coreP.rowParallelism))


  //noinspection ScalaUnnecessaryParentheses
  val (s_idle :: s_addr_comp :: s_prefetch_B_matrix :: s_load_C_addr :: s_load_C_data :: s_addr_wait_AB ::
    s_addr_commit :: s_load :: s_acc :: s_writeback_1 :: s_writeback_2 :: s_commit :: s_finish :: Nil) = Enum(13)

  // since we're doing GeMM, we're only operating on a subset of both the A and B matrix. This means between different
  // subrows of B, we're going to skip a whole bunch of elements. The difference _should_ be bus aligned (512b)
  // This difference is shared between sequences of A, B, and output
  val realRowBits = log2Up(coreP.maxRCDim) + 1
  val realRowLength = Reg(UInt(realRowBits.W))
  val realRowBytes = Cat(realRowLength, 0.U(log2Up(dataWidthBytes).W))

  val state = RegInit(s_idle)

  val (reqChannelB, accessChannelB) = getScratchpad("ChannelB")
  val (reqChannelOut, dataChannelOut) = getWriterModules(name = "ChannelOut", useSoftwareAddressing =  false,
    dataBytes = dataWidthBytes * arithUnits)
  // these channels will read both the buffers and the A Matrix
  val (reqChannelRow, dataChannelRow) = getReaderModules(name = "ChannelA", useSoftwareAddressing = false,
    dataBytes = dataWidthBytes, vlen=1)

  val BAddr = Reg(UInt(addrWidth.W))
  val BSave = Reg(UInt(addrWidth.W))
  val haveCachedB = RegInit(false.B)
  val OutputAddr = List.fill(coreP.rowParallelism)(Reg(UInt(addrWidth.W)))
  val AAddrs = List.fill(coreP.rowParallelism)(Reg(UInt(addrWidth.W)))

  // initialize some of the channels
  reqChannelB.request.valid := false.B
  reqChannelB.request.bits := DontCare
  reqChannelRow foreach { ioa =>
    ioa.bits.len := submatrixRowSizeBytes.U
    ioa.valid := false.B
    ioa.bits.addr := DontCare
  }
  dataChannelRow foreach { datchan =>
    datchan.data.ready := false.B
    datchan.stop := false.B
  }
  dataChannelOut foreach (_.finishEarly := false.B)

  reqChannelOut zip OutputAddr foreach { case (chan, addr) =>
    chan.bits.addr := addr
    chan.valid := false.B
    chan.bits.len := submatrixRowSizeBytes.U
  }

  // counter to gate whether or not we're loading in the right output buffer
  val arithCounter = Reg(UInt(rowBits.W))

  // iterate through matrix B 1 row at a time
  val currentBRow = Reg(UInt((rowBits + 1).W))
  // Matrix A state for each duplicated accumulator system
  val current_a = Seq.fill(coreP.rowParallelism)(Reg(UInt(dataWidthBits.W)))
  val row_loaded = Seq.fill(coreP.rowParallelism)(Reg(Bool()))
  val ACounter = Reg(UInt(rowBits.W))
  val lastA = (coreP.rowColDim / coreP.rowParallelism) - 1
  // this is ad-hoc but hopefully we have a better interface for this soon ala Brendan

  // cache row of B in BCache and intermediate results in OCache "output cache"
  // B is also shared across all rows
  val maxBCounter = elemsPerArithUnit - 1

  // accumulated values cache
  // these get co-opted for storing the output values while they're being written
  val OCache = Seq.fill(coreP.rowParallelism, arithUnits)(Reg(Vec(elemsPerArithUnit, UInt(dataWidthBits.W))))
  val OAccs = Seq.fill(coreP.rowParallelism, arithUnits)(Wire(UInt(dataWidthBits.W)))
  // 🍞🥖
  val bread = Reg(UInt(log2Up(elemsPerArithUnit).W))

  OAccs zip dataChannelOut foreach { case (acc, out) =>
    out.data.bits := Cat(acc.reverse)
    out.data.valid := false.B // overset later
  }

  // multiple stages within s_acc state
  // 1) read bram for current acc
  // 2) mult b * a + acc => new acc
  // 3) store new acc

  /*
   *       DATA PIPELINE
   */

  val all_rows_loaded = row_loaded.reduce(_ && _)
  // set defaults
  accessChannelB.readReq.valid := state === s_acc
  accessChannelB.readReq.bits := Cat(currentBRow, bread)

  // after 2 cy
  val bread_val = (0 until arithUnits) map { idx =>
    val start = idx * dataWidthBits
    val end = (idx + 1) * dataWidthBits - 1
    accessChannelB.readRes.bits(end, start).asUInt
  }

  val o_mul = current_a flatMap { row_a: UInt =>
    val a_pipe = RegNext(RegNext(row_a))
    bread_val map { subB: UInt =>
      (subB,  a_pipe)
    }
  }

  // after 2cy, on 3rd cycle
  val fma_tuples = o_mul zip OAccs.flatten map { case (mul_pair, add_item: UInt) =>
    (mul_pair, add_item)
  }

  val bread_stage = RegNext(bread)
  val bread_stage2 = RegNext(bread_stage)

  println(s"Elaborating fpu with ${fma_tuples.length} lanes")
  val fpu_latency = 3
  val fpu = Module(new FPUNew(FPFloatFormat.Fp32, fma_tuples.length, fpu_latency, Seq(FPNewOpClass.ADDMUL), tagWidth = log2Up(elemsPerArithUnit)))
  fpu.io.req.valid := RegNext(RegNext(state === s_acc))
  fpu.io.req.bits.operands(0) zip fma_tuples.map(_._1._1) foreach (a => a._1 := a._2)
  fpu.io.req.bits.operands(1) zip fma_tuples.map(_._1._2) foreach (a => a._1 := a._2)
  fpu.io.req.bits.operands(2) zip fma_tuples.map(_._2) foreach (a => a._1 := a._2)
  fpu.io.req.bits.tag := bread_stage2
  fpu.io.req.bits.op := FPOperation.FMADD
  fpu.io.req.bits.opModifier := 0.U
  fpu.io.req.bits.intFormat := DontCare
  fpu.io.req.bits.dstFormat := DontCare
  fpu.io.req.bits.srcFormat := FPFloatFormat.Fp32
  fpu.io.req.bits.roundingMode := FPRoundingMode.RNE

  fpu.io.flush := false.B
  fpu.io.resp.ready := true.B


  //
  OCache zip OAccs foreach { case (oc_per_row, oa_per_row) =>
    val bread_per_row_stage2 = bread_stage2
    oc_per_row zip oa_per_row foreach { case (oc_per_arith, acc_per_arith) =>
      acc_per_arith := oc_per_arith(bread_per_row_stage2)
    }
  }

  // write back accumulator register to row
  when(fpu.io.resp.valid) {
    fpu.io.resp.bits.result zip OCache.flatten foreach { case (o_mac_result, ocache) =>
      ocache(fpu.io.resp.bits.tag) := o_mac_result
    }
  }

  //  val on_last = bread === maxBCounter.U

  /*
   * END DATA PIPELINE
   */

  io.resp.bits.data := composerConstructor.composerCoreWrapper.core_id.U
  io.resp.valid := false.B
  io.req.ready := state === s_idle &&
    (reqChannelRow map { ioa => ioa.ready } reduce (_ && _)) &&
    reqChannelB.request.ready &&
    (reqChannelOut map { io => io.ready } reduce (_ && _))

  val buffer_valid = Seq.fill(coreP.rowParallelism)(RegInit(false.B))

  CppGeneration.exportChiselEnum(GemmCoreCommands)

  switch(state) {
    is(s_idle) {
      haveCachedB := false.B
      when(io.req.fire) {
        // get ready to load in row of matrix B
        val inst = io.req.bits.inst
        switch(GemmCoreCommands(inst.rs1(GemmCoreCommands.getWidth-1, 0))) {
          is(GemmCoreCommands.rowAndGo) {
            // BEGIN COMPUTATION
            currentBRow := 1.U
            realRowLength := io.req.bits.payload2(realRowBits - 1, 0)
            ACounter := 0.U
            state := s_addr_comp
          }
          is(GemmCoreCommands.A) {
            // STORE A_BASE
            AAddrs(0) := io.req.bits.payload2(addrWidth - 1, 0)
          }
          is(GemmCoreCommands.B) {
            // STORE B_BASE
            BAddr := io.req.bits.payload2(addrWidth - 1, 0)
            BSave := io.req.bits.payload2(addrWidth - 1, 0)
          }
          is(GemmCoreCommands.C) {
            // STORE OUTPUT_BASE
            OutputAddr(0) := io.req.bits.payload2(addrWidth - 1, 0)
          }
        }
      }
    }
    is(s_addr_comp) {
      // compute all of the addresses
      currentBRow := currentBRow + 1.U
      AAddrs.indices.foreach { idx: Int =>
        if (idx > 0) {
          when(currentBRow === idx.U) {
            AAddrs(idx) := AAddrs(idx - 1) + realRowBytes
            OutputAddr(idx) := OutputAddr(idx - 1) + realRowBytes
          }
        }
      }
      when(currentBRow === coreP.rowParallelism.U) {
        currentBRow := 0.U
        when(haveCachedB) {
          state := s_load_C_addr
        }.otherwise {
          haveCachedB := true.B
          state := s_prefetch_B_matrix
        }
        reqChannelOut foreach (_.valid := true.B)
      }
      // get ready to load in subset of row(s) of matrix A
      row_loaded foreach {
        _ := false.B
      }
    }
    is(s_prefetch_B_matrix) {
      reqChannelB.request.valid := true.B
      // since column parallelism increases # items read / cycle, they need to all live in the same SRAM/URAM cell,
      //    reducing the number of separable items in a row to rowColDim/columnParallelism
      reqChannelB.request.bits.scAddr := Cat(currentBRow, 0.U(log2Up(coreP.rowColDim / coreP.columnParallelism).W))
      reqChannelB.request.bits.memAddr := BAddr
      reqChannelB.request.bits.len := submatrixRowSizeBytes.U
      when(reqChannelB.request.fire) {
        BAddr := BAddr + realRowBytes
        currentBRow := currentBRow + 1.U
        when(currentBRow === maxRowColDim.U) {
          state := s_load_C_addr
          currentBRow := 0.U
        }
      }
    }
    is(s_load_C_addr) {
      reqChannelRow.map(_.bits.addr) zip OutputAddr foreach { case (ioaddr, oaddr) => ioaddr := oaddr }
      when (reqChannelRow map (_.ready) reduce (_ && _)) {
        reqChannelRow foreach (_.valid := true.B)
        state := s_load_C_data
      }
      bread := 0.U
      arithCounter := 0.U
    }
    is(s_load_C_data) {
      // when all of the rows have a piece of data ready
      when(dataChannelRow map (_.data.valid) reduce (_ && _)) {
        // when we're not on the last thing
        dataChannelRow.foreach(_.data.ready := true.B)
        arithCounter := arithCounter + 1.U
        when(arithCounter.andR) {
          // increment reader
          bread := 0.U
          state := s_addr_wait_AB
        }
        // store it

        val arith_id = arithCounter(log2Up(arithUnits)-1, 0)
        val arith_offset = arithCounter(rowBits-1, log2Up(arithUnits))
        dataChannelRow map (_.data.bits) zip OCache foreach { case (dat, cache) =>
          cache.zipWithIndex foreach { case (arithcache, idx) =>
            when(idx.U === arith_id) {
              arithcache(arith_offset) := dat(0).asUInt
            }
          }
        }
      }
    }
    is(s_addr_wait_AB) {
      val inputs_ready = reqChannelB.request.ready && reqChannelRow.map(_.ready).reduce(_ && _)
      // reqBIdle ensures that B has been entirely fetched
      when(inputs_ready) {
        state := s_addr_commit
      }
    }
    is(s_addr_commit) {
      when (reqChannelRow.map(_.ready) reduce (_ && _)) {
        state := s_load
        reqChannelRow zip AAddrs foreach { case (row, addr) =>
          row.bits.addr := addr
          row.valid := true.B
        }

      }
    }
    is(s_load) {
      // load single element of matrix A
      dataChannelRow.lazyZip(row_loaded).lazyZip(current_a) foreach { case (in_a, aload, areg) =>
        in_a.data.ready := !aload
        when(in_a.data.fire) {
          areg := in_a.data.bits(0).asUInt
          aload := true.B
        }
      }

      when(all_rows_loaded) {
        state := s_acc
        bread := 0.U
      }
    }
    is(s_acc) {
      bread := bread + 1.U

      when(bread === maxBCounter.U) {
        // multiplied this element of matrix A row with everything in matrix B row, move on to another A ele, B row pair
        when(currentBRow === maxRowColDim.U) {
          // if it's the last row of matrix B then we're done
          state := s_writeback_1
        }.otherwise {
          // otherwise theres more A/B to go
          currentBRow := currentBRow + 1.U
          row_loaded foreach {
            _ := false.B
          }
          state := s_load
        }
      }
    }
    is(s_writeback_1) {
      // split this into two stages for read from banks and then handing to memory
      state := s_writeback_2
      // reading from accumulator
    }
    is(s_writeback_2) {
      // putting result from .read() into reg
      state := s_commit
      buffer_valid foreach {
        _ := false.B
      }
    }
    is(s_commit) {
      dataChannelOut zip buffer_valid foreach { case (out, committed) =>
        out.data.valid := !committed
        when(out.data.fire) {
          committed := true.B
        }
      }
      when(buffer_valid.reduce(_ && _)) {
        when(bread === maxBCounter.U) {
          state := s_finish
        }.otherwise {
          bread := bread + 1.U
          state := s_writeback_1
        }
      }
    }
    is(s_finish) {
      dataChannelOut.foreach(_.finishEarly := true.B)
      when((reqChannelOut.map(_.ready) ++ dataChannelOut.map(_.channelIdle)).reduce(_ && _)) {
        when(ACounter === lastA.U) {
          io.resp.valid := true.B
          when(io.resp.fire) {
            state := s_idle
            ACounter := 0.U
          }
        }.otherwise {
          currentBRow := 1.U
          state := s_addr_comp
          ACounter := ACounter + 1.U
          AAddrs(0) := AAddrs.last + realRowBytes
          OutputAddr(0) := OutputAddr.last + realRowBytes
          BAddr := BSave
        }
      }
    }
  }
}

//noinspection DuplicatedCode
class WithGemmFloat(withNCores: Int,
                    gp: GemmParam) extends Config((site, _, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
    coreParams = ComposerCoreParams(memoryChannelParams = List(
      CScratchpadChannelParams(
        "ChannelB",
        supportWriteback = false,
        dataWidthBits = gp.dataWidthBytes * 8 * gp.columnParallelism,
        nDatas = gp.rowColDim * gp.rowColDim / gp.columnParallelism,
        specialization = CScratchpadSpecialization.flatPacked),
      CReadChannelParams(
        "ChannelA",
        gp.rowParallelism,
        maxInFlightTxs = gp.prefetchAmt),
      CWriteChannelParams(
        "ChannelOut",
        gp.rowParallelism,
        maxInFlightTxs = gp.prefetchAmt))),
    nCores = withNCores,
    name = "GemmCore",
    buildCore = {
      case (coreParams, parameters) =>
        new GemmFloatCore(coreParams, gp)(parameters)
    }))
})

