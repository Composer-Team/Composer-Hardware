package design
// 32 fused mulAdd - integer version - reading 32 elements at each time - writing 8 elems each cycle

import chipsalliance.rocketchip.config.Config
import chisel3.{UInt, _}
import chisel3.util._
import composer.common.ComposerRoccCommand
import freechips.rocketchip.config.Parameters
import composer.{ComposerChannelParams, ComposerConstructor, ComposerCore, ComposerCoreParams, ComposerSystemParams, ComposerSystemsKey, WithAWSMem, WithComposer}
import freechips.rocketchip.subsystem.ExtMem
import freechips.rocketchip.tile.RoCCInstruction

/**
  * @param dataWidthBytes    width of signed integer data type
  * @param rowColDim         size of the square matrix
  * @param columnParallelism Given row of matrix B, split the row to this many arithmetic units to process in parallel
  * @param rowParallelism    How many independent rows of matrix A to handle simultaneously
  */
case class GemmParam(dataWidthBytes: Int,
                     rowColDim: Int,
                     columnParallelism: Int,
                     rowParallelism: Int)



class GemmCore(composerCoreParams: ComposerConstructor, coreP: GemmParam)(implicit p: Parameters) extends ComposerCore(composerCoreParams)(p) {
  val dataWidthBytes = coreP.dataWidthBytes
  val matrixSizeBytes = coreP.rowColDim * coreP.rowColDim * dataWidthBytes
  println("Matrix size is " + matrixSizeBytes + "B")
  val dataWidthBits = dataWidthBytes * 8
  val arithUnits = coreP.columnParallelism
  // split rows between each arith unit via striping
  val elemsPerArithUnit = coreP.rowColDim / arithUnits
  val rowBits = log2Up(coreP.rowColDim)
  val addrWidth = log2Up(p(ExtMem).get.master.size)
  val maxRowColDim = coreP.rowColDim - 1
  require(isPow2(coreP.rowColDim))
  require(isPow2(arithUnits))

  val s_idle :: s_addr_wait :: s_addr2_commit :: s_load :: s_acc :: s_writeback_1 :: s_writeback_2 :: s_commit :: s_finish:: Nil = Enum(9)

  val state = RegInit(s_idle)
  val (input_B, input_B_req) = declareSparseReader(0, dataWidthBytes * arithUnits)
  val addrBBase = Reg(UInt(addrWidth.W))
  input_B_req.valid := state === s_addr2_commit
  input_B_req.bits.addr := addrBBase
  input_B_req.bits.len := (matrixSizeBytes).U
  input_B.stop := state === s_idle

  val input_A = Seq.tabulate(coreP.rowParallelism) { a: Int =>
    val q = declareSparseReader(1 + a, dataWidthBytes)
    q._1.stop := state === s_idle
    q
  }

  // TODO for parallelism, reduce number of bits of types that skip
  val currentBRow = Reg(UInt(rowBits.W))
  // Matrix A state for each duplicated accumulator system
  val current_a = Seq.fill(coreP.rowParallelism)(Reg(SInt(dataWidthBits.W)))
  val a_loaded = Seq.fill(coreP.rowParallelism)(Reg(Bool()))
  // given from instruction
  val currentAIdx = Reg(UInt(rowBits.W))
  // this is ad-hoc but hopefully we have a better interface for this soon ala Brendan

  // global address information

  val inputA_addrs = Seq.fill(coreP.rowParallelism)(Reg(UInt(addrWidth.W)))
  input_A zip inputA_addrs foreach { case (ioa, addr) =>
    ioa._2.bits.len := matrixSizeBytes.U
    ioa._2.bits.addr := addr
    ioa._2.valid := state === s_addr2_commit
  }

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

  val output_addrs = Seq.fill(coreP.rowParallelism)(Reg(UInt(addrWidth.W)))
  val output = Seq.tabulate(coreP.rowParallelism)(declareSparseWriter(_, dataWidthBytes * arithUnits))
  (output, output_addrs).zipped foreach { case (chan, addr) =>
    chan._2.bits.addr := addr
    chan._2.bits.len := (coreP.dataWidthBytes * coreP.rowColDim).U
    chan._2.valid := state === s_addr2_commit
    chan._1.finished := state === s_idle

  }
  OAccs zip output foreach { case (acc, out) =>
    out._1.data.bits := Cat(acc)
    out._1.data.valid := false.B // overset later
  }
  OCache.zip(OAccs).foreach { case (ocache_row: Seq[SyncReadMem[SInt]], oacc_row: Seq[SInt]) =>
    ocache_row zip oacc_row map { case (oc: SyncReadMem[SInt], oa: SInt) =>
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
  active_bread := active_bread && !on_last
  val isDone = RegNext(RegNext(on_last && state === s_acc))

  /*
   * END DATA PIPELINE
   */

  // TODO init all wires!
  input_A foreach { a => a._1.data.ready := false.B }
  output foreach { a => a._1.data.valid := false.B }
  input_B.data.ready := false.B
  io.resp.bits.data := 0.U
  io.resp.valid := false.B
  io.req.ready := state === s_idle && (input_A map { ioa => ioa._2.ready } reduce (_ && _))

  def hook_thing_up(a: Seq[UInt], rs2: UInt): Unit = {
    a.zipWithIndex foreach { case (addr, idx) =>
      when(idx.U === rs2) {
        addr := io.req.bits.rs2(addrWidth - 1, 0)
      }
    }
  }
  val output_committed = Seq.fill(coreP.rowParallelism)(RegInit(false.B))

  switch(state) {
    is(s_idle) {
      when(io.req.fire) {
        // get ready to load in row of matrix B
        val inst = io.req.bits.inst
        switch(inst.rs1) {
          is(0.U) {
            // BEGIN COMPUTATION
            currentBRow := 0.U
            b_loadidx := 0.U
            b_loaded := false.B
            // get ready to load in subset of row(s) of matrix A
            a_loaded foreach {
              _ := false.B
            }
            currentAIdx := 0.U
            state := s_addr_wait
            OAccs_valid := false.B
          }
          is(1.U) {
            // STORE A_BASE
            hook_thing_up(inputA_addrs, inst.rs2)
          }
          is(2.U) {
            // STORE B_BASE
            addrBBase := io.req.bits.rs2(addrWidth - 1, 0)
          }
          is(3.U) {
            // STORE OUTPUT_BASE
            hook_thing_up(output_addrs, inst.rs2)
          }
        }
      }
    }
    is(s_addr_wait) {
      val output_channels_ready = output map (_._2.ready) reduce (_ && _)
      val inputs_ready = input_B_req.ready && input_A.map(_._2.ready).reduce(_ && _)
      when(inputs_ready && output_channels_ready) {
        state := s_addr2_commit
      }
    }
    is(s_addr2_commit) {
      state := s_load
    }
    is(s_load) {
      // load matrix A
      (input_A, a_loaded, current_a).zipped foreach { case (in_a, aload, areg) =>
        in_a._1.data.ready := !aload
        when(in_a._1.data.fire) {
          areg := in_a._1.data.bits.asSInt
          aload := true.B
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
        state := s_acc
        bread := 0.U
      }
      when(!on_last) {
        bread := bread + 1.U
      }

    }
    is(s_acc) {
      bread := bread + 1.U
      when(isDone) {
        // multiplied this element of matrix A row with everything in matrix B row, move on to another A ele, B row pair
        OAccs_valid := true.B
        when(currentBRow === maxRowColDim.U) {
          // if it's the last row of matrix B then we're done
          state := s_writeback_1
        }.otherwise {
          // otherwise theres more A/B to go
          b_loadidx := 0.U
          b_loaded := false.B
          currentBRow := currentBRow + 1.U
          a_loaded foreach {_ := false.B }
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
      output_committed foreach {
        _ := false.B
      }
    }
    is(s_commit) {
      output zip output_committed foreach { case (out, committed) =>
        out._1.data.valid := !committed
        when(out._1.data.fire) {
          committed := true.B
        }
      }
      when(output_committed.reduce(_ && _)) {
        when(bread === maxBCounter.U) {
          state := s_finish
        }.otherwise {
          bread := bread + 1.U
          state := s_writeback_1
        }
      }
    }
    is(s_finish) {
      io.resp.valid := true.B
      io.resp.bits.data := 1.U
      when (io.resp.fire) {
        state := s_idle
      }
    }
  }
}

class WithGemm(withNCores: Int,
               gp: GemmParam) extends Config((site, _, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
    coreParams = ComposerCoreParams(
      // one for B, one for each row of A
      readChannelParams = Seq.fill(1+gp.rowParallelism)(ComposerChannelParams()),
      // one for each row of output
      writeChannelParams = Seq.fill(gp.rowParallelism)(ComposerChannelParams())
    ),
    nCores = withNCores,
    name = "GemmCore",
    buildCore = {
      case (coreParams: ComposerConstructor, parameters: Parameters) =>
        new GemmCore(coreParams, gp)(parameters)
    }))
})

class GemmConfig extends Config (
  new WithGemm(1, GemmParam(4, 16, 1, 1)) ++ new WithComposer() ++ new WithAWSMem()
)

class MyModuleBundle extends Bundle {
  val a = SInt(13.W)
  val payload = VecInit(Seq.fill(25)(UInt(60.W)))
}
