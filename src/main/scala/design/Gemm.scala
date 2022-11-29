package design
// 32 fused mulAdd - integer version - reading 32 elements at each time - writing 8 elems each cycle

import chipsalliance.rocketchip.config.Config
import chisel3.{UInt, _}
import chisel3.util._
import freechips.rocketchip.config.Parameters
import composer.{ComposerChannelParams, ComposerConstructor, ComposerCore, ComposerCoreParams, ComposerSystemParams, ComposerSystemsKey, WithAWSMem, WithComposer}
import design.GemmCore.splitPayload
import freechips.rocketchip.subsystem.ExtMem

/**
  * @param dataWidthBytes    width of signed integer data type
  * @param rowColDim         size of the square matrix
  * @param columnParallelism Given row of matrix B, split the row to this many arithmetic units to process in parallel
  * @param rowParallelism    How many independent rows of matrix A to handle simultaneously
  */
case class GemmParam(dataWidthBytes: Int,
                     rowColDim: Int,
                     columnParallelism: Int,
                     rowParallelism: Int,
                     maxRCDim: Int)


object GemmCore {
  def splitPayload(in: UInt, payloadSize: Int): Seq[UInt] = {
    assert(in.getWidth % payloadSize == 0)
    val divs = in.getWidth / payloadSize
    (0 until divs) map { idx =>
      val end = (idx + 1) * payloadSize - 1
      val start = idx * payloadSize
      in(end, start)
    }
  }
}

class GemmCore(composerCoreParams: ComposerConstructor, coreP: GemmParam)(implicit p: Parameters) extends ComposerCore(composerCoreParams)(p) {
  val dataWidthBytes = coreP.dataWidthBytes
  val rowSizeBytes = coreP.rowColDim * dataWidthBytes
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


  val s_idle :: s_addr_comp :: s_addr_wait_buffers :: s_load_buffer1 :: s_load_buffer2 :: s_addr_wait_AB :: s_addr_commit :: s_load :: s_acc :: s_writeback_1 :: s_writeback_2 :: s_commit :: s_finish :: Nil = Enum(13)

  // since we're doing GeMM, we're only operating on a subset of both the A and B matrix. This means between different
  // subrows of B, we're going to skip a whole bunch of elements. The difference _should_ be bus aligned (512b)
  // This difference is shared between sequences of A, B, and output
  val realRowBits = log2Up(coreP.maxRCDim) + 1
  val realRowLength = Reg(UInt(realRowBits.W))
  val realRowBytes = Cat(realRowLength, 0.U(log2Up(dataWidthBytes).W))
  println("realRowBytes shift" + log2Up(dataWidthBytes))

  val state = RegInit(s_idle)

  val (dataChannelB, reqChannelB) = declareSparseReader(0, dataWidthBytes * arithUnits)
  val (dataChannelOut, reqChannelOut) = {
    val q = List.tabulate(coreP.rowParallelism)(declareSparseWriter(_, dataWidthBytes * arithUnits))
    (q.map(_._1), q.map(_._2))
  }
  // these channels will read both the buffers and the A Matrix
  val (dataChannelRow, reqChannelRow) = {
    val q = List.tabulate(coreP.rowParallelism) { a: Int => declareSparseReader(1 + a, dataWidthBytes) }
    (q.map(_._1), q.map(_._2))
  }

  val BAddr = Reg(UInt(addrWidth.W))
  val BSave = Reg(UInt(addrWidth.W))
  val OutputAddr = List.fill(coreP.rowParallelism)(Reg(UInt(addrWidth.W)))
  val AAddrs = List.fill(coreP.rowParallelism)(Reg(UInt(addrWidth.W)))

  // initialize some of the channels
  reqChannelB :: reqChannelRow foreach { ioa =>
    ioa.bits.len := rowSizeBytes.U
    ioa.valid := false.B
    ioa.bits.addr := DontCare
  }
  (dataChannelB :: dataChannelRow) foreach { datchan =>
    datchan.data.ready := false.B
    datchan.stop := false.B
  }
  dataChannelOut foreach (_.finished := false.B)

  reqChannelOut zip OutputAddr foreach { case (chan, addr) =>
    chan.bits.addr := addr
    chan.valid := false.B
    chan.bits.len := rowSizeBytes.U
  }

  // counter to gate whether or not we're loading in the right output buffer
  val arithCounter = Reg(UInt(log2Up(arithUnits).W))

  // iterate through matrix B 1 row at a time
  val currentBRow = Reg(UInt(rowBits.W))
  // Matrix A state for each duplicated accumulator system
  val current_a = Seq.fill(coreP.rowParallelism)(Reg(SInt(dataWidthBits.W)))
  val row_loaded = Seq.fill(coreP.rowParallelism)(Reg(Bool()))
  val ACounter = Reg(UInt(rowBits.W))
  println("ACounter is " + rowBits + "b")
  val lastA = (coreP.rowColDim / coreP.rowParallelism) - 1
  println("Needs to represent at least " + lastA)
  // this is ad-hoc but hopefully we have a better interface for this soon ala Brendan

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
  // 🍞🥖
  val bread = Reg(UInt(log2Up(elemsPerArithUnit).W))

  OAccs zip dataChannelOut foreach { case (acc, out) =>
    out.data.bits := Cat(acc.reverse)
    out.data.valid := false.B // overset later
  }
  OCache.zip(OAccs).foreach { case (ocache_row: Seq[SyncReadMem[SInt]], oacc_row: Seq[SInt]) =>
    ocache_row zip oacc_row map { case (oc: SyncReadMem[SInt], oa: SInt) =>
      val do_read = (state === s_acc) || (state === s_writeback_1)
      val cache_read = Wire(SInt(dataWidthBits.W))
      cache_read := oc.read(bread, do_read)
      // 1 cy
      when(RegNext(do_read)) {
        oa := cache_read // 2 cy
      }
    }
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
  dataChannelB.data.ready := false.B

  // after 1 cy
  val bread_val = Seq.fill(arithUnits)(Wire(SInt(dataWidthBits.W)))
  bread_val zip BCache foreach { case (bwire, mem) =>
    bwire := mem.read(bread, state === s_acc)
  }

  // avail after 2 cy
  val o_mul = current_a.flatMap { row_a =>
    bread_val map { arith_b =>
      // do this to over-ride inferred width
      val o_mul_ = Reg(SInt(dataWidthBits.W))
      o_mul_ := row_a * arith_b
      o_mul_
    }
  }


  // after 2cy, on 3rd cycle
  val o_acc = o_mul zip OAccs.flatten map { case (o_mul_dat, o_acc_old_dat) =>
    o_mul_dat + o_acc_old_dat
  }
  val addr = RegNext(RegNext(bread))
  val do_write = RegNext(RegNext(state === s_acc))
  o_acc zip OCache.flatten foreach { case (o_acc_dat, ocache) =>
    when(do_write) {
      ocache.write(addr, o_acc_dat)
    }
  }

  val on_last = bread === maxBCounter.U

  /*
   * END DATA PIPELINE
   */

  dataChannelB.data.ready := false.B
  io.resp.bits.data := 0.U
  io.resp.valid := false.B
  io.req.ready := state === s_idle &&
    (reqChannelRow map { ioa => ioa.ready } reduce (_ && _)) &&
    reqChannelB.ready &&
    (reqChannelOut map { io => io.ready } reduce (_ && _))

  val buffer_valid = Seq.fill(coreP.rowParallelism)(RegInit(false.B))

  // make sure we can pack all the addresses together into one instruction
  require(io.req.bits.rs1.getWidth + io.req.bits.rs2.getWidth >= reqChannelB.bits.addr.getWidth * 3)

  switch(state) {
    is(s_idle) {
      when(io.req.fire) {
        // get ready to load in row of matrix B
        val inst = io.req.bits.inst
        switch(inst.rs1) {
          is(0.U) {
            // BEGIN COMPUTATION
            currentBRow := 1.U
            b_loadidx := 0.U
            b_loaded := false.B
            realRowLength := io.req.bits.rs2(realRowBits - 1, 0)
            ACounter := 0.U
            arithCounter := 0.U
            state := s_addr_comp
          }
          is(1.U) {
            // STORE A_BASE
            AAddrs(0) := io.req.bits.rs2(addrWidth - 1, 0)
          }
          is(2.U) {
            // STORE B_BASE
            BAddr := io.req.bits.rs2(addrWidth - 1, 0)
            BSave := io.req.bits.rs2(addrWidth - 1, 0)
          }
          is(3.U) {
            // STORE OUTPUT_BASE
            OutputAddr(0) := io.req.bits.rs2(addrWidth - 1, 0)
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
            AAddrs(idx) := AAddrs(idx - 1) + Cat(realRowLength, 0.U(log2Up(dataWidthBytes).W))
            OutputAddr(idx) := OutputAddr(idx - 1) + Cat(realRowLength, 0.U(log2Up(dataWidthBytes).W))
          }
        }
      }
      when(currentBRow === coreP.rowParallelism.U) {
        currentBRow := 0.U
        state := s_addr_wait_buffers
        reqChannelOut foreach { req =>
          req.valid := true.B
        }
      }
      // get ready to load in subset of row(s) of matrix A
      row_loaded foreach {
        _ := false.B
      }
    }
    is(s_addr_wait_buffers) {
      val output_channels_ready = reqChannelRow map (_.ready) reduce (_ && _)
      when(output_channels_ready) {
        state := s_load_buffer1
      }
    }
    is(s_load_buffer1) {
      reqChannelRow zip OutputAddr foreach { case (io, oaddr) =>
        io.bits.addr := oaddr
        io.valid := true.B
      }
      bread := 0.U
      state := s_load_buffer2
    }
    is(s_load_buffer2) {
      when(dataChannelRow map (_.data.valid) reduce (_ && _)) {
        when(!arithCounter.andR) {
          arithCounter := arithCounter + 1.U
        }.otherwise {
          arithCounter := 0.U
          when(on_last) {
            bread := 0.U
            state := s_addr_wait_AB
          }.otherwise {
            bread := bread + 1.U
          }
        }
        dataChannelRow.foreach(_.data.ready := true.B)
        dataChannelRow map (_.data.bits) zip OCache foreach { case (dat, cache) =>
          cache.zipWithIndex foreach { case (arithcache, idx) =>
            when(idx.U === arithCounter) {
              arithcache.write(bread, dat.asSInt)
            }
          }
        }
      }
    }

    is(s_addr_wait_AB) {
      val inputs_ready = reqChannelB.ready && reqChannelRow.map(_.ready).reduce(_ && _)
      when(inputs_ready) {
        state := s_addr_commit
      }
    }

    is(s_addr_commit) {
      reqChannelB.valid := true.B
      reqChannelB.bits.addr := BAddr
      reqChannelRow zip AAddrs foreach { case (reqChan, aaddr) =>
        reqChan.bits.addr := aaddr
        reqChan.valid := true.B
      }
      BAddr := BAddr + realRowBytes
      state := s_load
    }
    is(s_load) {
      // load single element of  matrix A
      dataChannelRow.lazyZip(row_loaded).lazyZip(current_a) foreach { case (in_a, aload, areg) =>
        in_a.data.ready := !aload
        when(in_a.data.fire) {
          areg := in_a.data.bits.asSInt
          aload := true.B
        }
      }

      // load matrix B
      dataChannelB.data.ready := !b_loaded
      when(dataChannelB.data.fire) {
        b_loadidx := b_loadidx + 1.U
        BCache zip splitPayload(dataChannelB.data.bits, dataWidthBits) foreach { case (cache, dat) =>
          cache.write(b_loadidx, dat.asSInt)
        }
        when(b_loadidx === maxBCounter.U) {
          b_loaded := true.B
        }
      }

      when(b_loaded && all_rows_loaded) {
        state := s_acc
        bread := 0.U
      }
      when(!on_last) {
        bread := bread + 1.U
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
          b_loadidx := 0.U
          b_loaded := false.B
          currentBRow := currentBRow + 1.U
          row_loaded foreach {
            _ := false.B
          }
          state := s_addr_commit
          assert(dataChannelB.finished)
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
      dataChannelOut.foreach(_.finished := true.B)
      when(dataChannelOut.map(_.stop) reduce (_ && _)) {
        when(ACounter === lastA.U) {
          io.resp.valid := true.B
          io.resp.bits.data := 1.U
          when(io.resp.fire) {
            state := s_idle
            ACounter := 0.U
          }
        }.otherwise {
          currentBRow := 1.U
          b_loadidx := 0.U
          b_loaded := false.B
          state := s_addr_comp
          ACounter := ACounter + 1.U
          AAddrs(0) := AAddrs.last + rowSizeBytes.U
          OutputAddr(0) := OutputAddr.last + rowSizeBytes.U
          BAddr := BSave
        }
      }
    }
  }
}

class WithGemm(withNCores: Int,
               gp: GemmParam) extends Config((site, _, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
    coreParams = ComposerCoreParams(
      // one for B, one for each row of A
      readChannelParams = Seq.fill(1 + gp.rowParallelism)(ComposerChannelParams()),
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

class GemmConfig extends Config(
  new WithGemm(4, GemmParam(4, 1024, 8, 4, 4096)) ++ new WithVectorAdder(1, 16) ++
    new WithALUs(1) ++ new WithComposer() ++ new WithAWSMem(4)
)
