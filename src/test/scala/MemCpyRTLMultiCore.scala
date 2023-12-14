import chisel3._
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import composer.Protocol.AXI4Compat
import composer.common.CLog2Up
import freechips.rocketchip.subsystem.MasterPortParams


class MemCpyRTLMultiCore(memBusWidth: Int,
                         hostBusWidth: Int,
                         txLen: Int,
                         nCores: Int,
                         totalMemcpySizeBytes: Int,
                         readLookaheadTxs: Int,
                         memAddrBits: Int,
                         ioAddrBits: Int) extends Module {
  require(readLookaheadTxs >= 1)
  val M00_AXI = IO(new AXI4Compat(param = MasterPortParams(
    base = 0,
    size = 1L << memAddrBits,
    beatBytes = memBusWidth / 8,
    idBits = 6)))

  val S00_AXI = IO(Flipped(new AXI4Compat(param = MasterPortParams(
    base = 0,
    size = 1L << ioAddrBits,
    beatBytes = hostBusWidth / 8,
    idBits = 6))))

  val s_idle :: s_collect :: s_io_write_response :: s_doRead :: s_emitRead :: s_sync :: Nil = Enum(6)
  val w_idle :: w_active :: w_copy :: Nil = Enum(3)
  S00_AXI.initFromSlaveLow()
  M00_AXI.initFromMasterLow()
  val addr = Reg(UInt(32.W))
  M00_AXI.arburst := 1.U
  M00_AXI.awburst := 1.U
  M00_AXI.araddr := addr
  M00_AXI.bready := true.B

  // memory write bus is handled by FIFO only because it's going to be complicated to do stalling and I don't feel like
  // it. FIFO accomplishes basically the same thing (with slightly more resource consumption :/)
  // Enqueue port default off
  // Tie up deq port to AXI write bus
  val writeCounter = Reg(UInt(log2Up(txLen).W))
  val writerFifo = Module(new Queue(M00_AXI.wdata.cloneType, txLen, pipe = false, flow = false))
  writerFifo.io.enq.bits := DontCare
  writerFifo.io.enq.valid := false.B
  M00_AXI.wvalid := writerFifo.io.deq.valid
  M00_AXI.wlast := writeCounter === (txLen - 1).U
  M00_AXI.wstrb := VecInit(Seq.fill(M00_AXI.wstrb.getWidth)(true.B)).asUInt
  val topBit = M00_AXI.wdata.getWidth - 1
  val cycleCounter = Reg(UInt(32.W))
  cycleCounter := cycleCounter + 1.U
  M00_AXI.wdata := Cat(writerFifo.io.deq.bits(topBit, 32), cycleCounter)
  writerFifo.io.deq.ready := M00_AXI.wready
  when(M00_AXI.wready && M00_AXI.wvalid) {
    writeCounter := writeCounter + 1.U
  }
  M00_AXI.arlen := (txLen - 1).U
  M00_AXI.awlen := (txLen - 1).U

  val cycleHolds = Reg(Vec(nCores, UInt(hostBusWidth.W)))

  val isSyncings = Wire(Vec(nCores, Bool()))
  val isIdles = Wire(Vec(nCores, Bool()))
  val readEmitDesire = Wire(Vec(nCores, Bool()))
  val readEmitApproval = PriorityEncoderOH(readEmitDesire)
  val writeEmitDesire = Wire(Vec(nCores, Bool()))
  val writeInFlight = Reg(Vec(nCores, Bool()))
  when(reset.asBool) {
    writeInFlight.foreach(_ := false.B)
  }
  when(M00_AXI.bvalid) {
    writeInFlight(M00_AXI.bid) := false.B
  }
  val writeEmitApproval = PriorityEncoderOH(writeEmitDesire)
  val writeLock = RegInit(false.B)

  Seq(isSyncings, isIdles, readEmitDesire, writeEmitDesire) foreach { vec =>
    vec.foreach(_ := false.B)
  }
  val isSync = isSyncings.fold(true.B)(_ && _)
  val allCoresIdle = isIdles.fold(true.B)(_ && _)

  val beatPerAddr = (M00_AXI.addrBits.toFloat / S00_AXI.wdata.getWidth).ceil.toInt
  val base_addr_vec = Reg(Vec(beatPerAddr, UInt(M00_AXI.addrBits.W)))
  val base_addr = Cat(base_addr_vec.reverse)

  println("memAddrWidth is " + M00_AXI.addrBits + ", ioWidth is " + S00_AXI.wdata.getWidth)
  println("Address for GO signal is " + (1 << log2Up(beatPerAddr)))
  println("Need " + beatPerAddr + " beats per address")
  println("Need total allocation size: " + (totalMemcpySizeBytes * 2 * nCores) + " bytes")
  println("pwd is " + System.getProperty("user.dir"))

  val r_idle :: r_resp :: Nil = Enum(2)
  val r_state = RegInit(r_idle)
  val r_id = Reg(UInt(S00_AXI.arid.getWidth.W))
  val r_ndataBits = log2Up(nCores)
  val r_addr = Reg(UInt(r_ndataBits.W))
  val r_expectedBeats = Reg(UInt(S00_AXI.arlen.getWidth.W))

  val w_id = Reg(UInt(S00_AXI.bid.getWidth.W))
  val w_addr = Reg(UInt((log2Up(beatPerAddr) + 1).W))


  // Read machine is global, write machine is per-core
  S00_AXI.rid := r_id
  S00_AXI.bid := w_id
  S00_AXI.rdata := cycleHolds(r_addr)
  val nCoreIdxBits = CLog2Up(nCores)
  when(r_state === r_idle) {
    S00_AXI.arready := true.B
    when(S00_AXI.arvalid) {
      r_state := r_resp
      r_id := S00_AXI.arid
      r_addr := (if (nCores == 1) 0.U else (S00_AXI.araddr >> 2)(nCoreIdxBits - 1, 0))
      r_expectedBeats := S00_AXI.arlen
    }
  }.elsewhen(r_state === r_resp) {
    S00_AXI.rvalid := true.B
    S00_AXI.rlast := r_expectedBeats === 0.U
    when(S00_AXI.rready) {
      when(S00_AXI.rlast) {
        r_state := r_idle
      }.otherwise {
        r_expectedBeats := r_expectedBeats - 1.U
        r_addr := r_addr + 1.U
      }
    }
  }

  (0 until nCores) foreach { coreIdx =>
    // each core has its own state machine
    val state = RegInit(s_idle)
    val writeState = RegInit(w_idle)

    val maxStorage = readLookaheadTxs * txLen
    val storageIdxBits = log2Up(maxStorage)
    val myMem = SyncReadMem(maxStorage, UInt(S00_AXI.rdata.getWidth.W))
    val readHead, readEmitHead, writeHead = RegInit(0.U(storageIdxBits.W))
    // keep track of roll around
    val readParity, writeParity = RegInit(0.U(1.W))
    val writeStall = Wire(Bool())
    val readEmitStall = Wire(Bool())
    when (readParity === writeParity) {
      writeStall := (readHead - writeHead) < txLen.U
      readEmitStall := (readEmitHead - writeHead) >= txLen.U
    }.otherwise {
      writeStall := false.B
      readEmitStall := (writeHead - readEmitHead) >= txLen.U
    }
    require(maxStorage % txLen == 0)
    val readBaseAddr = base_addr + (totalMemcpySizeBytes * 2 * coreIdx).U
    val writeBaseAddr = base_addr + (totalMemcpySizeBytes * (coreIdx * 2 + 1)).U

    val readAddrReg, writeAddrReg = Reg(M00_AXI.araddr.cloneType)
    val txBytes = txLen * M00_AXI.rdata.getWidth / 8
    val totalNTxs = totalMemcpySizeBytes / txBytes
    if (coreIdx == 0) println("Total Number Txs: " + totalNTxs)
    val writeLen = Reg(UInt(4.W))
    val readTxCounter, writeTxCounter = RegInit(0.U((log2Up(totalNTxs)+1).W))
    val wasGO = Reg(Bool())

    val cycleCounterPerCore = Reg(UInt(32.W))
    cycleCounterPerCore := cycleCounterPerCore + 1.U

    when(state === s_idle) {
      isIdles(coreIdx) := true.B
      readTxCounter := 0.U
      writeTxCounter := 0.U
      readAddrReg := readBaseAddr
      writeAddrReg := writeBaseAddr
      when(allCoresIdle) {
        S00_AXI.awready := M00_AXI.arready
        when(S00_AXI.awready && S00_AXI.awvalid) {
          if (coreIdx == 0) w_id := S00_AXI.awid
          state := s_collect
          // TODAY I LEARNED: wlast is not required! It's only required to be used if the transaction quits early
          // So, you still need to count beats and cannot rely on wlast being asserted
          writeLen := S00_AXI.awlen
          // has to be 4-byte aligned from software side. Unalign for vec idx
          w_addr := (S00_AXI.awaddr >> 2).asUInt
        }
      }
    }.elsewhen(state === s_collect) {
      S00_AXI.wready := true.B
      // only transition off wlast signal. We don't know how ARM is going to instrument MMIO stores so just pay
      // attention to the last beat (for data, since that'll be the lowest order)
      val isGO = w_addr === (1 << log2Up(beatPerAddr)).U
      when(S00_AXI.wready && S00_AXI.wvalid) {
        if (coreIdx == 0) {
          when(!isGO) {
            base_addr_vec(w_addr(log2Up(beatPerAddr) - 1, 0)) := S00_AXI.wdata
          }
        }
        writeLen := writeLen - 1.U
        when(S00_AXI.wlast || writeLen === 0.U) {
          wasGO := isGO
          state := s_io_write_response
        }
      }
    }.elsewhen(state === s_io_write_response) {
      cycleCounterPerCore := 0.U
      cycleCounter := 0.U
      if (coreIdx == 0) S00_AXI.bvalid := true.B
      when(S00_AXI.bready) {
        when(wasGO) {
          state := s_emitRead
          writeState := w_active
        }.otherwise {
          state := s_idle
        }
      }
      cycleCounterPerCore := 0.U
    }.elsewhen(state === s_emitRead) {
      // if we have room in our cache, then signal that we're interested in emitting a read command
      readEmitDesire(coreIdx) := !readEmitStall
      when(readEmitApproval(coreIdx)) {
        M00_AXI.arvalid := true.B
        M00_AXI.araddr := readAddrReg
        M00_AXI.arid := coreIdx.U
        when(M00_AXI.arready) {
          readEmitHead := readEmitHead + txLen.U
          readTxCounter := readTxCounter + 1.U
          state := s_doRead
        }
      }
    }.elsewhen(state === s_doRead) {
      M00_AXI.rready := true.B
      when(M00_AXI.rvalid && M00_AXI.rid === coreIdx.U) {
        myMem.write(readHead, M00_AXI.rdata)
        readHead := readHead + 1.U
        when(readHead.asBools.reduce(_ && _)) {
          readParity := readParity + 1.U
        }

        when(M00_AXI.rlast) {
          when(readTxCounter === totalNTxs.U) {
            state := s_sync
            cycleHolds(coreIdx) := cycleCounterPerCore
          }.otherwise {
            state := s_emitRead
          }
        }
      }
    }.elsewhen(state === s_sync) {
      isSyncings(coreIdx) := writeState === w_idle
      when(isSync) {
        state := s_idle
      }
    }

    val writeMemReadCnt, writeMemDeqCnt = Reg(UInt((log2Up(txLen)+1).W))
    val doRead = Wire(Bool())
    doRead := false.B
    val wd = myMem.read(writeHead, doRead)
    val wv = RegNext(doRead)
    require(txLen >= 4)
    when(writeState === w_idle) {
    }.elsewhen(writeState === w_active) {
      when(!writeStall) {
        writeEmitDesire(coreIdx) := true.B
      }
      when(writeEmitApproval(coreIdx) === true.B && !writeLock) {
        M00_AXI.awaddr := writeAddrReg
        M00_AXI.awvalid := true.B
        M00_AXI.awid := coreIdx.U
        when(M00_AXI.awready) {
          writeInFlight(coreIdx) := true.B
          writeLock := true.B
          writeState := w_copy
          writeCounter := 0.U
          writeMemReadCnt := 1.U
          writeHead := writeHead + 1.U
          doRead := true.B
          writeMemDeqCnt := 0.U
        }
      }
    }.elsewhen(writeState === w_copy) {
      writerFifo.io.enq.valid := wv
      writerFifo.io.enq.bits := wd
      doRead := writeMemReadCnt < txLen.U
      when(doRead) {
        writeMemReadCnt := writeMemReadCnt + 1.U
        writeHead := writeHead + 1.U
        when (writeHead.asBools.reduce(_ && _)) {
          writeParity := writeParity + 1.U
        }
      }
      when(writerFifo.io.deq.fire) {
        writeMemDeqCnt := writeMemDeqCnt + 1.U
      }
      when(writeMemDeqCnt === txLen.U){
        writeLock := false.B
        writeTxCounter := writeTxCounter + 1.U
        writeMemDeqCnt := 0.U
        when(writeTxCounter === (totalNTxs - 1).U) {
          writeState := w_idle
        }.otherwise {
          writeState := w_active
        }
      }

    }
  }
}

object memcpyBuild extends App {
//  // kria
//  val memBusWidth = 128
//  val hostBusWidth = 32
//  val txLen = 16
//  val nCores = 4
//  val totalMemcpySizeBytes = 1024*32
//  val readLookaheadTxs = 4
//  val kriaMemAddrBits = 49
//  val kriaIOAddrBits = 40

  // aws f1 - u200
  val memBusWidth = 512
  val hostBusWidth = 32
  val txLen = 16
  val nCores = 16
  val totalMemcpySizeBytes = 1024*32
  val readLookaheadTxs = 4
  val memAddrBits = 34
  val IOAddrBits = 10

  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog",
      // disable memory intiialization
      "--emission-options=disableMemRandomization,disableRegisterRandomization"),
    Seq(ChiselGeneratorAnnotation(() => new MemCpyRTLMultiCore(
      memBusWidth,
      hostBusWidth,
      txLen,
      nCores,
      totalMemcpySizeBytes,
      readLookaheadTxs,
      memAddrBits,
      IOAddrBits
    ))))
}