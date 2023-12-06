import chisel3._
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import composer.Protocol.AXI4Compat
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
  val mem = IO(new AXI4Compat(param = MasterPortParams(
    base = 0,
    size = 1L << memAddrBits,
    beatBytes = memBusWidth / 8,
    idBits = 6)))

  val io = IO(Flipped(new AXI4Compat(param = MasterPortParams(
    base = 0,
    size = 1L << ioAddrBits,
    beatBytes = hostBusWidth / 8,
    idBits = 6))))

  val s_idle :: s_collect :: s_io_write_response :: s_doRead :: s_emitRead :: s_sync :: Nil = Enum(6)
  val w_idle :: w_active :: w_copy :: Nil = Enum(3)
  io.initFromSlaveLow()
  mem.initFromMasterLow()
  val addr = Reg(UInt(32.W))
  mem.arburst := 1.U
  mem.awburst := 1.U
  mem.araddr := addr
  mem.bready := true.B

  // memory write bus is handled by FIFO only because it's going to be complicated to do stalling and I don't feel like
  // it. FIFO accomplishes basically the same thing (with slightly more resource consumption :/)
  // Enqueue port default off
  // Tie up deq port to AXI write bus
  val writeCounter = Reg(UInt(log2Up(txLen).W))
  val writerFifo = Module(new Queue(mem.wdata.cloneType, txLen, pipe = false, flow = false))
  writerFifo.io.enq.bits := DontCare
  writerFifo.io.enq.valid := false.B
  mem.wvalid := writerFifo.io.deq.valid
  mem.wlast := writeCounter === (txLen - 1).U
  mem.wstrb := VecInit(Seq.fill(mem.wstrb.getWidth)(true.B)).asUInt
  mem.wdata := writerFifo.io.deq.bits
  writerFifo.io.deq.ready := mem.wready
  when(mem.wready && mem.wvalid) {
    writeCounter := writeCounter + 1.U
  }
  mem.arlen := (txLen - 1).U
  mem.awlen := (txLen - 1).U

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
  when(mem.bvalid) {
    writeInFlight(mem.bid) := false.B
  }
  val writeEmitApproval = PriorityEncoderOH(writeEmitDesire)
  val writeLock = RegInit(false.B)

  Seq(isSyncings, isIdles, readEmitDesire, writeEmitDesire) foreach { vec =>
    vec.foreach(_ := false.B)
  }
  val isSync = isSyncings.fold(true.B)(_ && _)
  val allCoresIdle = isIdles.fold(true.B)(_ && _)

  val beatPerAddr = (mem.addrBits.toFloat / io.wdata.getWidth).ceil.toInt
  val base_addr_vec = Reg(Vec(beatPerAddr, UInt(mem.addrBits.W)))
  val base_addr = Cat(base_addr_vec.reverse)

  println("memAddrWidth is " + mem.addrBits + ", ioWidth is " + io.wdata.getWidth)
  println("Address for GO signal is " + (1 << log2Up(beatPerAddr)))
  println("Need " + beatPerAddr + " beats per address")
  println("Need total allocation size: " + (totalMemcpySizeBytes * 2 * nCores) + " bytes")
  println("pwd is " + System.getProperty("user.dir"))

  val r_idle :: r_resp :: Nil = Enum(2)
  val r_state = RegInit(r_idle)
  val r_id = Reg(UInt(io.arid.getWidth.W))
  val r_ndataBits = log2Up(nCores)
  val r_addr = Reg(UInt(r_ndataBits.W))
  val r_expectedBeats = Reg(UInt(io.arlen.getWidth.W))

  val w_id = Reg(UInt(io.bid.getWidth.W))
  val w_addr = Reg(UInt((log2Up(beatPerAddr) + 1).W))


  // Read machine is global, write machine is per-core
  io.rid := r_id
  io.bid := w_id
  io.rdata := cycleHolds(r_addr)
  when(r_state === r_idle) {
    io.arready := true.B
    when(io.arvalid) {
      r_state := r_resp
      r_id := io.arid
      r_addr := (io.araddr >> 2)
      r_expectedBeats := io.arlen
    }
  }.elsewhen(r_state === r_resp) {
    io.rvalid := true.B
    io.rlast := r_expectedBeats === 0.U
    when(io.rready) {
      when(io.rlast) {
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
    val cycleCounter = Reg(UInt(hostBusWidth.W))

    val maxStorage = readLookaheadTxs * txLen
    val storageIdxBits = log2Up(maxStorage)
    val myMem = SyncReadMem(maxStorage, UInt(io.rdata.getWidth.W))
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

    val readAddrReg, writeAddrReg = Reg(mem.araddr.cloneType)
    val txBytes = txLen * mem.rdata.getWidth / 8
    val totalNTxs = totalMemcpySizeBytes / txBytes
    val readTxCounter, writeTxCounter = RegInit(0.U((log2Up(totalNTxs)+1).W))
    val wasGO = Reg(Bool())

    cycleCounter := cycleCounter + 1.U
    when(state === s_idle) {
      isIdles(coreIdx) := true.B
      readTxCounter := 0.U
      writeTxCounter := 0.U
      readAddrReg := readBaseAddr
      writeAddrReg := writeBaseAddr
      when(allCoresIdle) {
        io.awready := mem.arready
        when(io.awready && io.awvalid) {
          if (coreIdx == 0) w_id := io.awid
          state := s_collect
          // has to be 4-byte aligned from software side. Unalign for vec idx
          w_addr := (io.awaddr >> 2).asUInt
        }
      }
    }.elsewhen(state === s_collect) {
      io.wready := true.B
      // only transition off wlast signal. We don't know how ARM is going to instrument MMIO stores so just pay
      // attention to the last beat (for data, since that'll be the lowest order)
      val isGO = w_addr === (1 << log2Up(beatPerAddr)).U
      when(io.wready && io.wvalid) {
        if (coreIdx == 0) {
          when(!isGO) {
            base_addr_vec(w_addr(log2Up(beatPerAddr) - 1, 0)) := io.wdata
          }
        }
        when(io.wlast) {
          wasGO := isGO
          state := s_io_write_response
        }
      }
    }.elsewhen(state === s_io_write_response) {
      cycleCounter := 0.U
      if (coreIdx == 0) io.bvalid := true.B
      when(io.bready) {
        when(wasGO) {
          state := s_emitRead
          writeState := w_active
        }.otherwise {
          state := s_idle
        }
      }
      cycleCounter := 0.U
    }.elsewhen(state === s_emitRead) {
      // if we have room in our cache, then signal that we're interested in emitting a read command
      readEmitDesire(coreIdx) := !readEmitStall
      when(readEmitApproval(coreIdx)) {
        mem.arvalid := true.B
        mem.araddr := readAddrReg
        mem.arid := coreIdx.U
        when(mem.arready) {
          readEmitHead := readEmitHead + txLen.U
          readTxCounter := readTxCounter + 1.U
          state := s_doRead
        }
      }
    }.elsewhen(state === s_doRead) {
      mem.rready := true.B
      when(mem.rvalid && mem.rid === coreIdx.U) {
        myMem.write(readHead, mem.rdata)
        readHead := readHead + 1.U
        when(readHead.asBools.reduce(_ && _)) {
          readParity := readParity + 1.U
        }

        when(mem.rlast) {
          when(readTxCounter === totalNTxs.U) {
            state := s_sync
            cycleHolds(coreIdx) := cycleCounter
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
        mem.awaddr := writeAddrReg
        mem.awvalid := true.B
        mem.awid := coreIdx.U
        when(mem.awready) {
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
  val memBusWidth = 128
  val hostBusWidth = 32
  val txLen = 16
  val nCores = 4
  val totalMemcpySizeBytes = 1024*32
  val readLookaheadTxs = 4
  val kriaMemAddrBits = 49
  val kriaIOAddrBits = 40

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
      kriaMemAddrBits,
      kriaIOAddrBits
    ))))
}