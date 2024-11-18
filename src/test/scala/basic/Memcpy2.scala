package basic

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import firrtl.transforms.NoConstantPropagationAnnotation
import firrtl.{AnnotationSeq, CustomDefaultMemoryEmission, CustomDefaultRegisterEmission, MemoryNoInit}
import freechips.rocketchip.amba.axi4.AXI4BundleParameters

import scala.annotation.tailrec

object SHREG {
  @tailrec
  def apply[T <: Data](a: T, l: Int): T = {
    l match {
      case 0 => a
      case _ => SHREG(RegNext(a), l - 1)
    }
  }
}

class MemoryFIFOUT(n: Int, w: Int, l: Int) extends Module {
  val addr_w = log2Up(n)
  require(n >= 64 && n % 64 == 0)
  val io = IO(new Bundle {
    val w_en = Input(Bool())
    val w_addr = Input(UInt(addr_w.W))
    val w_data = Input(UInt(w.W))

    val r_prime_valid = Input(Bool())
    val r_prime_addr = Input(UInt((addr_w - 6).W))
    val r_prime_ready = Output(Bool())

    val r_valid_out = Output(Bool())
    val r_data_out = Output(UInt(w.W))
    val r_ready_out = Input(Bool())
  })

  val mem = SyncReadMem(n, UInt(w.W))

  when(io.w_en) {
    mem(io.w_addr) := io.w_data
  }

  val outQueue = Module(new Queue(UInt(w.W), 64, useSyncReadMem = true))
  val amReading = RegInit(false.B)
  val r_idx = Reg(UInt(addr_w.W))
  io.r_prime_ready := !amReading && !outQueue.io.deq.valid
  when(io.r_prime_ready && io.r_prime_valid) {
    amReading := true.B
    r_idx := Cat(io.r_prime_addr, 0.U(6.W))
  }

  val shreg_in = mem.read(r_idx, amReading)
  val shreg_out = SHREG(shreg_in, l-1)
  val shregv_in = WireInit(false.B)
  val shregv_out = SHREG(shregv_in, l)

  when(amReading) {
    r_idx := r_idx + 1.U
    shregv_in := true.B
    when(r_idx(5, 0) === 0x3F.U) {
      amReading := false.B
    }
  }
  outQueue.io.enq.valid := shregv_out
  outQueue.io.enq.bits := shreg_out
  when(outQueue.io.enq.valid) {
    assert(outQueue.io.enq.ready)
  }

  io.r_data_out := outQueue.io.deq.bits
  io.r_valid_out := outQueue.io.deq.valid
  outQueue.io.deq.ready := io.r_ready_out
}

class BeethovenTop(n_parallel: Int,
                   n_slots: Int) extends Module {
  require(n_slots >= 2 * n_parallel, "dont make sense!")
  val M00 = IO(beethoven.Protocol.AXI.AXI4Compat(
    AXI4BundleParameters(
      addrBits = 64,
      dataBits = 512,
      idBits = 16
    )))
  M00.initFromMasterLow()

  val S00 = IO(Flipped(beethoven.Protocol.AXI.AXI4Compat(
    AXI4BundleParameters(
      addrBits = 32,
      dataBits = 32,
      idBits = 1
    ))))
  S00.initFromSlaveLow()
  val s_idle :: s_write_exch :: s_write_response :: s_emit_read :: s_wait :: Nil = Enum(5)
  val id_bits = log2Up(n_parallel)
  val slotidx_bits = log2Up(n_slots)
  val n_read_txs = Reg(UInt(32.W))
  val n_write_resps = Reg(UInt(32.W))
  val writes_to_do = Module(new Queue(UInt(id_bits.W), n_slots))
  val slots_to_walloc = Module(new Queue(UInt(slotidx_bits.W), n_slots))
  slots_to_walloc.io.enq.valid := false.B
  slots_to_walloc.io.enq.bits := DontCare
  val state = RegInit(s_idle)

  val readID2slot = Reg(Vec(n_parallel, UInt(slotidx_bits.W)))
  val slotProgress = Reg(Vec(n_slots, UInt(6.W)))

  val readAlloc = Module(new AllocSource(n_parallel))
  val writeAlloc = Module(new AllocSource(n_parallel))
  val slotAlloc = Module(new AllocSource(n_slots))
  Seq(readAlloc, writeAlloc, slotAlloc) foreach { alloc =>
    alloc.io.alloc_in := false.B
    alloc.io.free_in := false.B
    alloc.io.free_idx := DontCare
  }

  val have_alloced_read = Reg(Bool())
  val have_alloced_write = Reg(Bool())
  val have_alloced_slot = Reg(Bool())
  val alloced_slot = Reg(UInt(slotidx_bits.W))
  val alloced_read = Reg(UInt(id_bits.W))

  // 64beats * 64B = 4KB = 12b
  val addr = Reg(UInt((64 - 12).W))
  val slot2Addr = Reg(Vec(n_slots, UInt((64 - 12).W)))
  M00.arburst := 0.U
  M00.arsize := 6.U
  M00.arlen := 63.U

  val ROB = Module(new MemoryFIFOUT(64 * n_slots, 512, 3))
  ROB.io.r_prime_valid := false.B
  ROB.io.r_ready_out := false.B
  ROB.io.r_prime_addr := DontCare
  M00.rready := true.B
  ROB.io.w_en := RegNext(M00.rvalid)
  ROB.io.w_data := RegNext(M00.rdata)
  ROB.io.w_addr := RegNext(slotProgress(readID2slot(M00.rid)))
  readAlloc.io.free_idx := M00.rid
  when(M00.rvalid && M00.rready) {
    val idx = readID2slot(M00.rid)
    slotProgress(idx) := slotProgress(idx) + 1.U
    when(slotProgress(idx) === 63.U) {
      slots_to_walloc.io.enq.valid := true.B
      slots_to_walloc.io.enq.bits := idx
      readAlloc.io.free_in := true.B
    }
  }

  val writeIdx2slot = Reg(Vec(n_parallel, UInt(slotidx_bits.W)))

  writes_to_do.io.enq.valid := slots_to_walloc.io.deq.valid && writeAlloc.io.valid_out
  writes_to_do.io.enq.bits := writeAlloc.io.idx_out
  writeAlloc.io.alloc_in := slots_to_walloc.io.deq.valid
  slots_to_walloc.io.deq.ready := writeAlloc.io.valid_out
  when(writes_to_do.io.enq.fire) {
    writeIdx2slot(writeAlloc.io.idx_out) := slots_to_walloc.io.deq.bits
  }

  val w_addr :: w_data :: Nil = Enum(2)
  val w_state = RegInit(w_addr)

  writes_to_do.io.deq.ready := false.B
  M00.awlen := 63.U
  M00.awsize := 6.U

  val write_can_go = ROB.io.r_prime_ready

  slotAlloc.io.free_idx := writeIdx2slot(M00.bid)
  M00.bready := true.B
  slotAlloc.io.free_in := M00.bvalid

  writeAlloc.io.free_idx := M00.bid
  writeAlloc.io.free_in := M00.bvalid

  val cycleCounter = Reg(UInt(37.W))
  when(S00.awvalid && S00.awready) {
    cycleCounter := 0.U
  }
  when(state =/= s_idle) {
    cycleCounter := cycleCounter + 1.U
  }
  when(M00.bvalid) {
    n_write_resps := n_write_resps - 1.U
    when(n_write_resps === 1.U) {
      state := s_idle
    }
  }

  val do_read = RegInit(false.B)
  S00.arready := !do_read
  when(S00.arvalid) {
    do_read := true.B
  }
  S00.rvalid := do_read
  S00.rdata := cycleCounter(36, 5) // cycles / 1024
  S00.rid := RegNext(S00.arid)
  S00.rlast := true.B
  when(S00.rvalid && S00.rready) {
    do_read := false.B
  }

  val cnt = Reg(UInt(6.W))
  M00.awaddr := Cat(slot2Addr(writeIdx2slot(writes_to_do.io.deq.bits)), 0.U(12.W)) | (1L << 30).U

  when(w_state === w_addr) {
    writes_to_do.io.deq.ready := M00.awready && write_can_go
    M00.awvalid := writes_to_do.io.deq.valid && write_can_go
    M00.awid := writes_to_do.io.deq.bits
    when(M00.awvalid && M00.awready) {
      w_state := w_data
      ROB.io.r_prime_valid := true.B
      ROB.io.r_prime_addr := slot2Addr(writeIdx2slot(writes_to_do.io.deq.bits))
      cnt := 0.U
    }
  }.elsewhen(w_state === w_data) {
    M00.wvalid := ROB.io.r_valid_out
    ROB.io.r_ready_out := M00.wready
    M00.wdata := ROB.io.r_data_out
    M00.wlast := cnt === 63.U

    when(M00.wready && M00.wvalid) {
      cnt := cnt + 1.U
      when(M00.wlast && M00.wvalid && M00.wready) {
        w_state := w_addr
      }
    }
  }

  when(state === s_idle) {
    S00.awready := true.B
    when(S00.awvalid) {
      state := s_write_exch
      addr := 0.U
    }
  }.elsewhen(state === s_write_exch) {
    S00.wready := true.B
    when(S00.wvalid) {
      n_read_txs := S00.wdata
      n_write_resps := S00.wdata
      state := s_write_response
      have_alloced_read := false.B
      have_alloced_slot := false.B
    }
  }.elsewhen(state === s_write_response) {
    S00.bvalid := true.B
    when(S00.bready) {
      state := s_emit_read
    }
  }.elsewhen(state === s_emit_read) {
    slotAlloc.io.alloc_in := !have_alloced_slot
    readAlloc.io.alloc_in := !have_alloced_read
    when(slotAlloc.io.valid_out && !have_alloced_slot) {
      have_alloced_slot := true.B
      alloced_slot := slotAlloc.io.idx_out
    }
    when(readAlloc.io.valid_out && !have_alloced_read) {
      have_alloced_read := true.B
      alloced_read := readAlloc.io.idx_out
    }
    when(have_alloced_read && have_alloced_slot) {
      M00.arvalid := true.B
      M00.araddr := Cat(addr, 0.U(12.W))
      M00.arid := alloced_read
      readID2slot(alloced_read) := alloced_slot
      when(M00.arready) {
        slot2Addr(alloced_slot) := addr
        addr := addr + 1.U
        n_read_txs := n_read_txs - 1.U
        have_alloced_read := false.B
        have_alloced_slot := false.B
        when(n_read_txs === 1.U) {
          state := s_wait
        }
      }
    }
  }.elsewhen(state === s_wait) {
    // wait!
  }
}

object BeethovenTop {
  def main(args: Array[String]): Unit = {
    (new ChiselStage).emitVerilog(new BeethovenTop(16, 64), annotations = AnnotationSeq(Seq(
      CustomDefaultMemoryEmission(MemoryNoInit),
      CustomDefaultRegisterEmission(useInitAsPreset = false, disableRandomization = true),
//      NoConstantPropagationAnnotation
    )))
  }
}