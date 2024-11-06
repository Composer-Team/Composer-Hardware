package basic

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import firrtl.{AnnotationSeq, CustomDefaultMemoryEmission, CustomDefaultRegisterEmission, MemoryNoInit}
import freechips.rocketchip.amba.axi4.AXI4BundleParameters

class AllocSource(n: Int) extends Module {
  val io = IO(new Bundle {
    val alloc_in = Input(Bool())

    val valid_out = Output(Bool())
    val idx_out = Output(UInt(log2Up(n).W))

    val free_in = Input(Bool())
    val free_idx = Input(UInt(log2Up(n).W))
  })

  val idle = Reg(Vec(n, Bool()))
  io.valid_out := Cat(idle) =/= 0.U
  val alloced = PriorityEncoder(idle)
  io.idx_out := alloced

  when (io.alloc_in) {
    idle(alloced) := false.B
  }

  when (io.free_in) {
    idle(io.free_idx) := true.B
  }

  when (reset.asBool) {
    idle foreach { _ := true.B }
  }
}

class Lock(n: Int) extends Module {
  val io = IO(new Bundle {
    val acquire_in = Input(Vec(n, Bool()))
    val acquire_out = Output(Vec(n, Bool()))
    val free = Input(Bool())

    val external_stall = Input(Bool())
  })

  val is_acquired = RegInit(false.B)
  val prevHigh = Wire(Vec(n, Bool()))
  val canAcquire = (!is_acquired || io.free) && !io.external_stall
  prevHigh(0) := false.B
  io.acquire_out(0) := canAcquire && io.acquire_in(0)
  (1 until n) foreach { idx =>
    prevHigh(idx) := prevHigh(idx - 1) || io.acquire_in(idx-1)
    io.acquire_out(idx) := !prevHigh(idx) && io.acquire_in(idx) && canAcquire
  }

  when (io.free) {
    is_acquired := false.B
  }
  when (io.acquire_out.asUInt =/= 0.U) {
    is_acquired := true.B
  }
}

class Memcpy(n_parallel: Int,
             n_slots: Int) extends Module {
  require(n_slots >= n_parallel)
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

  val read_idle = Reg(Vec(n_parallel, Bool()))
  val write_idle = Reg(Vec(n_parallel, Bool()))
  val stat_idle = RegInit(true.B)
  val m_idle :: m_get_len :: m_resp :: m_active :: Nil = Enum(4)
  val machine_state = RegInit(m_idle)
  val cycle_count = Reg(UInt(32.W))
  val cycle_counter = Reg(UInt(50.W))

  val nTxsToLaunch = RegInit(0.U(32.W))
  val nTxsToRecieve = RegInit(0.U(32.W))
  val readAddr = Reg(UInt(64.W))
  val writeMask = 0x100000000L

  val tx_addrs = Reg(Vec(n_slots, UInt(64.W)))
  val slot_idle :: slot_active :: slot_acquire:: slot_transfer :: slot_wait :: Nil = Enum(5)

  val readSourceAllocator = Module(new AllocSource(n_parallel))
  val writeSourceAllocator = Module(new AllocSource(n_parallel))
  val slotAllocator = Module(new AllocSource(n_slots))
  val writeBusLock = Module(new Lock(n_slots))
  slotAllocator.io.free_in := M00.bvalid
  slotAllocator.io.free_idx := DontCare // set later
  readSourceAllocator.io.free_in := M00.rlast
  readSourceAllocator.io.free_idx := M00.rid

  // write bus lock allows a slot to claim the write bus and write onto it a full read burst that we have in storage
  // we can only acquire the write bus lock if there is a source available
  val writeAlloc_machine = {
    writeBusLock.io.free := false.B
    writeBusLock.io.acquire_in.foreach{_ := false.B }
    writeBusLock.io.external_stall := (!M00.awready) || (!writeSourceAllocator.io.valid_out)
    writeSourceAllocator.io.alloc_in := writeBusLock.io.acquire_out.asUInt =/= 0.U
    writeSourceAllocator.io.free_in := M00.bvalid
    M00.bready := true.B
    writeSourceAllocator.io.free_idx := M00.bid
    M00.awlen := 63.U
    M00.awsize := 6.U // 2^6 bytes per beat = 64B = 512b = full line transfer
  }

  val readAlloc_machine = {
    // readsource and a slot need to be allocated at the same time
    val bothHaveAvailable = readSourceAllocator.io.valid_out && slotAllocator.io.valid_out
    val canAllocate = bothHaveAvailable && nTxsToLaunch > 0.U
    readSourceAllocator.io.alloc_in := canAllocate
    slotAllocator.io.alloc_in := canAllocate

    val readQueue = {
      val mod = Module(new Queue(
        new Bundle {
          val idx = UInt(16.W)
          val addr = UInt(64.W)
        }, n_parallel
      ))

      mod.io.enq.valid := canAllocate
      mod.io.enq.bits.addr := readAddr
      mod.io.enq.bits.idx := readSourceAllocator.io.idx_out
      when(mod.io.enq.fire) {
        readAddr := readAddr + (512 / 8 * 64).U
        nTxsToLaunch := nTxsToLaunch - 1.U
      }
      M00.arvalid := mod.io.deq.valid
      mod.io.deq.ready := M00.arready
      M00.arlen := 63.U
      M00.arsize := 6.U // 2^6 bytes per beat = 64B = 512b = full line transfer
      M00.araddr := mod.io.deq.bits.addr
      M00.arid := mod.io.deq.bits.idx
    }
  }
  M00.rready := true.B
  when(M00.bvalid && M00.bready) {
    nTxsToRecieve := nTxsToRecieve - 1.U
    when (nTxsToRecieve === 1.U) {
      when (cycle_counter(49, 32) =/= 0.U) {
        cycle_count := 0xFFFFFFFFL.U
      }.otherwise {
        cycle_count := cycle_counter(31, 0)
      }
    }
  }
  (0 until n_slots).map { idx =>
    val mem = Reg(Vec(64, UInt(512.W)))
    val write_idx = Reg(UInt(6.W))
    val slot_idx = Reg(UInt(log2Up(n_parallel).W))
    val slot_state = RegInit(slot_idle)
    val c_addr = Reg(UInt(64.W))

    when (slot_state === slot_idle) {
      write_idx := 0.U
      val slot_alloc_fire = slotAllocator.io.alloc_in && slotAllocator.io.valid_out
      val read_alloc_fire = readSourceAllocator.io.alloc_in && readSourceAllocator.io.valid_out
      when ( slot_alloc_fire
        && read_alloc_fire
        && slotAllocator.io.idx_out === idx.U) {
        slot_state := slot_active
        slot_idx := readSourceAllocator.io.idx_out
        c_addr := readAddr | writeMask.U
      }
    }.elsewhen(slot_state === slot_active) {
      when (M00.rvalid && M00.rid === slot_idx) {
        mem(write_idx) := M00.rdata
        write_idx := write_idx + 1.U
        when (write_idx === 63.U || M00.rlast) {
          slot_state := slot_acquire
          write_idx := 0.U
        }
      }
    }.elsewhen(slot_state === slot_acquire) {
      writeBusLock.io.acquire_in(idx) := true.B
      when (writeBusLock.io.acquire_out(idx)) {
        slot_idx := writeSourceAllocator.io.idx_out
        slot_state := slot_transfer
        M00.awvalid := true.B
        M00.awaddr := c_addr
        M00.awid := writeSourceAllocator.io.idx_out
      }
    }.elsewhen(slot_state === slot_transfer) {
      M00.wvalid := true.B
      M00.wdata := mem(write_idx)
      M00.wlast := write_idx === 63.U
      M00.wstrb := BigInt("FFFFFFFFFFFFFFFF", 16).U
      when (M00.wready) {
        write_idx := write_idx + 1.U
        when (write_idx === 63.U) {
          writeBusLock.io.free := true.B
          slot_state := slot_wait
        }
      }
    }.elsewhen(slot_state === slot_wait) {
      when (M00.bvalid && M00.bid === slot_idx) {
        slot_state := slot_idle
        slotAllocator.io.free_in := true.B
        slotAllocator.io.free_idx := idx.U
      }
    }
  }
  cycle_counter := cycle_counter + 1.U
  S00.rdata := cycle_count

  S00.arready := stat_idle
  when (S00.arready && S00.arvalid) {
    stat_idle := false.B
  }

  S00.awready := machine_state === m_idle
  when (S00.awready && S00.awvalid) {
    machine_state := m_get_len
  }.elsewhen (machine_state === m_get_len) {
    S00.wready := true.B
    when (S00.wready && S00.wvalid) {
      Seq(nTxsToLaunch, nTxsToRecieve) foreach {
        _ := S00.wdata
      }
      cycle_counter := 0.U
      cycle_count := 0xDEADBEEFL.U
      readAddr := 0.U
      machine_state := m_resp
    }
  }.elsewhen(machine_state === m_resp) {
    S00.bvalid := true.B
    when (S00.bready && S00.bvalid) {
      machine_state := m_idle
    }
  }

  when (!stat_idle) {
    S00.rvalid := true.B
    when (S00.rvalid && S00.rready) {
      stat_idle := true.B
    }
  }
}

object Memcpy {
  def main(args: Array[String]): Unit = (new ChiselStage).emitVerilog(new Memcpy(16, 64),
    annotations = AnnotationSeq(Seq(
      CustomDefaultMemoryEmission(MemoryNoInit),
      CustomDefaultRegisterEmission(useInitAsPreset = false, disableRandomization = true)
    )))
}