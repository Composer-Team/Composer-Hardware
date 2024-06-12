package beethoven.Protocol

import chisel3._
import chisel3.util._
import beethoven.Generation.CppGeneration
import beethoven.MemoryStreams.MemoryIOBundle
import beethoven.Protocol.ACEZynqRegionManager.ace_cmd_clean_invalidate
import beethoven.common.CLog2Up
import freechips.rocketchip.subsystem.MasterPortParams

/**
 * This code is now in a state of deprecation.
 * When considering coherence, there are some tradeoffs that are hard to make on the basis of performance, but trivial
 * to make on the basis of ease-of-implementation.
 *
 * From the basis of performance, we have two options when considering accelerators of the style that Beethoven intends
 * to accelerate (namely VLIW accelerators with multiple-ms latency). We can either:
 * 1. Invalidate the cache line in CPU caches  before the accelerator starts using it and then issue non-shared DRAM
 * transactions from the accelerator. This incurs the cost of an initial setup for all of the segments that the kernel
 * uses, but then future memory accesses from the accelerator incur no additional latency and bypass any sort of
 * coherence logic/snooping that a shared segment may incur.
 * 2. Issue all transactions from the accelerator as shared, write-invalidate transactions. This likely incurs a small
 * overhead on every transaction, but it's not clear how big of a problem this is. From the point-of-view of our
 * accelerator implementation, it's just a matter of hard-coding some AXI bits. From the performance perspective,
 * there's really nothing for us to do; if the interconnect uses some sort of directory, this may incur a very small
 * latency, and if the interconnect uses snooping, this may incur a slightly larger latency. It's _extremely_
 * difficult to figure of the costs here without actually implementing the system and testing it on every system,
 * which I don't intend to do at the current moment... So we're just going with what's easy and gets the job done.
 */

/**
 * The Region manager is responsible for a couple of things.
 * 1. Accept commands from the host which contain an address range (start addr, offset)
 * and enqueue them into a list for managed segments
 * 2. Accept commands from the host to remove an address from the address manager
 * 3. Accept commands from host to perform a cache flush on all of the relevant regions
 * 4. Manage snoop transactions from external sources.
 *
 *
 * 1, 2 - Since the front bus is only 32b at this time, we have to take the command in
 * multiple beats. This is handled by the front bus enqueueing 32b segments into
 * a queue
 *
 * 3 - Use AXI4 ACE protocol MakeInvalid transaction (D4-221 in protocl spec) to ensure
 * all cached copies of a cache line are marked invalid.
 *
 * 4 - This isn't a cache, so snoops will never succeed. Even for addresses that we
 * manage, data is only stored here intermittently and it is the responsibility of
 * the developer to not access data that is borrowed by the FPGA.
 *
 * See Zynq TRM pg. 1085 for more info on Zynq usage of ACE.
 *
 * The Zynq system has a physically tagged, virtually indexed cache, although there
 * appears to be some sort of physical->virtual mapping because the TRM specifies
 * physical addresses to be used on the address bus. This is fine by me :)
 *
 * Regions should be able to be held in a couple of different states.
 * - Read mode - No need for anything but I/O coherency. No special concessions need
 * to be made here because the HPC and HP buses are I/O coherent. More
 * explicitly, this means that reads on these buses are coherent with
 * system caches.
 * - Write / ReadWrite mode - I/O coherence is fine for ReadWrite mode, obviously not
 * particularly useful for write mode. At any rate though, these regions
 * need to be _invalidated_ from system caches before returning a
 * response. This can be done before or after. Some considerations:
 * Flushing before is more likely to be harmful for a misbehaving user.
 * If the user uses in-flight regions in the host system and un-does
 * the invalidation, then the cache will have to be reinvalidated again
 * afterwards. On the other hand, data that's flushed from CPU caches
 * during execution due to cache line eviction will overwrite data.
 */


object ACEZynqRegionManager {
  val ace_cmd_add = 0
  val ace_cmd_barrier_release = 1
  val ace_cmd_invalidate = 2
  val ace_cmd_clean_invalidate = 3
}

class ACERegionManagementCommand(params: MasterPortParams, nIdxBits: Int) extends Bundle {
  val addr: UInt = Output(UInt(log2Up(params.size).W))
  // this needs to be longer than lenBits because segment length may be longer than AXI burst length
  val len: UInt = Output(UInt(log2Up(params.size).W))
  /**
   * Operations
   * ADD
   * RELEASE_BARRIER - commands cannot be delivered until coherence engine is idle
   * INVALIDATE - activate command barrier and flush
   * CLEAN_INVALIDATE
   */
  val operation: UInt = Output(UInt(2.W))

  val idx: UInt = Output(UInt(nIdxBits.W))
}

class RegionCommandAssembler(params: MasterPortParams, nIdxBits: Int) extends Module {
  val in: DecoupledIO[UInt] = IO(Flipped(Decoupled(UInt(32.W))))
  val out: DecoupledIO[ACERegionManagementCommand] = IO(Decoupled(new ACERegionManagementCommand(params, nIdxBits)))
  require(log2Up(params.size) <= 64)

  // private scope
  {
    val beatCount = RegInit(0.U(3.W))
    val addrHold, lenHold = Reg(Vec(2, UInt(32.W)))
    val op = Reg(UInt(2.W))
    val idx = Reg(UInt(nIdxBits.W))
    val valid_out = RegInit(false.B)

    out.bits.len := Cat(lenHold)
    out.bits.addr := Cat(addrHold)
    out.bits.operation := op
    out.bits.idx := idx
    out.valid := valid_out

    in.ready := !valid_out
    when(in.fire) {
      beatCount := beatCount + 1.U
      when(beatCount === 0.U) {
        addrHold(1) := in.bits
      }.elsewhen(beatCount === 1.U) {
        addrHold(0) := in.bits
      }.elsewhen(beatCount === 2.U) {
        lenHold(1) := in.bits
      }.elsewhen(beatCount === 3.U) {
        lenHold(0) := in.bits
      }.elsewhen(beatCount === 4.U) {
        op := in.bits(1, 0)
        idx := in.bits(31, 2)
        valid_out := true.B
      }
    }.elsewhen(out.fire) {
      valid_out := false.B
      beatCount := 0.U
    }
  }
}

class ACEZynqRegionManager(params: MasterPortParams,
                           maxSegments: Int,
                          // latency, rows, cols
                           SPMemoryGenerator: (Int, Int, Int) => MemoryIOBundle
                          ) extends Module {
  CppGeneration.addPreprocessorDefinition("HAS_COHERENCE", maxSegments.toString)
  CppGeneration.addPreprocessorDefinition(Seq(
    ("COHERENCE_OP_ADD", ACEZynqRegionManager.ace_cmd_add.toString),
    ("COHERENCE_OP_INVALIDATE", ACEZynqRegionManager.ace_cmd_invalidate.toString),
    ("COHERENCE_OP_BARRIER_RELEASE", ACEZynqRegionManager.ace_cmd_barrier_release.toString),
    ("COHERENCE_OP_CLEAN_INVALIDATE", ACEZynqRegionManager.ace_cmd_clean_invalidate.toString)
  ))
  val out: ACE = IO(new ACE(params))
  val barrier: Bool = IO(Output(Bool()))
  val in_cmd: DecoupledIO[UInt]= IO(Flipped(Decoupled(UInt(32.W))))

  {
    val barrier_reg = RegInit(false.B)
    barrier := barrier_reg

    val maxBurst = 1

    val lbb = CLog2Up(params.beatBytes)
    val addrChopBits = out.addrBits - lbb
    val addrBits = out.addrBits
    val memLatency = 3
    val memValid = RegInit(0.U(memLatency.W))
    memValid := Cat(memValid.tail(1), false.B)
      val mapping_memory = SPMemoryGenerator(memLatency, maxSegments, addrChopBits * 2)
    dontTouch(mapping_memory)
    mapping_memory.initLow(clock)
    val indexHold = Reg(UInt(log2Up(maxSegments).W))
    mapping_memory.addr(0) := indexHold
    out.initFromMasterLow()
    val nIdxBits = log2Up(maxSegments)

    // all snoops are answered in one cycle and the responses are always the same
    out.acready := true.B
    out.crvalid := true.B
    out.crresp := 0.U
    out.cdvalid := false.B

    val cmd_translator = Module(new RegionCommandAssembler(params, nIdxBits))
    cmd_translator.in <> in_cmd
    cmd_translator.out.ready := false.B

    val s_idle :: s_add_segment :: s_read_flush :: s_read_wait :: s_emit :: s_emit_wait :: s_emit_ack :: Nil = Enum(7)
    val state = RegInit(s_idle)

    val addrHold = Reg(UInt(addrBits.W))
    val lenHold = Reg(UInt(addrBits.W))
    val opHold = Reg(UInt(2.W))
    val txSmall = lenHold < maxBurst.U

    when(state === s_idle) {
      // always accept incoming snoop requests first
      cmd_translator.out.ready := true.B
      when(cmd_translator.out.fire) {
        when(cmd_translator.out.bits.operation === ACEZynqRegionManager.ace_cmd_add.U) {
          state := s_add_segment
          addrHold := cmd_translator.out.bits.addr
          lenHold := cmd_translator.out.bits.len
          indexHold := cmd_translator.out.bits.idx
        }.elsewhen(cmd_translator.out.bits.operation === ACEZynqRegionManager.ace_cmd_barrier_release.U) {
          barrier_reg := false.B
        }.otherwise {
          state := s_read_flush
          barrier_reg := true.B
          indexHold := cmd_translator.out.bits.idx
          opHold := cmd_translator.out.bits.operation
        }
      }
    }.elsewhen(state === s_add_segment) {
      mapping_memory.write_enable(0) := true.B
      mapping_memory.read_enable(0) := false.B
      mapping_memory.chip_select(0) := true.B
      mapping_memory.data_in(0) := Cat(addrHold >> lbb, lenHold >> lbb)
      state := s_idle
    }.elsewhen(state === s_read_flush) {
      mapping_memory.write_enable(0) := false.B
      mapping_memory.read_enable(0) := true.B
      mapping_memory.chip_select(0) := true.B
      memValid := 1.U
      state := s_read_wait
    }.elsewhen(state === s_read_wait) {
      when(memValid.head(1).asBool) {
        addrHold := mapping_memory.data_out(0).head(addrChopBits) << lbb
        lenHold := mapping_memory.data_out(0).tail(addrChopBits)
        state := s_emit
      }
    }.elsewhen(state === s_emit) {
      out.arbar := 0.U
      out.arvalid := true.B
      out.ardomain := 2.U // 10
      when(opHold === ace_cmd_clean_invalidate.U) {
        // CleanInvalid
        out.arsnoop := 9.U
      }.otherwise {
        // MakeInvalid transaction: D3-179 in AXI-ACE Spec
        out.arsnoop := 13.U
      }
      out.arsnoop := 13.U // 1101
      out.arid := 0.U
      out.araddr := addrHold
      out.arburst := 1.U
      out.aruser := Cat(0xF.U(4.W), 0.U(6.W))

      when(txSmall) {
        out.arlen := 0.U
      }.otherwise {
        // Supports up to 16 bursts
        out.arlen := (maxBurst - 1).U
      }
      out.arsize := 4.U // 16B
      out.arprot := 0.U // ignored in AXI4 anyways
      out.arqos := 0.U // ignored in AXI4 anyways
      // 0b1000 indicates write-allocate (kinda just irrelevant for our purposes
      // 0b0010 indicates modifiable/cacheable - I think this means that the interconnect can split apart the
      //   transaction as it pleases
      // 0b0001 indicates bufferable - that the interconnect can choose to reorder transactions as it pleases and
      //   issue this transaction when convenient
      out.arcache := 0xB.U
      when(out.arready) {
        state := s_emit_wait
        when(txSmall) {
          addrHold := addrHold + 16.U
          lenHold := lenHold - 1.U
        }.otherwise {
          lenHold := lenHold - maxBurst.U
          addrHold := addrHold + (16 * maxBurst).U
        }
      }
    }.elsewhen(state === s_emit_wait) {
      out.rready := true.B
      when(out.rvalid) {
        state := s_emit_ack
      }
    }.elsewhen(state === s_emit_ack) {
      // no handshake here
      out.rack := true.B
      when(lenHold === 0.U) {
        state := s_idle
      }.otherwise {
        state := s_emit
      }
    }
  }
}
