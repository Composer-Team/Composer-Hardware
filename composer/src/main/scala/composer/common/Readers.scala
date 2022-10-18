package composer.common

import composer._

import chisel3._
import chisel3.util._

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.config.Parameters

/**
 * A request to the ColumnReadChannel
 *
 * @param maxBytes the largest number of bytes that can be in a single element
 */
class ColumnReadRequest(addressBits: Int, maxBytes: Int)
  extends Bundle {
  //private val sizeBits = log2Ceil(maxBytes) + 1
  /** The starting address of the read */
  val addr = UInt(addressBits.W)
  /** The number of elements to read */
  val len = UInt(addressBits.W)
}

class ReadChannelIO(addressBits: Int, maxBytes: Int) extends Bundle {
  val req = Flipped(Decoupled(new ColumnReadRequest(addressBits, maxBytes)))
  val channel = new DataChannelIO(maxBytes)
  val busy = Output(Bool())
}

/**
 * Reads data sequentially from memory and sends it out piece by piece
 *
 * @param maxBytes the largest number of bytes per element the reader will produce
 */
class ColumnReadChannel(maxBytes: Int)(implicit p: Parameters) extends LazyModule()(p) {

  // to-do: blockbytes for buffer readers should be 256?
  val blockBytes = p(CacheBlockBytes)
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      name = "ColumnReadCHanngel",
      sourceId = IdRange(0, 1),
      supportsProbe = TransferSizes(1, blockBytes),
      supportsGet = TransferSizes(1, blockBytes)
    ))
  )))

  lazy val module = new ColumnReadChannelModule(maxBytes = maxBytes, outer = this)

}

class ColumnReadChannelModule(maxBytes: Int, outer: ColumnReadChannel)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val blockBytes = p(CacheBlockBytes)
  val (tl, edge) = outer.node.out(0)
  //private val sizeBits = log2Ceil(maxBytes) + 1
  val addressBits = log2Up(edge.manager.maxAddress)

  require(isPow2(maxBytes))
  val io = IO(new ReadChannelIO(addressBits, maxBytes))

  val beatBytes = edge.manager.beatBytes
  val logBeatBytes = log2Up(beatBytes)
  val logChannelSize = log2Up(io.channel.data.bits.getWidth) - 3
  val beatsPerBlock = blockBytes / beatBytes
  println(s"lbb: $logBeatBytes, bpb: $beatsPerBlock, lcs: $logChannelSize")

  val addr = Reg(UInt(addressBits.W))
  val blockAddr = Wire(UInt(addressBits.W))
  val len = Reg(UInt((addressBits-logBeatBytes).W))

  val buffer = Reg(Vec(beatsPerBlock, UInt((beatBytes * 8).W)))
  val buffer_pending = RegInit(0.U(beatsPerBlock.W))
  val gnt_idx = RegInit(0.U((log2Ceil(beatsPerBlock) + 1).W))

  val byte_offset = Reg(UInt((log2Ceil(blockBytes) + 1).W))

  val s_idle :: s_acquire :: s_send :: s_finishing :: s_stopping :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val shouldStop = Reg(Bool())

  when(io.req.fire) {
    addr := io.req.bits.addr
    byte_offset := io.req.bits.addr(log2Ceil(blockBytes) - 1, 0)
    //size := io.req.bits.size
    len := io.req.bits.len >> logChannelSize
    //stride := io.req.bits.stride.getOrElse(0.U)
    state := s_acquire
    shouldStop := false.B

    val mask = (1.U << log2Ceil(maxBytes).U).asUInt - 1.U
    //val mask = (1.U << io.req.bits.size) - 1.U
    assert((io.req.bits.addr & mask) === 0.U,
      "ColumnReadChannel: unaligned address")
    /*if (strided) {
      assert((io.req.bits.stride.get & mask) === 0.U,
        "ColumnReadChannel: unaligned stride")
     }*/

    when(io.req.bits.len === 0.U) {
      state := s_finishing
    }
  }

  when(tl.a.fire) {
    gnt_idx := 0.U
    addr := addr + blockBytes.U
    state := s_send
  }

  //val gnt_idx = tl.d.bits.source
  val gnt_data = tl.d.bits.data

  when(tl.d.fire) {
    buffer(gnt_idx) := gnt_data
    gnt_idx := gnt_idx + 1.U
  }

  buffer_pending := (buffer_pending &
    (~Mux(tl.d.fire, UIntToOH(gnt_idx).asUInt, 0.U)).asUInt) |
    Mux(tl.a.fire, 15.U, 0.U) // 15 = 2^beatsPerBlock - 1

  when(io.channel.data.fire) {
    //val new_byte_offset = byte_offset + stride + (1.U << size)
    //val new_byte_offset = byte_offset + (1.U << size)
    val new_byte_offset = byte_offset + (1.U << log2Ceil(maxBytes).U).asUInt
    when(len === 1.U) {
      state := s_finishing
    }.elsewhen(new_byte_offset >= blockBytes.U) {
      state := s_acquire
    }
    len := len - 1.U
    byte_offset := new_byte_offset % blockBytes.U
  }
  when(io.channel.stop && state =/= s_idle) {
    state := s_stopping
    //shouldStop := true.B
  }

  io.channel.finished := false.B
  when(state === s_finishing && !buffer_pending.orR) {
    io.channel.finished := true.B
    state := s_idle
  }

  when(state === s_stopping && !buffer_pending.orR) {
    state := s_idle
  }

  val byte_pending = FillInterleaved(beatBytes, buffer_pending)
  val byte_mask = ((1.U << (1.U << log2Ceil(maxBytes).U).asUInt).asUInt - 1.U) (maxBytes - 1, 0)
  val data_valid = !((byte_pending >> byte_offset).asUInt & byte_mask).orR
  val data_data = buffer.asUInt >> Cat(byte_offset, 0.U(3.W))
  val masked_data = data_data.asUInt & FillInterleaved(8, byte_mask)

  blockAddr := Cat(addr(addressBits - 1, log2Ceil(blockBytes)),
    0.U(log2Ceil(blockBytes).W))


  io.req.ready := state === s_idle
  tl.a.valid := state === s_acquire && !shouldStop
  tl.a.bits := edge.Get(fromSource = 0.U,
    toAddress = blockAddr,
    lgSize = log2Ceil(blockBytes).U)._2
  println("logblock bytes is " + log2Ceil(blockBytes) + " block bytes is " + blockBytes)

  tl.d.ready := buffer_pending.orR
  io.channel.data.valid := (state === s_send) && data_valid
  io.channel.data.bits := masked_data
  io.busy := state =/= s_idle
}