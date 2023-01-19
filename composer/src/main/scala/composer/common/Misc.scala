package composer.common

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.{CoreIDLengthKey, SystemIDLengthKey}
import freechips.rocketchip.tile.{RoCCResponse, XLen}

/**
  * A small register-based memory providing an easy interface for setting
  * control information.
  *
  * @param n The number of registers
  * @param w The number of bits for each register
  */
class SettingsFile(n: Int, w: Int) extends Module {
  val lenLen = math.min(32, w)
  val io = IO(new Bundle {
    // Interface to write into the file
    val write = new Bundle {
      val en = Input(Bool())
      val addr = Input(UInt(log2Up(n).W))
      val len = Input(UInt(w.W))
      val data = Input(UInt(w.W))
    }
    // The values contained in the register file
    val addrs_out = Output(Vec(n, UInt(w.W)))
    val lens_out = Output(Vec(n, UInt(w.W)))
  })
  val addrs = Reg(Vec(n, UInt(w.W)))
  val lens = Reg(Vec(n, UInt(lenLen.W)))
  when(io.write.en) {
    addrs(io.write.addr) := io.write.data
    lens(io.write.addr) := io.write.len
  }

  io.addrs_out := addrs
  io.lens_out := lens
}

/**
  * Routes data from the input to one or more outputs based on an ID
  *
  * @param typ   The Chisel Data type to route
  * @param n     Number of output interfaces
  * @param getId Function that takes the input data and produces the id
  */
class RequestRouter[T <: Data](typ: T, n: Int, getId: T => UInt) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(typ))
    val out = Vec(n, Decoupled(typ))
  })

  val id = getId(io.in.bits)
  io.in.ready := io.out(id).ready

  io.out.zipWithIndex.foreach { case (out, i) =>
    out.valid := id === i.U && io.in.valid
    out.bits := io.in.bits
  }
}

//noinspection ScalaUnusedSymbol
class ComposerRoccCommand()(implicit p: Parameters) extends Bundle {
  val inst = new Bundle {
    val rd = UInt(5.W)
    val rs1 = UInt(5.W)
    val rs2 = UInt(5.W)
    val xd = Bool()

    val xs1 = Bool()
    val xs2 = Bool()
    val opcode = UInt(7.W)

    val system_id = UInt(p(SystemIDLengthKey).W)
    val funct = UInt((7 - p(SystemIDLengthKey)).W)
  }
  val core_id = UInt(p(CoreIDLengthKey).W)
  val payload1 = UInt((64 - p(CoreIDLengthKey)).W)
  val payload2 = UInt(64.W)

  def pack(bufferToPow2: Boolean = true, withRoutingPayload: Option[UInt] = None): UInt = {
    val s = Cat(inst.rd, inst.rs1, inst.rs2, inst.xd, inst.xs1, inst.xs2, inst.opcode, inst.system_id, inst.funct,
      core_id, payload1, payload2)
    if (bufferToPow2) {
      val l = 1 << log2Up(s.getWidth)
      withRoutingPayload match {
        case None => Cat(0.U((l - s.getWidth).W), s)
        case Some(pay) =>
          require(pay.getWidth <= l - s.getWidth)
          Cat(0.U((l - s.getWidth - pay.getWidth).W), pay, s)
      }
    } else s
  }
}

object ComposerRoccCommand {
  /** unpack from ComposerRoccCommand.pack() */
  def apply(a: UInt)(implicit p: Parameters): ComposerRoccCommand = {
    val b = Wire(new ComposerRoccCommand())
    b.payload2 := a(63, 0)
    val coreId_len = p(CoreIDLengthKey)
    val sysId_len = p(SystemIDLengthKey)
    b.payload1 := a(127 - coreId_len, 64)
    b.core_id := a(127, 128 - coreId_len)
    b.inst.funct := a(134 - sysId_len, 128)
    b.inst.system_id := a(134, 135- sysId_len)
    b.inst.opcode := a (141, 135)
    b.inst.xs2 := a(142)
    b.inst.xs1 := a(143)
    b.inst.xd := a(144)
    b.inst.rs2 := a(149, 145)
    b.inst.rs1 := a(154, 150)
    b.inst.rd := a(159, 155)
    b
  }

  val packLengthBytes = 160 / 8
}

class ComposerRoccUserResponse()(implicit p: Parameters) extends Bundle {
  val data = UInt((64 - p(SystemIDLengthKey)).W)
  val rd = UInt(5.W)
}

class ComposerRoccResponse()(implicit p: Parameters) extends Bundle {
  val rd = UInt(5.W)
  val system_id = UInt(p(SystemIDLengthKey).W)
  val core_id = UInt(p(CoreIDLengthKey).W)
  val data = UInt((64 - p(SystemIDLengthKey) - p(CoreIDLengthKey)).W)

  def pack: UInt = {
    val q = Wire(UInt(64.W))
    q := Cat(system_id, core_id, data)
    q
  }
}

object ComposerRoccResponse {
  def apply(a: UInt)(implicit p: Parameters): ComposerRoccResponse = {
    // unpack from @pack
    val wire = Wire(new ComposerRoccResponse())
    val syswid = p(SystemIDLengthKey)
    val corewid = p(CoreIDLengthKey)
    wire.data := a(63-syswid - corewid, 0)
    wire.core_id := a(63-syswid, 64-syswid - corewid)
    wire.system_id := a(63, 64-syswid)
    wire
  }

  def getDataWidth(implicit p: Parameters): Int = 64 - p(SystemIDLengthKey) - p(CoreIDLengthKey)
  val getWidthBits: Int = 64
  val getWidthBytes: Int = getWidthBits / 8
}
