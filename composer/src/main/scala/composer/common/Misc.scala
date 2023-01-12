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
    val funct = UInt(3.W)
  }
  val core_id = UInt(p(CoreIDLengthKey).W)
  val payload1 = UInt((64-p(CoreIDLengthKey)-p(SystemIDLengthKey)).W)
  val payload2 = UInt(64.W)
}

class ComposerRoccResponse()(implicit p: Parameters) extends Bundle {
  val rd = UInt(5.W)
  val system_id = UInt(p(SystemIDLengthKey).W)
  val core_id = UInt(p(CoreIDLengthKey).W)
  val data = UInt((64-system_id.getWidth-core_id.getWidth).W)
  def packData()(implicit p: Parameters): UInt = {
    val q = Wire(UInt(64.W))
    q := Cat(system_id, core_id, data)
    q
  }
  def getDataWidth: Int = data.getWidth
}
