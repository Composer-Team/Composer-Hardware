package composer.common

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.{RoCCResponse, XLen}

/**
 * A small register-based memory providing an easy interface for setting
 * control information.
 *
 * @param n The number of registers
 * @param w The number of bits for each register
 */
class SettingsFile(n: Int, w: Int) extends Module {
  val io = IO(new Bundle {
    // Interface to write into the file
    val write = new Bundle {
      val en = Input(Bool())
      val addr = Input(UInt(log2Up(n).W))
      val data = Input(UInt(w.W))
    }
    // The values contained in the register file
    val values = Output(Vec(n, UInt(w.W)))
  })

  val values = Reg(Vec(n, UInt(w.W)))

  when(io.write.en) {
    values(io.write.addr) := io.write.data
  }

  io.values := values
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
class ComposerRoccCommand() extends Bundle {
  // TODO UG: fix rs1(outer field), core_id, and system_id according to params (also add implicit parameters)
  val inst = new Bundle {
    val rd = UInt(5.W)
    val rs1 = UInt(5.W)
    val rs2 = UInt(5.W)
    val xd = Bool()

    val xs1 = Bool()
    val xs2 = Bool()
    val opcode = UInt(7.W)

    val system_id = UInt(4.W)
    val funct = UInt(3.W)
  }
  val core_id = UInt(8.W)
  // TODO UG: This should change dynamically according to width of sys/core id width
  val rs1 = UInt(56.W)
  val rs2 = UInt(64.W)
}

class ComposerRoccResponse()(implicit p: Parameters) extends Bundle {
  val rd = UInt(5.W)
  // TODO UG: these should reflect parameters SystemIDBitsKey, etc...
  val system_id = UInt(4.W)
  val core_id = UInt(8.W)
  val data = UInt((p(XLen)-system_id.getWidth-core_id.getWidth).W)
  def packData()(implicit p: Parameters): UInt = {
    val q = Wire(UInt(64.W))
    q := Cat(system_id, core_id, data)
    q
  }
}


object Util {
  def scaledLength(length: UInt, align: Int): UInt = {
    val l2 = log2Ceil(align)
    val needtoaddone = length(l2 - 1, 0).orR
    val lengthdiv = Mux(needtoaddone, 1.U + (length >> l2).asTypeOf(UInt()), length >> l2)
    //val ret = Cat(lengthdiv, 0.U(l2.W))
    Cat(lengthdiv)
  }

  /**
   * Enum for the local things with the Modular System (doesn't start at zero)
   */
  def FunctEnum(n: Int): Seq[UInt] = {
    (2 until ((n - 2) + 2)).map(_.U((1 max log2Ceil(n)).W)).toList
  }

  implicit class BoolSeqHelper(s: Seq[Bool]) {
    def any: Bool = s.reduce(_ || _)

    def none: Bool = !s.any


    def all: Bool = s.reduce(_ && _)

    def notAll: Bool = !s.all
  }

}

