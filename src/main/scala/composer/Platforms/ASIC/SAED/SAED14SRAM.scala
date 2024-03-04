package composer.Platforms.ASIC.SAED

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.MemoryStreams._
import composer.Platforms.ASIC.SAED.SAED14SRAM.mc
import composer.Platforms._

object SAED14SRAM {
  def mc(implicit p: Parameters): SAEDMemoryCompiler =
    p(PlatformKey).asInstanceOf[Platform with HasMemoryCompiler].memoryCompiler.asInstanceOf[SAEDMemoryCompiler]
}

class SAED_2RW_SRAM (rows: Int, dataBits: Int)(implicit p: Parameters) extends BlackBox with HasMemoryInterface {
  override val desiredName = mc.getMemoryName(2, rows, dataBits)
  val addrBits = log2Up(rows)
  val io = IO(new Bundle() {
    val A1 = Input(UInt(addrBits.W))
    val CE1 = Input(Bool())
    val WEB1 = Input(Bool())
    val OEB1 = Input(Bool())
    val CSB1 = Input(Bool())
    val I1 = Input(UInt(dataBits.W))
    val O1 = Output(UInt(dataBits.W))

    val A2 = Input(UInt(addrBits.W))
    val CE2 = Input(Bool())
    val WEB2 = Input(Bool())
    val OEB2 = Input(Bool())
    val CSB2 = Input(Bool())
    val I2 = Input(UInt(dataBits.W))
    val O2 = Output(UInt(dataBits.W))
  })

  override def addr: Seq[UInt] = Seq(io.A1, io.A2)

  override def data_out: Seq[UInt] = Seq(io.O1, io.O2)

  override def data_in: Seq[UInt] = Seq(io.I1, io.I2)

  override def chip_select: Seq[Bool] = Seq(io.CSB1, io.CSB2)

  override def read_enable: Seq[Bool] = Seq(io.OEB1, io.OEB2)

  override def write_enable: Seq[Bool] = Seq(io.WEB1, io.WEB2)

  override def clocks: Seq[Bool] = Seq(io.CE1, io.CE2)
}

class SAED_1RW_SRAM(rows: Int, dataBits: Int)(implicit p: Parameters) extends BlackBox with HasMemoryInterface {
  override val desiredName = mc.getMemoryName(1, rows, dataBits)
  val addrBits = log2Up(rows)
  val io = IO(new Bundle() {
    val A = Input(UInt(addrBits.W))
    val CE = Input(Bool())
    val WEB = Input(Bool())
    val OEB = Input(Bool())
    val CSB = Input(Bool())
    val I = Input(UInt(dataBits.W))
    val O = Output(UInt(dataBits.W))
  })

  override def addr: Seq[UInt] = Seq(io.A)

  override def data_out: Seq[UInt] = Seq(io.O)

  override def data_in: Seq[UInt] = Seq(io.I)

  override def chip_select: Seq[Bool] = Seq(io.CSB)

  override def read_enable: Seq[Bool] = Seq(io.OEB)

  override def write_enable: Seq[Bool] = Seq(io.WEB)

  override def clocks: Seq[Bool] = Seq(io.CE)
}
