package composer.Platforms.ASIC.ASAP7

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._
import composer.MemoryStreams.HasMemoryInterface
import composer.Platforms.ASICMemoryCompilerKey

class ASAP7_SP_SRAM (rows: Int, dataBits: Int)(implicit p: Parameters) extends BlackBox with HasMemoryInterface {
  override val desiredName = p(ASICMemoryCompilerKey).getMemoryName(nPorts = 1, rows, dataBits)
  val addrBits = log2Up(rows)
  val io = IO(new Bundle() {
    val clk = Input(Bool())
    val ADDRESS = Input(UInt(log2Up(rows).W))
    val wd = Input(UInt(dataBits.W))
    val banksel = Input(Bool())
    val read = Input(Bool())
    val write = Input(Bool())
    val dataout = Output(UInt(dataBits.W))

    val sdel = Input(UInt(5.W))
  })

  override def addr: Seq[UInt] = Seq(io.ADDRESS)

  override def data_out: Seq[UInt] = Seq(io.dataout)

  override def data_in: Seq[UInt] = Seq(io.wd)

  override def chip_select: Seq[Bool] = Seq(io.banksel)

  override def read_enable: Seq[Bool] = Seq(io.read)

  override def write_enable: Seq[Bool] = Seq(io.write)

  override def clocks: Seq[Bool] = Seq(io.clk)
}
