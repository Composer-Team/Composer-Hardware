package composer.MemoryStreams.RAM

import chisel3._
import chisel3.util._
import composer.MemoryStreams._
import composer.common.ShiftReg

class RegMem(nRows: Int, nColumns: Int, nPorts: Int, latency: Int) extends RawModule with HasMemoryInterface{
  val io = IO(new CMemoryIOBundle(0, 0, nPorts, log2Up(nRows), nColumns))

  override def data_in: Seq[UInt] = io.data_in

  override def data_out: Seq[UInt] = io.data_out

  override def addr: Seq[UInt] = io.addr

  override def chip_select: Seq[Bool] = io.chip_select

  override def read_enable: Seq[Bool] = io.read_enable

  override def write_enable: Seq[Bool] = io.write_enable

  override def clocks: Seq[Bool] = Seq(io.clock)

  val clock = io.clock.asClock

  withClock(clock) {
    val mem = Reg(Vec(nRows, UInt(nColumns.W)))
    val out_regs = Reg(Vec(nPorts, UInt(nColumns.W)))
    val out_regs_delayed = ShiftReg(out_regs, latency - 1)
    io.data_out zip out_regs_delayed foreach { case (i, r) => i := r }
    data_out zip out_regs foreach { case (o, r) => o := r }
    (0 until nPorts) foreach { port_idx =>
      when(io.chip_select(port_idx)) {
        when(io.read_enable(port_idx)) {
          out_regs(port_idx) := mem(io.addr(port_idx))
        }.elsewhen(io.write_enable(port_idx)) {
          mem(io.addr(port_idx)) := io.data_in(port_idx)
        }
      }
    }
  }
}
