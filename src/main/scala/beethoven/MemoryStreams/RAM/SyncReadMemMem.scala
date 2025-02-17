package beethoven.MemoryStreams.RAM

import chisel3._
import chisel3.util._
import beethoven.MemoryStreams.{HasMemoryInterface, MemoryIOBundle}
import beethoven.common.ShiftReg
import chipsalliance.rocketchip.config.Parameters

private class SRMMHelper(nReadPorts: Int,
                         nWritePorts: Int,
                         nReadWritePorts: Int,
                         nRows: Int,
                         dataWidth: Int,
                         latency: Int)(implicit p: Parameters) extends Module {
  val mio = IO(new MemoryIOBundle(nReadPorts, nWritePorts, nReadWritePorts, log2Up(nRows), dataWidth, false))
  val cmem = SyncReadMem(nRows, UInt(dataWidth.W))
  dontTouch(mio)
  (0 until nReadWritePorts) foreach { port_idx =>
    val pidx = mio.getReadWritePortIdx(port_idx)
    val readDat = cmem.read(mio.addr(pidx), mio.chip_select(pidx) && mio.read_enable(pidx))
    when(mio.chip_select(pidx)) {
      when(!mio.read_enable(pidx)) {
        cmem.write(mio.addr(pidx), mio.data_in(pidx))
      }
      assert(!(mio.read_enable(pidx) && mio.write_enable(pidx)(0)))
    }
    if (latency == 1) {
      mio.data_out(pidx) := readDat
    } else {
      mio.data_out(pidx) := ShiftReg(readDat, latency - 1, clock)
    }
  }

  (0 until nWritePorts) foreach { port_idx =>
    val pidx = mio.getWritePortIdx(port_idx)
    when(mio.chip_select(pidx)) {
      cmem.write(mio.addr(pidx), mio.data_in(pidx))
    }
    mio.data_out(pidx) := DontCare
  }

  (0 until nReadPorts) foreach { port_idx =>
    val pidx = mio.getReadPortIdx(port_idx)
    val readDat = cmem.read(mio.addr(pidx), mio.chip_select(pidx) && mio.read_enable(pidx))
    if (latency == 1) {
      mio.data_out(pidx) := readDat
    } else {
      mio.data_out(pidx) := ShiftReg(readDat, latency - 1, clock)
    }
  }
}

class SyncReadMemMem(nReadPorts: Int,
                     nWritePorts: Int,
                     nReadWritePorts: Int,
                     nRows: Int, dataWidth: Int, latency: Int)(implicit p: Parameters) extends RawModule with HasMemoryInterface {

  val mio = IO(new MemoryIOBundle(nReadPorts, nWritePorts, nReadWritePorts, log2Up(nRows), dataWidth, false))
  withClockAndReset(mio.clock.asClock, false.B.asAsyncReset) {
    val srmm = Module(new SRMMHelper(nReadPorts, nWritePorts, nReadWritePorts, nRows, dataWidth, latency))
    srmm.mio <> mio
  }

  override def data_in: Seq[UInt] = mio.data_in

  override def data_out: Seq[UInt] = mio.data_out

  override def addr: Seq[UInt] = mio.addr

  override def chip_select: Seq[Bool] = mio.chip_select

  override def read_enable: Seq[Bool] = mio.read_enable

  override def write_enable: Vec[UInt] = mio.write_enable

  override def clocks: Seq[Bool] = Seq(mio.clock)
}
