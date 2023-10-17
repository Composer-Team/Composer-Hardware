package composer.MemoryStreams.RAM

import chisel3._
import chisel3.util._
import composer.MemoryStreams.CMemoryIOBundle
import composer.common.ShiftReg

private class SRMMHelper(nReadPorts: Int,
                         nWritePorts: Int,
                         nReadWritePorts: Int,
                         nRows: Int, dataWidth: Int, latency: Int) extends Module {
  val mio = IO(new CMemoryIOBundle(nReadPorts, nWritePorts, nReadWritePorts, log2Up(nRows), dataWidth))
  val cmem = SyncReadMem(nRows, UInt(dataWidth.W))
  dontTouch(mio)
  (0 until nReadWritePorts) foreach { port_idx =>
    val pidx = mio.getReadWritePortIdx(port_idx)
    val readDat = cmem.read(mio.addr(pidx), mio.chip_select(pidx) && mio.read_enable(pidx))
    when(mio.chip_select(pidx)) {
      when(mio.write_enable(pidx)) {
        cmem.write(mio.addr(pidx), mio.data_in(pidx))
      }
      assert(!(mio.read_enable(pidx) && mio.write_enable(pidx)))
    }
    if (latency == 1) {
      mio.data_out(pidx) := readDat
    } else {
      mio.data_out(pidx) := ShiftReg(readDat, latency - 1)
    }
  }

  (0 until nWritePorts) foreach { port_idx =>
    val pidx = mio.getWritePortIdx(port_idx)
    when(mio.chip_select(pidx)) {
      cmem.write(mio.addr(pidx), mio.data_in(pidx))
    }
  }

  (0 until nReadPorts) foreach { port_idx =>
    val pidx = mio.getReadPortIdx(port_idx)
    val readDat = cmem.read(mio.addr(pidx), mio.chip_select(pidx) && mio.read_enable(pidx))
    if (latency == 1) {
      mio.data_out(pidx) := readDat
    } else {
      mio.data_out(pidx) := ShiftReg(readDat, latency - 1)
    }
  }
}


class SyncReadMemMem(nReadPorts: Int,
                     nWritePorts: Int,
                     nReadWritePorts: Int,
                     nRows: Int, dataWidth: Int, latency: Int) extends RawModule {
  val mio = IO(new CMemoryIOBundle(nReadPorts, nWritePorts, nReadWritePorts, log2Up(nRows), dataWidth))
  withClockAndReset(mio.clock.asClock, false.B.asAsyncReset) {
    val srmm = Module(new SRMMHelper(nReadPorts, nWritePorts, nReadWritePorts, nRows, dataWidth, latency))
    srmm.mio <> mio
  }
}
