package composer.MemoryStreams.RAM

import chisel3._
import chisel3.util._
import composer.MemoryStreams.CMemoryIOBundle
import composer.common.ShiftReg

class SyncReadMemMem(nPorts: Int, nRows: Int, dataWidth: Int, latency: Int) extends RawModule {
  val mio = IO(new CMemoryIOBundle(nPorts, log2Up(nRows), dataWidth))
  withClockAndReset(mio.clock.asClock, false.B.asAsyncReset) {
    val cmem = SyncReadMem(nRows, UInt(dataWidth.W))
    (0 until nPorts) foreach { port_idx =>
      val readDat = cmem.read(mio.addr(port_idx), mio.chip_select(port_idx) && mio.read_enable(port_idx))
      if (port_idx < 2) {
        when(mio.chip_select(port_idx)) {
          when(mio.write_enable(port_idx)) {
            cmem.write(mio.addr(port_idx), mio.data_in(port_idx))
          }
          assert(!(mio.read_enable(port_idx) && mio.write_enable(port_idx)))
        }
      } else {
        when (mio.chip_select(port_idx)) {
          when (mio.write_enable(port_idx)) {
            assert(false.B, "Only support 2 write ports per Xilinx spec. If you need this functionality, contact maintainer.")
          }
        }
      }
      if (latency == 1) {
        mio.data_out(port_idx) := readDat
      } else {
        mio.data_out(port_idx) := ShiftReg(readDat, latency - 1)
      }
    }
  }
}
