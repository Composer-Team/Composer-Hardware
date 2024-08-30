package beethoven.Platforms.ASIC

import beethoven.Generation.{BeethovenBuild, BuildMode}
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.BaseModule
import chisel3.util._
import beethoven.MemoryStreams.RAM.SyncReadMemMem
import beethoven.MemoryStreams._
import beethoven.Platforms.ASIC.ProcessCorner.ProcessCorner
import beethoven.Platforms.ASIC.ProcessTemp.ProcessTemp
import beethoven.Platforms.ASIC.memoryCompiler.CanCompileMemories
import beethoven.Platforms._
import beethoven.common.{CLog2Up, ShiftReg}
import os.Path

import scala.annotation.tailrec

/**
 * The linkage of this memory compiler goes to an actual program that can compile memories, their datasheets, etc...
 * This in an important distinction because it makes it much harder to enumerate all of the memories in a search for
 * a memory cascade. To deal with this, such a compiler must provide its own search.
 */

trait SupportsWriteEnable {
  def getWESRAMArray(suggestedRows: Int, suggestedColumns: Int, nPort: Int, latency: Int)(implicit p: Parameters): Option[SRAMArray]

  def generateMemoryFactory(nPorts: Int, nRows: Int, nColumns: Int, withWE: Boolean)(implicit p: config.Parameters): () => BaseModule with HasMemoryInterface

  def generateMemoryFactory(nPorts: Int, nRows: Int, nColumns: Int)(implicit p: config.Parameters): () => BaseModule with HasMemoryInterface =
    generateMemoryFactory(nPorts, nRows, nColumns, withWE = false)

}


/**
 * The linkage of this memory compiler is to a static list of memories that we don't expect to have datasheets
 * or much of anything besides just the cell libs. Provide just the
 */
trait CannotCompileMemories extends CanCompileMemories {
  val availableMems: Map[Int, Seq[SD]]

  override def getSRAMArray(suggestedRows: Int,
                            suggestedColumns: Int,
                            nPorts: Int,
                            latency: Int)(implicit p: Parameters): Option[SRAMArray] = {
    // first figure out how deep we have to cascade.
    availableMems.get(nPorts) match {
      case Some(memSet) =>
        val maxRows = memSet.map(_.nRows).max
        require(latency * maxRows >= suggestedRows,
          f"Unable to build a $suggestedRows deep memory in $latency cycles. ${(suggestedRows.toFloat / maxRows).ceil.toInt} cycles needed.")
        // find the minimum depth that will make us able to build the cascade
        val chosenDepth = memSet.map(_.nRows).filter(depth => depth * latency >= suggestedRows).min
        // get the widest memory with the selected depth

        /**
         * Sieve of eratosthenes-ish except record shortest sequence to get to a certain width given the basis
         */
        @tailrec
        def sieve(lim: Int, numberChoices: List[Int], originators: Map[Int, List[Int]]):
        Map[Int, List[Int]] = {
          def get_add_list(): Map[Int, List[Int]] = {
            if (numberChoices.isEmpty) return Map.empty
            val inc = numberChoices.head
            val addList = originators.map(o => (o._1 + inc, inc :: o._2))
            val filter = addList.filter { kv =>
              if (kv._1 > lim) false
              else if (!originators.contains(kv._1)) true
              else {
                val other = originators(kv._1)
                if (other.length > kv._2.length) true
                else false
              }
            }
            filter
          }

          val AL = get_add_list()
          if (AL.isEmpty) {
            if (numberChoices.isEmpty) originators
            else sieve(lim, numberChoices.tail, originators)
          } else {
            sieve(lim, numberChoices, originators.removedAll(AL.keys) ++ AL)
          }
        }

        val max_width = 1 << log2Up(suggestedColumns)
        val basis = memSet.map(_.dWidth).distinct.toList
        val sieveRes = sieve(max_width, basis,
          Map.from(basis.map(p => (p, List(p)))))
        val chosenWidth = sieveRes.filter(p => p._1 >= suggestedColumns).toList.minBy(_._1)
        val row = chosenWidth._2.map((chosenDepth, _))
        Some(SRAMArray(List.fill(chosenWidth._2.length)(List.fill(1)(row))))
      case None =>
        // The memory compiler doesn't support SRAMs with the require portage - fail immediately
        None
    }
  }
}

abstract class MemoryCompiler {
  val supportedCorners: Seq[(ProcessCorner, ProcessTemp)]

  val mostPortsSupported: Int

  val isActiveHighSignals: Boolean

  def generateMemoryFactory(nPorts: Int, nRows: Int, nColumns: Int)(implicit p: Parameters): () => BaseModule with HasMemoryInterface
}


private class C_ASIC_MemoryCascade(rows: Int,
                                   dataBits: Int,
                                   cascadeBits: Int,
                                   idx: Int,
                                   nPorts: Int,
                                   withWE: Boolean)(implicit p: Parameters) extends RawModule {
  val totalAddr = log2Up(rows) + cascadeBits
  val io = IO(new MemoryIOBundle(0, 0, nPorts, totalAddr, dataBits, withWE) with withMemoryIOForwarding)
  val mc = p(PlatformKey).asInstanceOf[Platform with HasMemoryCompiler].memoryCompiler

}

object MemoryCompiler {
  var libs: Map[(ProcessCorner, ProcessTemp), Seq[Path]] = Map.empty
  var gds2: Seq[Path] = Seq.empty
  var sram_paths: List[Path] = List.empty

  def mc(implicit p: Parameters) = p(PlatformKey).asInstanceOf[Platform with HasMemoryCompiler].memoryCompiler.asInstanceOf[MemoryCompiler with CanCompileMemories]

  /**
   * Build a large SRAM structure from the provided SRAM cells in the library. This function is expected to be
   * called from a module that will implement the required size and width of the memory with a given latency.
   * This method will implement that behavior given the provided SRAM cells. For this reason, the caller should
   * not implement logic in the module - only call this function. The caller should also have an implicit `io`
   * of type `CMemoryIOBundle` so that we can correctly tie signals.
   *
   * @param Latency   latency of the memory
   * @param dataWidth width of the data
   * @param nRows     number of elements of data in the memory
   * @param nPorts    number of ports for the memory. Not all ports are supported for all memories. This will be
   *                  library and platform limited.
   * @param io        IO in the parent context. Needs to be a CMemoryIOBundle
   */
  def buildSRAM(Latency: Int, dataWidth: Int, nRows: Int, nPorts: Int, withWE: Boolean, allowFallBack: Boolean, freqMHz: Int)(implicit io: MemoryIOBundle, p: Parameters): Unit = {
    def works(latency: Int): Option[(SRAMArray, Float)] = {
      if (withWE)
        mc.asInstanceOf[MemoryCompiler with SupportsWriteEnable].getWESRAMArray(
          nRows, dataWidth, nPorts, latency).map(a => (a, a.characteristics("area").asInstanceOf[Float]))
      else
        mc.getSRAMArray(nRows, dataWidth, nPorts, latency).map(a => (a, a.characteristics("area").asInstanceOf[Float]))
    }

    def rn[T <: Data](x: T): T = {
      val r = Wire(x.cloneType)
      withClock(io.clock.asClock) {
        r := RegNext(x)
      }
      r
    }

    val memOpts = Seq((Latency-2, (x: Data) => rn(x), (x: Data) => rn(x)),
      (Latency-1, (x: Data) => x, (x: Data) => rn(x)),
      (Latency-0, (x: Data) => x, (x: Data) => x)).flatMap {
      case (lat, pre, post) =>
        works(lat).map((_, pre, post))
    }
    if (memOpts.isEmpty) {
      if (allowFallBack) {
//        beethoven.Generation.CLogger.log(s"Failed to find suitable SRAM configuration for ${nPorts}x${nRows}x${dataWidth} at L=${Latency}" +
//          s" Falling back to Register-based memory")
        val mem = Module(new SyncReadMemMem(0, 0, nPorts, nRows, dataWidth, Latency))
        mem.mio.clock := io.clock
        mem.suggestName(s"mem_${nRows}x${dataWidth}x${nPorts}_l${Latency}")
        // hook ports into io
        (0 until nPorts) foreach { port_idx =>
          io.data_out(port_idx) := mem.mio.data_out(port_idx)
          mem.mio.data_in(port_idx) := io.data_in(port_idx)
          mem.mio.read_enable(port_idx) := io.read_enable(port_idx)
          mem.mio.chip_select(port_idx) := io.chip_select(port_idx)
          mem.mio.write_enable(port_idx) := io.write_enable(port_idx)
          mem.mio.addr(port_idx) := io.addr(port_idx)
        }
        return
      } else {
        throw new Exception(s"Failed to find suitable SRAM configuration for ${nPorts}x${nRows}x${dataWidth} at L=${Latency}")
      }
    }
    val ((mem, area), pre, post) = memOpts.minBy(_._1._2)
    var addr_bits_acc = 0


    withClockAndReset(io.clock.asClock, false.B.asAsyncReset) {
      @tailrec
      def scan_shift[T <: Data](a: T, d: Int, acc: List[T] = List.empty): List[T] = {
        if (d == 0) acc.reverse
        else {
          val ap = ShiftReg(a, 1)
          scan_shift(ap, d - 1, ap :: acc)
        }
      }

      val addr_shifts = io.addr.map { addr => scan_shift(addr, mem.array.length) }
      val chip_active_shifts = io.chip_select.map { cs => scan_shift(cs, mem.array.length) }
      val we_shift = io.write_enable.map { we => scan_shift(we, mem.array.length) }
      val re_shift = io.read_enable.map { re => scan_shift(re, mem.array.length) }
      val data_shifts = io.data_in.map { data => scan_shift(data, mem.array.length) }
      val data_stages = Seq.fill(nPorts)(Seq.fill(mem.array.length-1)(Reg(UInt(dataWidth.W))))
      val data_out_wires = Seq.fill(nPorts)(Seq.fill(mem.array.length)(Wire(UInt(dataWidth.W))))


      val l_bits = CLog2Up(mem.array.length)
      val m_bits = CLog2Up(mem.array.head.length)
//      val mem_bits = CLog2Up(mem.array.head.head.head._1)

      def fixActive(a: UInt): UInt = {
        if (mc.isActiveHighSignals) a else (~a).asUInt
      }

      val latency_array = mem.array.zipWithIndex.map { case (rDivSArray, l_idx: Int) =>
        if (l_idx < mem.array.length - 1) {
          (0 until nPorts) foreach { port_idx =>
            data_stages(port_idx)(l_idx) := RegNext(data_out_wires(port_idx)(l_idx))
          }
        }
        val per_port_addr = addr_shifts.map(a => a(l_idx))
        val l_hit = {
          if (l_bits == 0) per_port_addr.map(_ => true.B)
          else {
            val l = per_port_addr.map(a => a.head(l_bits))
            l.map(_ === l_idx.U)
          }
        }
        val muxArray = rDivSArray.zipWithIndex.map { case (bankArray, m_idx) =>
          val m_hit = {
            if (m_bits == 0) per_port_addr.map(_ => true.B)
            else {
              val m = per_port_addr.map(a => a.tail(l_bits).head(m_bits))
              m.map(_ === m_idx.U)
            }
          }
          val lm_hit = l_hit.zip(m_hit).map { case (l, m) => l && m }
          val sramMacroRowOuts: List[List[UInt]] = bankArray.zipWithIndex.map { case ((rows, cols), s_idx: Int) =>
            val d_off = (0 until s_idx).map(bankArray(_)._2).sum
            val mem =
              if (withWE) mc.asInstanceOf[SupportsWriteEnable].generateMemoryFactory(nPorts, rows, cols, withWE)(p)()
              else mc.generateMemoryFactory(nPorts, rows, cols)(p)()
            mem.clocks.foreach(_ := io.clock)
            ((0 until nPorts) map { port_idx =>
              mem.data_in(port_idx) := data_shifts(port_idx)(l_idx)(d_off + cols - 1, d_off)
              mem.read_enable(port_idx) := fixActive(re_shift(port_idx)(l_idx))
              mem.chip_select(port_idx) := fixActive(chip_active_shifts(port_idx)(l_idx) && lm_hit(port_idx))
              mem.write_enable(port_idx) := fixActive(we_shift(port_idx)(l_idx))
              mem.addr(port_idx) := addr_shifts(port_idx)(l_idx).tail(l_bits + m_bits)
              Mux(RegNext(lm_hit(port_idx)),
                mem.data_out(port_idx),
                if (l_idx == 0) 0.U(cols.W) else data_stages(port_idx)(l_idx - 1))
            }).toList
          }
          // sramMacroRowOuts[bank][port]
          val portCats = (0 until nPorts).map { port_idx =>
            Cat(sramMacroRowOuts.map(a => a(port_idx)).reverse)
          }
          portCats.zip(data_out_wires).foreach { case (p, w) => w(l_idx) := p }
        }
      }
      io.data_out.zip(data_out_wires).foreach { case (d, w) => d := w.last }
    }
  }
}
