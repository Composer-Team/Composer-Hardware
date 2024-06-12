package beethoven.Platforms.ASIC

import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.BaseModule
import chisel3.util._
import beethoven.MemoryStreams.RAM.SyncReadMemMem
import beethoven.MemoryStreams._
import beethoven.Platforms.ASIC.ProcessCorner.ProcessCorner
import beethoven.Platforms.ASIC.ProcessTemp.ProcessTemp
import beethoven.Platforms._

import scala.annotation.tailrec

/**
 * The linkage of this memory compiler goes to an actual program that can compile memories, their datasheets, etc...
 * This in an important distinction because it makes it much harder to enumerate all of the memories in a search for
 * a memory cascade. To deal with this, such a compiler must provide its own search.
 */
trait CanCompileMemories {
  def getMemoryCascade(suggestedRows: Int, suggestedColumns: Int, nPort: Int, latency: Int, reqFreqMHz: Int): Option[CascadeDescriptor]
}

trait SupportsWriteEnable {
  def getWEMemoryCascade(suggestedRows: Int, suggestedColumns: Int, nPort: Int, latency: Int, reqFreqMHz: Int): Option[CascadeDescriptor]

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

  def getMemoryCascade(suggestedRows: Int, suggestedColumns: Int, nPorts: Int, latency: Int, reqFreqMHz: Int = 0): Option[CascadeDescriptor] = {
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
        Some(CascadeDescriptor(Seq.fill(chosenWidth._2.length)(chosenDepth), chosenWidth._2))
      case None =>
        // The memory compiler doesn't support SRAMs with the require portage. Pass the problem down the line.
        // if the Compiler supports RegMems, then it will succeed, otherwise, fail later
        val d = 1 << log2Up((suggestedRows.toFloat / latency).ceil.toInt)
        Some(CascadeDescriptor(Seq(d), Seq(suggestedColumns)))
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
  withClockAndReset(io.clock.asClock, false.B.asAsyncReset) {
    val mem =
      if (withWE) mc.asInstanceOf[SupportsWriteEnable].generateMemoryFactory(nPorts, rows, dataBits, withWE)(p)()
      else mc.generateMemoryFactory(nPorts, rows, dataBits)(p)()
    mem.clocks.foreach(_ := io.clock)
    (0 until nPorts) foreach { port_idx =>
      val cascade_select = if (cascadeBits == 0) true.B else idx.U === io.addr(port_idx).head(cascadeBits)
      io.data_out(port_idx) := mem.data_out(port_idx)
      mem.data_in(port_idx) := io.data_in(port_idx)
      mem.read_enable(port_idx) := io.read_enable(port_idx)
      val CSActiveHigh = if (mc.isActiveHighSignals) io.chip_select(port_idx) else !io.chip_select(port_idx)
      val selectMe_activeHigh = CSActiveHigh && cascade_select
      val selectMe = if (mc.isActiveHighSignals) selectMe_activeHigh else !selectMe_activeHigh
      mem.chip_select(port_idx) := selectMe
      mem.write_enable(port_idx) := io.write_enable(port_idx)
      mem.addr(port_idx) := io.addr(port_idx).tail(cascadeBits)
      withClock(io.clock.asClock) {
        io.addr_FW(port_idx) := RegNext(io.addr(port_idx))
        io.read_enable_FW(port_idx) := RegNext(io.read_enable(port_idx))
        io.write_enable_FW(port_idx) := RegNext(io.write_enable(port_idx))
        io.chip_select_FW(port_idx) := RegNext(io.chip_select(port_idx) ^ selectMe_activeHigh)
        val cascade_select_stage = RegNext(cascade_select)
        val data_in_stage = RegNext(io.data_in(port_idx))
        io.data_out(port_idx) := Mux(cascade_select_stage, mem.data_out(port_idx), data_in_stage)
      }
    }
  }
}

object MemoryCompiler {

  def mc(implicit p: Parameters) = p(PlatformKey).asInstanceOf[Platform with HasMemoryCompiler].memoryCompiler.asInstanceOf[MemoryCompiler with CanCompileMemories]

  /**
   * Build a large SRAM structure from the provided SRAM cells in the library. This function is expected to be
   * called from a module that will implement the required size and width of the memory with a given latency.
   * This method will implement that behavior given the provided SRAM cells. For this reason, the caller should
   * not implement logic in the module - only call this function. The caller should also have an implicit `io`
   * of type `CMemoryIOBundle` so that we can correctly tie signals.
   *
   * @param latency   latency of the memory
   * @param dataWidth width of the data
   * @param nRows     number of elements of data in the memory
   * @param nPorts    number of ports for the memory. Not all ports are supported for all memories. This will be
   *                  library and platform limited.
   * @param io        IO in the parent context. Needs to be a CMemoryIOBundle
   */
  def buildSRAM(Latency: Int, dataWidth: Int, nRows: Int, nPorts: Int, withWE: Boolean, allowFallBack: Boolean)(implicit io: MemoryIOBundle, p: Parameters): Unit = {
    val (memory_chain_latency, preStage, postStage) = {
      def works(latency: Int): Boolean = {
        if (withWE)
          mc.asInstanceOf[MemoryCompiler with SupportsWriteEnable].getWEMemoryCascade(
            nRows, dataWidth, nPorts, latency, p(PlatformKey).clockRateMHz).isDefined
        else
          mc.getMemoryCascade(nRows, dataWidth, nPorts, latency, p(PlatformKey).clockRateMHz).isDefined
      }

      def rn[T <: Data](x: T): T = {
        val r = Wire(x.cloneType)
        withClock(io.clock.asClock) {
          r := RegNext(x)
        }
        r
      }

      if (works(Latency - 2)) (Latency - 2, (x: Data) => rn(x), (x: Data) => rn(x))
      else if (works(Latency - 1)) (Latency - 1, (x: Data) => x, (x: Data) => rn(x))
      else if (works(Latency)) (Latency, (x: Data) => x, (x: Data) => x)
      else {
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
        if (allowFallBack) {
          beethoven.Generation.CLogger.log(s"Failed to find suitable SRAM configuration for ${nPorts}x${nRows}x${dataWidth} at L=${Latency}" +
            s" Falling back to Register-based memory")
          return
        } else {
          beethoven.Generation.CLogger.log(s"Failed to find suitable SRAM configuration for ${nPorts}x${nRows}x${dataWidth} at L=${Latency}")
          throw new Exception()
        }
      }
    }
    val memoryStructure = if (withWE)
      mc.asInstanceOf[SupportsWriteEnable].getWEMemoryCascade(
        nRows, dataWidth, nPorts, memory_chain_latency, p(PlatformKey).clockRateMHz).get
    else mc.getMemoryCascade(nRows, dataWidth, nPorts, memory_chain_latency, p(PlatformKey).clockRateMHz).get
    val totalRowBits = log2Up(nRows)
    val cascadeRows = memoryStructure.depths
    val addressBases = cascadeRows.zip(cascadeRows.scan(0)(_ + _)).map {
      case (sz, sum) => sum >> log2Up(sz)
    }

    def al_switch(a: UInt): UInt =
      if (mc.isActiveHighSignals) a else (~a).asUInt

    val banks = Seq.tabulate(memoryStructure.bankWidths.length) { bank_idx =>
      val data_offset = (0 until bank_idx).map(memoryStructure.bankWidths(_)).sum
      val bank_width = memoryStructure.bankWidths(bank_idx)
      val banks_i = io.data_in.map(d => d(data_offset + bank_width - 1, data_offset))
      val banks_o = Seq.fill(nPorts)(Wire(UInt(bank_width.W)))
      val cascade = Seq.tabulate(memory_chain_latency) { idx =>
        val m = Module(
          new C_ASIC_MemoryCascade(
            cascadeRows(idx),
            bank_width,
            Math.max(0, totalRowBits - log2Up(cascadeRows(idx))),
            addressBases(idx),
            nPorts,
            withWE
          )
        )
        m.suggestName(s"mem_${nRows}x${dataWidth}x${nPorts}_l${memory_chain_latency}_c${bank_idx}_r${idx}}")
        m
      }
      cascade zip cascade.tail foreach { case (front, back) =>
        back.io.addr <> front.io.addr_FW
        back.io.chip_select <> front.io.chip_select_FW
        back.io.data_in <> front.io.data_out
        back.io.write_enable <> front.io.write_enable_FW
        back.io.read_enable <> front.io.read_enable_FW
      }
      cascade.foreach { csc => csc.io.clock := io.clock }
      val head = cascade.head
      val tail = cascade.last
      (0 until nPorts) foreach { port_idx =>
        head.io.addr(port_idx) := preStage(io.addr(port_idx))
        head.io.data_in(port_idx) := preStage(banks_i(port_idx))
        head.io.chip_select(port_idx) := preStage(al_switch(io.chip_select(port_idx)))
        head.io.write_enable(port_idx) := preStage(al_switch(io.write_enable(port_idx)))
        head.io.read_enable(port_idx) := preStage(al_switch(io.read_enable(port_idx)))
        banks_o(port_idx) := tail.io.data_out(port_idx)
      }
      (bank_idx, banks_o, cascade.map(_.name))
    }

    withClock(io.clock.asClock) {
      (0 until nPorts) foreach { port_idx =>
        val bank_o = banks.map(p => (p._1, p._2(port_idx)))
        io.data_out(port_idx) := postStage(Cat(bank_o.sortBy(_._1).reverse.map(_._2)))
      }
    }
  }
}
