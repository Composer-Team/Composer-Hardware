package composer.MemoryStreams.RAM

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._
import chisel3.experimental.BaseModule
import composer.ASICMemoryCompilerKey
import composer.MemoryStreams._

import scala.annotation.tailrec

abstract class MemoryCompiler {
  /**
   * Sequence of available memories for each number of supported ports
   */
  val mems: Map[Int, Seq[SD]]

  val isActiveHighSignals: Boolean

  def generateMemoryFactory(nPorts: Int, nRows: Int, nColumns: Int)(implicit p: Parameters): () => BaseModule with HasMemoryInterface

  def getMemoryName(nPorts: Int, nRows: Int, nColumns: Int): String

  def getMemoryCascade(suggestedRows: Int, suggestedColumns: Int, nPorts: Int, latency: Int): CascadeDescriptor = {
    // first figure out how deep we have to cascade.
    val ms = mems.get(nPorts)
    ms match {
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
        CascadeDescriptor(chosenDepth, chosenWidth._2)
      case None =>
        // The memory compiler doesn't support SRAMs with the require portage. Pass the problem down the line.
        // if the Compiler supports RegMems, then it will succeed, otherwise, fail later
        val d = 1 << log2Up((suggestedRows.toFloat / latency).ceil.toInt)
        CascadeDescriptor(d, Seq(suggestedColumns))
    }
  }
}

private class C_ASIC_MemoryCascade(rows: Int,
                                   dataBits: Int,
                                   cascadeBits: Int,
                                   idx: Int,
                                   nPorts: Int)(implicit p: Parameters) extends RawModule {
  val totalAddr = log2Up(rows) + cascadeBits
  val io = IO(new CMemoryIOBundle(nPorts, totalAddr, dataBits) with withMemoryIOForwarding)
  withClockAndReset(io.clock.asClock, false.B.asAsyncReset) {
    val mem = p(ASICMemoryCompilerKey).generateMemoryFactory(nPorts, rows, dataBits)(p)()
    mem.clocks.foreach(_ := io.clock)
    (0 until nPorts) foreach { port_idx =>
      val cascade_select = if (cascadeBits == 0) true.B else idx.U === io.addr(port_idx).head(cascadeBits)
      io.data_out(port_idx) := mem.data_out(port_idx)
      mem.data_in(port_idx) := io.data_in(port_idx)
      mem.read_enable(port_idx) := io.read_enable(port_idx)
      val CSActiveHigh = if (p(ASICMemoryCompilerKey).isActiveHighSignals) io.chip_select(port_idx) else !io.chip_select(port_idx)
      val selectMe_activeHigh = CSActiveHigh && cascade_select
      val selectMe = if (p(ASICMemoryCompilerKey).isActiveHighSignals) selectMe_activeHigh else !selectMe_activeHigh
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

  /**
   * Build a large SRAM structure from the provided SRAM cells in the library. This function is expected to be
   * called from a module that will implement the required size and width of the memory with a given latency.
   * This method will implement that behavior given the provided SRAM cells. For this reason, the caller should
   * not implement logic in the module - only call this function. The caller should also have an implicit `io`
   * of type `CMemoryIOBundle` so that we can correctly tie signals.
   * @param latency latency of the memory
   * @param dataWidth width of the data
   * @param nRows number of elements of data in the memory
   * @param nPorts number of ports for the memory. Not all ports are supported for all memories. This will be
   *               library and platform limited.
   * @param io IO in the parent context. Needs to be a CMemoryIOBundle
   */
  def buildSRAM(latency: Int, dataWidth: Int, nRows: Int, nPorts: Int)(implicit io: CMemoryIOBundle, p: Parameters): Unit = {
    val memoryStructure = p(ASICMemoryCompilerKey).getMemoryCascade(nRows, dataWidth, nPorts, latency)
    val totalRowBits = log2Up(nRows)
    val cascadeRows = Seq.fill(latency)(memoryStructure.depth)
    val addressBases = cascadeRows.zip(cascadeRows.scan(0)(_ + _)).map {
      case (sz, sum) => sum >> log2Up(sz)
    }

    def al_switch(a: Bool): Bool = {
      if (p(ASICMemoryCompilerKey).isActiveHighSignals) a else !a
    }

    val banks = Seq.tabulate(memoryStructure.bankWidths.length) { bank_idx =>
      val data_offset = (0 until bank_idx).map(memoryStructure.bankWidths(_)).sum
      val bank_width = memoryStructure.bankWidths(bank_idx)
      val banks_i = io.data_in.map(d => d(data_offset + bank_width - 1, data_offset))
      val banks_o = Seq.fill(nPorts)(Wire(UInt(bank_width.W)))
      val cascade = Seq.tabulate(latency)(idx =>
        Module(
          new C_ASIC_MemoryCascade(
            cascadeRows(idx),
            bank_width,
            Math.max(0, totalRowBits - log2Up(cascadeRows(idx))),
            addressBases(idx),
            nPorts
          )
        )
      )
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
        head.io.addr(port_idx) := io.addr(port_idx)
        head.io.data_in(port_idx) := banks_i(port_idx)
        head.io.chip_select(port_idx) := al_switch(io.chip_select(port_idx))
        head.io.write_enable(port_idx) := al_switch(io.write_enable(port_idx))
        head.io.read_enable(port_idx) := al_switch(io.read_enable(port_idx))
        banks_o(port_idx) := tail.io.data_out(port_idx)
      }
      (bank_idx, banks_o)
    }
    (0 until nPorts) foreach { port_idx =>
      val bank_o = banks.map(p => (p._1, p._2(port_idx)))
      io.data_out(port_idx) := Cat(bank_o.sortBy(_._1).reverse.map(_._2))
    }
  }
}
