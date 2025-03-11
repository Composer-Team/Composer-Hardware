package beethoven.Platforms.ASIC.memoryCompiler

import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chisel3._
import chisel3.experimental.BaseModule
import chisel3.util._
import beethoven.MemoryStreams.RAM.SyncReadMemMem
import beethoven.MemoryStreams._
import beethoven.Platforms.ASIC.memoryCompiler.MemoryCompiler._
import beethoven.Platforms.ASIC.memoryCompiler.MemoryCompilerDeliverable.{Datasheet, MemoryCompilerDeliverable}
import beethoven.Platforms._
import beethoven.common.{CLog2Up, ShiftReg}
import beethoven.{BuildMode, platform}
import os.Path

import scala.annotation.tailrec

/**
 * The linkage of this memory compiler goes to an actual program that can compile memories, their datasheets, etc...
 * This in an important distinction because it makes it much harder to enumerate all of the memories in a search for
 * a memory cascade. To deal with this, such a compiler must provide its own search.
 */

trait SupportsWriteEnable {
  def getWESRAMArray(suggestedRows: Int, suggestedColumns: Int, nPort: Int, latency: Int)(implicit p: Parameters): Option[SRAMArray]

  def generateMemoryFactory(sc: sramChar_t, withWE: Boolean)(implicit p: config.Parameters): () => BaseModule with HasMemoryInterface

  def generateMemoryFactory(sc: sramChar_t)(implicit p: config.Parameters): () => BaseModule with HasMemoryInterface =
    generateMemoryFactory(sc, withWE = false)

}


object MemoryCompilerDeliverable extends Enumeration {
  val Datasheet, GDS, VERILOG, LEF, LIB, MILKYWAY, LVS = Value
  type MemoryCompilerDeliverable = Value
}

/**
 * The linkage of this memory compiler is to a static list of memories that we don't expect to have datasheets
 * or much of anything besides just the cell libs. Provide just the
 */
abstract class MemoryCompiler {

  val mostPortsSupported: Int

  val isActiveHighSignals: Boolean

  def generateMemoryFactory(char_t: sramChar_t)(implicit p: Parameters): () => BaseModule with HasMemoryInterface

  def isLegalConfiguration(char_t: sramChar_t): Boolean

  val sramTimingPessimism_ns = 0.05

  val supports_onlyPow2: Boolean

  val supportedCorners: Seq[String]

  var worst_corners_found: Seq[String] = Seq.empty

  val datasheet_timing_key: String
  val datasheet_geomx_key: String
  val datasheet_geomy_key: String
  val datasheet_power_key: String

  val deliverablesSupported: Seq[MemoryCompilerDeliverable]

  /** From a behavioral perspective, all we need is an array of N rows by M columns
   * However, SRAM compilers allow a lot of freedom (ECC, EMA, banking, muxing, etc...) that provide the same _function_
   * but with different PPA. In general, we can't know what all the things that the memory compiler is going to ask,
   * so the user should define this function that enumerates as many configurations for that cell as the user wants
   *
   * The plan will be to find the configuration among these that meets timing AND minimizes area (and therefore probably power)
   * */
  def getFeasibleConfigurations(rows: Int, cols: Int, ports: Int, withWriteEnable: Boolean): Seq[sramChar_t]

  def getDeliverable(sc: sramChar_t, deliverable: MemoryCompilerDeliverable)(implicit p: Parameters): sramChar_t

  def collateDeliverables(sc: sramChar_t, deliverables: Seq[MemoryCompilerDeliverable])(implicit p: Parameters): sramChar_t = {
    if (!isLegalConfiguration(sc)) {
      throw new Exception("Illegal configuration")
    }
    // always generate the wdatasheet
    val gens = (deliverables ++ Seq(MemoryCompilerDeliverable.Datasheet)).distinct

    val outputs = gens.map { g =>
      val dl = getDeliverable(sc, g)
      dl
    }.reduce(_ ++ _)

    val worstCorner = outputs(SRAMDatasheetPaths).map { dpath =>
      val chars = getSRAMCharacteristics(dpath)
      (dpath, chars(datasheet_timing_key), chars(datasheet_geomx_key) * chars(datasheet_geomy_key), chars(datasheet_power_key))
    }.maxBy(_._2)

    supportedCorners.filter(a => worstCorner._1.last.contains(a)).foreach { a =>
      worst_corners_found = (worst_corners_found :+ a).distinct
    }
    println("CRITICAL CORNERS: " + worst_corners_found.mkString(", "))

    sc ++ outputs ++ Parameters((_, _, _) => {
      case SRAMArea => worstCorner._3
      case SRAMPower => worstCorner._4
      case SRAMCycleTime => worstCorner._2
    })
  }

  def getMemoryCascadeOpt(suggestedRows: Int,
                          suggestedColumns: Int,
                          nPorts: Int,
                          latency: Int,
                          withWE: Boolean)(implicit p: Parameters): Option[SRAMArray] = {
    if (latency < 1) return None
    println(s"ASKING FOR SUGGESTED ROWS ${suggestedRows} l = ${latency}")
    // (latency-1)*x + y >= suggestedRows & x,y are powers of two
    val (rx, ry) = {
      if (latency == 1) {
        if (supports_onlyPow2) {
          (1 << log2Up(suggestedRows), -1)
        } else
          (suggestedRows, -1)
      } else {
        val x = 1 << log2Up((suggestedRows.toFloat / latency).ceil.toInt)
        val y = if (supports_onlyPow2) 1 << log2Up(suggestedRows - x * (latency - 1)) else suggestedRows - x * (latency - 1)
        (x, y)
      }
    }
    val rs = Seq(rx, ry).filter(_ > 0).distinct

    val (wx, wy) = {
      if (suggestedColumns <= 128) {
        (suggestedColumns, -1)
      } else {
        (128, suggestedColumns % 128)
      }
    }



    val ws = Seq(wx, wy).filter(_ > 0).distinct

    println("rs: " + rs)
    println("ws: " + ws)

    val combos = rs.flatMap(a => ws.map(b => (a, b))).distinct

    val combos_eval = combos.map { case (qr, qc) =>
      val feasible = getFeasibleConfigurations(qr, qc, nPorts, withWE)
      val maxTcyc = (1000.0 / platform.clockRateMHz) - sramTimingPessimism_ns
      val evaled = feasible.map(a => (a, collateDeliverables(a, Seq(Datasheet))))
      val validConfigs = evaled.filter(a => a._2(SRAMCycleTime) < maxTcyc)
      if (validConfigs.isEmpty) {
        System.err.println(f"Failed to find valid configuration for $qr x $qc, timing_req: $maxTcyc, ${evaled.map(_._2(SRAMCycleTime))}")
        return None
      } else {
        System.err.println(f"Found ${validConfigs.length} valid configurations for $qr x $qc, timing_req: $maxTcyc, ${validConfigs.map(_._2(SRAMCycleTime))}")
      }
      val bestValid = evaled.minBy(_._2(SRAMArea))._2
      ((qr, qc), bestValid)
    }

    Some(SRAMArray(
      List.tabulate(latency) {
        l => // for each row
          List.tabulate((suggestedColumns.toFloat / 128).ceil.toInt) {
            c => // for each bank
              val csum = (c + 1) * 128
              val usum = c * 128
              // if suggested columns is higher than csum, then 128w is implied
              // if suggested columns is lower, then this is the last column and we should be suggestedColumns - usum oclumsn wide

              val w = if (suggestedColumns >= csum) 128
              else suggestedColumns - usum
              val r = if (l == latency - 1 && l != 0) ry else rx
              println("R IS " + r)
              def eq_pr(a: (Int, Int), b: (Int, Int)): Boolean = a._1 == b._1 && a._2 == b._2
              combos_eval.find(a => eq_pr(a._1, (r, w))).get._2
          }
      }))
  }

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

object SRAMRows extends Field[Int]

object SRAMColumns extends Field[Int]

object SRAMPorts extends Field[Int]

object SRAMWriteEnable extends Field[Boolean]

object SRAMGDSPaths extends Field[Seq[os.Path]]

object SRAMVerilogPaths extends Field[Seq[os.Path]]

object SRAMLEFPaths extends Field[Seq[os.Path]]

object SRAMLibPaths extends Field[Seq[os.Path]]

object SRAMDatasheetPaths extends Field[Seq[os.Path]]

object SRAMLVSPaths extends Field[Seq[os.Path]]

object SRAMPower extends Field[Float]

object SRAMArea extends Field[Float]

object SRAMCycleTime extends Field[Float]

object MemoryCompiler {
  var libs: Seq[Path] = Seq.empty
  var gds2: Seq[Path] = Seq.empty
  var lefs: Seq[Path] = Seq.empty
  var sram_paths: List[Path] = List.empty


  // sram characteristics (nRows, nCols, nBanks, etc...)
  type sramChar_t = Parameters


  def getSRAMCharacteristics(asciiDatatable: Path): Map[String, Float] = {
    val lines = os.read(asciiDatatable).split("\n")

    def myFilter(a: String): Boolean = {
      if (a.isEmpty) false
      else if (a(0) == '#') false
      else {
        val q = a.split("\\s+")
        if (q.length < 2) false
        else q(1).toFloatOption.isDefined
      }
    }

    Map.from[String, Float](lines.filter(myFilter)
      map { ln: String =>
      val members = ln.split("\\s+")
      val name: String = members(0)
      val f: Float = members(1).toFloat
      val q: (String, Float) = (name, f)
      q
    })
  }


  def mc(implicit p: Parameters) = p(PlatformKey).asInstanceOf[Platform with HasMemoryCompiler].memoryCompiler

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
        mc.getMemoryCascadeOpt(nRows, dataWidth, nPorts, latency, withWE).map { q =>
          (q, q.array.flatten.map(s => s(SRAMArea)).sum)
        }
      //          .map(a => (a, a.characteristics("area").asInstanceOf[Float]))
    }

    def rn[T <: Data](x: T): T = {
      val r = Wire(x.cloneType)
      withClock(io.clock.asClock) {
        r := RegNext(x)
      }
      r
    }

    val memOpts = Seq((Latency - 2, (x: Data) => rn(x), (x: Data) => rn(x)),
      (Latency - 1, (x: Data) => x, (x: Data) => rn(x)),
      (Latency - 0, (x: Data) => x, (x: Data) => x)).flatMap {
      case (lat, pre, post) =>
        works(lat).map((_, pre, post))
    }
    if (memOpts.isEmpty) {
      if (allowFallBack) {
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
    val ((mem, _), _, _) = memOpts.minBy(_._1._2)

    withClockAndReset(io.clock.asClock, false.B.asAsyncReset) {
      val addr_shifts = io.addr.map { addr => ScanShifter(addr, mem.array.length) }
      val chip_active_shifts = io.chip_select.map { cs => ScanShifter(cs, mem.array.length) }
      val we_shift = io.write_enable.map { we => ScanShifter(we, mem.array.length) }
      val re_shift = io.read_enable.map { re => ScanShifter(re, mem.array.length) }
      val data_shifts = io.data_in.map { data => ScanShifter(data, mem.array.length) }
      val data_stages = Seq.fill(nPorts)(Seq.fill(mem.array.length - 1)(Reg(UInt(dataWidth.W))))
      val data_out_wires = Seq.fill(nPorts)(Seq.fill(mem.array.length)(Wire(UInt(dataWidth.W))))

      val l_bits = CLog2Up(mem.array.length)
      println("lbits: " + l_bits)

      def fixActive(a: UInt): UInt = {
        if (mc.isActiveHighSignals) a else (~a).asUInt
      }


      // each stage increases the depth of the array
      println(s"RDIVAR IS ${mem.array.length}x${mem.array(0).length}")
      val latency_array = mem.array.zipWithIndex.map { case (bankArray, l_idx: Int) =>
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
        val sramMacroRowOuts: List[List[UInt]] = bankArray.zipWithIndex.map { case (sd, s_idx: Int) =>
          val cols = sd(SRAMColumns)
          val d_off = (0 until s_idx).map(bankArray(_)(SRAMColumns)).sum
          val mem =
            if (withWE) mc.asInstanceOf[SupportsWriteEnable].generateMemoryFactory(sd, withWE)(p)()
            else mc.generateMemoryFactory(sd)(p)()
          println(f"generated: ${sd(SRAMRows)}x${sd(SRAMColumns)}")
          mem.clocks.foreach(_ := io.clock)
          ((0 until nPorts) map { port_idx =>
            mem.data_in(port_idx) := data_shifts(port_idx)(l_idx)(d_off + cols - 1, d_off)
            mem.read_enable(port_idx) := fixActive(re_shift(port_idx)(l_idx))
            mem.chip_select(port_idx) := fixActive(chip_active_shifts(port_idx)(l_idx).asBool && l_hit(port_idx).asBool)
            mem.write_enable(port_idx) := fixActive(we_shift(port_idx)(l_idx))
            mem.addr(port_idx) := addr_shifts(port_idx)(l_idx).tail(l_bits)
            Mux(RegNext(l_hit(port_idx)),
              mem.data_out(port_idx),
              if (l_idx == 0) 0.U(cols.W) else data_stages(port_idx)(l_idx - 1))
          }).toList
        }
        val portCats = (0 until nPorts).map { port_idx =>
          Cat(sramMacroRowOuts.map(a => a(port_idx)).reverse)
        }
        portCats.zip(data_out_wires).foreach { case (p, w) => w(l_idx) := p }
      }
      io.data_out.zip(data_out_wires).foreach { case (d, w) => d := w.last }
    }
  }
}

object GMem {
  var tMem: Double = 0.0
  var tPow: Double = 0.0
  var tCap: Double = 0
}
