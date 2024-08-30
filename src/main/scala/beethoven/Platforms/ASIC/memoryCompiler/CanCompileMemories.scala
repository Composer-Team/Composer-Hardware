package beethoven.Platforms.ASIC.memoryCompiler

import beethoven.Generation._
import beethoven.MemoryStreams._
import beethoven.Platforms.ASIC.MemoryCompiler.{gds2, libs, sram_paths}
import beethoven.Platforms.ASIC.{ProcessCorner, ProcessTemp}
import beethoven.Platforms.ASIC.ProcessCorner.ProcessCorner
import beethoven.Platforms.ASIC.ProcessTemp.ProcessTemp
import beethoven.Platforms.ASIC.memoryCompiler.CanCompileMemories.getSRAMCharacteristics
import beethoven.Platforms.ASIC.memoryCompiler.GMem.{tCap, tMem, tPow}
import beethoven.Platforms._
import beethoven.platform
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.BaseModule
import chisel3.util._
import os.Path

object CanCompileMemories {
  def getSRAMCharacteristics(asciiDatatable: Path): Map[String, Float] = {
    val lines = os.read(asciiDatatable).split("\n")

    def myFilter(a: String): Boolean = {
      val q = a.split("\\s+")
      (!q(0).equals("#")) && q(1).toFloatOption.isDefined
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
}

trait CanCompileMemories {
  object Generator extends Enumeration {
    val verilog, gds2, synopsys, ascii, vclef_fp = Value
    type Generator = Value
    val all = Seq(verilog, gds2, synopsys, ascii, vclef_fp)
  }

  val maxRowDivs = 8
  val sramTimingPessimism_ns: Double

  private val sramDatDir: Path = os.Path(BeethovenBuild.beethovenRoot()) / "sramDatGen"
  private val sramVerilogDir: Path = os.Path(BeethovenBuild.beethovenRoot()) / "sramVerilogGen"
  private val sramSynthDir: Path = os.Path(BeethovenBuild.beethovenRoot()) / "sramBuild"


  def get_inst_name(nPorts: Int, nWords: Int, nMux: Int, nBits: Int, withWE: Boolean)(implicit p: Parameters): String

  def pathWithCorner(sram_base_dir: Path,
                     instname: String,
                     processCorner: ProcessCorner,
                     temperature: ProcessTemp,
                     suffix: String,
                     lib_mode: String = ""): Path

  val generatorsSupported: Seq[Generator.Generator]

  val supportedCorners: Seq[(ProcessCorner, ProcessTemp)]

  lazy val pathSets = sram_paths.distinct.map { dir =>
    val base_name = dir.toString().split("/").last
    val corn = supportedCorners.map { case (c, t) =>
      val r = CanCompileMemories.getSRAMCharacteristics(pathWithCorner(sramDatDir, base_name, c, t, ".dat"))
      ((c, t), r("tcyc0"))
    }
    val slowestCorner = corn.reduce[((ProcessCorner, ProcessTemp), Float)] { case (a, b) =>
      if (a._2 < b._2) b else a
    }
    val fastestCorner = corn.reduce[((ProcessCorner, ProcessTemp), Float)] { case (a, b) =>
      if (a._2 < b._2) a else b
    }

    (pathWithCorner(sramSynthDir, base_name, slowestCorner._1._1, slowestCorner._1._2, ".db", lib_mode = "_nldm"),
      pathWithCorner(sramSynthDir, base_name, slowestCorner._1._1, slowestCorner._1._2, "_syn.lib", lib_mode = "_nldm"),
      pathWithCorner(sramSynthDir, base_name, fastestCorner._1._1, fastestCorner._1._2, "_syn.lib", lib_mode = "_nldm"),
      sramSynthDir / base_name / (base_name + ".vclef"))
  }

  def maxBits(nPorts: Int,
              nMux: Int,
              useECC: Boolean): Int

  def minWords(nPorts: Int,
               nMux: Int): Int

  def getMinMux(nWords: Int, nPorts: Int): Option[Int]

  def maxWords(nMux: Int): Int

  def wordIncrement(nMux: Int): Int

  def isLegalConfiguration(nPorts: Int,
                           nWords: Int,
                           nBits: Int,
                           nMux: Int, // mux
                           useGenerators: Seq[Generator.Generator],
                           roots: (Path, String),
                           useECC: Boolean = false,
                           useWE: Boolean = false
                          ): Boolean


  def generate(nPorts: Int,
               nWords: Int,
               nBits: Int,
               nMux: Int, // mux
               useGenerators: Seq[Generator.Generator],
               roots: (Path, String),
               useECC: Boolean = false,
               useWE: Boolean = false
              )(implicit p: Parameters): Map[String, Any] = {
    if (!isLegalConfiguration(nPorts, nWords, nBits, nMux, useGenerators, roots, useECC, useWE)) {
      throw new Exception("Illegal configuration")
    }
    val mb = maxBits(nPorts, nMux, useECC)
    assert(nBits >= 2 && nBits <= mb, f"Max bits supported for this mux ($nMux): $mb, $nBits bits requested")


    val (rootDir, remotePrefix) = roots
    val name = get_inst_name(nPorts, nWords, nMux, nBits, useWE)
    val sram_base_dir = rootDir / name
    var mapOut = Map[String, Any]("path" -> sram_base_dir)

    val gens = (useGenerators ++ Seq(Generator.ascii)).distinct

    if (!os.exists(sram_base_dir)) {
      println("main path")
      os.makeDir.all(sram_base_dir)
      gens.map(_.toString.replace("_", "-")) foreach { gen =>
        val binPath = nPorts match {
          case 1 => "sram_sp_hdc_svt_rvt_hvt"
          case 2 => "sram_dp_hdc_svt_rvt_hvt"
        }

        val libArgs = if (gen.equals("synopsys"))
          Seq(f"-libname", name) else Seq()

        val which = try {
          os.proc("which", binPath).call().out.trim
        } catch {
          case _: Throwable => ""
        }
        if (!which.equals("")) {
          // do the command on remote system
          println(s"which is '${which}'")
          val cmd = Seq(binPath,
            gen,
            "-mux", nMux.toString,
            "-bits", nBits.toString,
            "-words", nWords.toString,
            "-write_mask", if (useWE) "on" else "off",
            "-frequency", p(PlatformKey).clockRateMHz.toString,
            "-instname", name
          )
          os.proc(cmd).call(cwd = sram_base_dir, env = Map("_JAVA_OPTIONS" -> ""))
          if (gen.equals("synopsys")) {
            os.walk(sram_base_dir).filter(_.toString().split("\\.").last.equals("lib")).foreach { pth =>
              val fname = pth.toString().split("/").last
              val bn = pth.baseName
              val remove_syn = bn.substring(0, bn.lastIndexOf("_"))
              os.proc(Seq("lc_shell")).call(sram_base_dir, stdin = s"read_lib $fname; write_lib -format db $remove_syn; exit"
              )
            }
          }
        } else {
          val cmd = Seq("ssh", "chriskjellqvist@oak",
            f"cd remote; rm -rf * ; export _JAVA_OPTIONS=; " +
              f"$binPath $gen -mux $nMux -bits $nBits -words $nWords " +
              f"-write_mask ${if (useWE) "on" else "off"} -frequency ${p(PlatformKey).clockRateMHz} -instname $name " +
              (if (useECC) "-ecc " else " ") + libArgs.mkString(" "))
//          println(s"runnig ${cmd.mkString(" ")}")
          os.makeDir.all(sram_base_dir)
          // silence stderr
          os.proc(cmd).call(cwd = sram_base_dir, env = Map("_JAVA_OPTIONS" -> ""), stderr = os.Pipe)
          if (gen.equals("synopsys")) {
            os.walk(sram_base_dir).filter(_.toString().split("\\.").last.equals("lib")).foreach { pth =>
              val fname = pth.toString().split("/").last
              val bn = pth.baseName
              val remove_syn = bn.substring(0, bn.lastIndexOf("_"))
              os.proc(Seq("ssh", "chriskjellqvist@oak", "lc_shell")).call(sram_base_dir, stdin = s"read_lib $fname; write_lib -format db $remove_syn; exit"
              )
            }
          }
          os.proc(s"rsync", "-ar", f"chriskjellqvist@oak:~/remote/*", sram_base_dir).call(stdout = os.Inherit, stderr = os.Inherit)
        }
      }
    }

    val corners = supportedCorners.map { case (corner: ProcessCorner, temp: ProcessTemp) =>
      val chars = getSRAMCharacteristics(pathWithCorner(sramDatDir, name, corner, temp, ".dat"))
      (corner, temp, chars)
    }
    val worstCorner = if (corners.length == 1) corners(0) else corners.reduce { (a, b) =>
      if (a._3("tcyc0") > b._3("tcyc0")) a else b
    }
//    val worstPowerCorner = corners.filter(a => a._1 == ProcessCorner.Typical && a._2 == ProcessTemp.C25) match {
//      case a :: _ => a
//      case _ => if (corners.length == 1) corners (0) else corners.reduce { (a, b) =>
//        if (a._3("icc_p0") > b._3("icc_p0")) a else b
//      }
//    }
    val worstPowerCorner = worstCorner

    // now, put all of the fields in the map
    worstCorner._3.foreach { case (k, v) =>
      mapOut = mapOut.updated(k, v)
    }
    mapOut = mapOut.updatedWith("icc_p0")(_ => Some(worstPowerCorner._3("icc_p0")))

    mapOut
  }

  private def checkPassTiming(instname: String, rDivs: Int)(implicit p: Parameters): (Boolean, Map[String, Float]) = {
    val passes = supportedCorners map { case (corner: ProcessCorner, temp: ProcessTemp) =>
      val chars = getSRAMCharacteristics(pathWithCorner(sramDatDir, instname, corner, temp, ".dat"))
      // use geomy, because x dimension is _usually_ used for wide memories
      val geomy = chars("geomy")
      // ns/mm = ps/um
      val ps_delay_from_route = (rDivs - 1) * (geomy * platform.asInstanceOf[HasTechLib].techLib.worstCaseNSperMM)
      // multiply by two, because we'll have to route back and forth
      val t_ns_req = (1000.0 - 2 * ps_delay_from_route - sramTimingPessimism_ns * 1000) / p(PlatformKey).clockRateMHz
      (chars("tcyc0") <= t_ns_req, chars)
    }
    val pass = passes.forall(_._1)
    val worst_case_characteristics = passes.map(_._2).reduce((a, b) => if (a("tcyc0") > b("tcyc0")) a else b)
    (pass, worst_case_characteristics)
  }

  def getMemoryCascadeOpt(suggestedRows: Int,
                          suggestedColumns: Int,
                          nPorts: Int,
                          latency: Int,
                          withWE: Boolean)(implicit p: Parameters): Option[SRAMArray] = {
    if (latency < 1) return None

    // figure out rows first
    // minimize mux parameter unless we really need that many rows. Mux makes us multiplicatively wide and
    //   and makes timing harder
    val opts = (1 to maxRowDivs).filter { rd =>
      val rows_per_latdiv = 1 << chisel3.util.log2Up((suggestedRows.toFloat / latency / rd).ceil.toInt)
      val minMuxValid = getMinMux(rows_per_latdiv, nPorts).isDefined
      minMuxValid
    }.flatMap { rDivs: Int =>
      val rows_per_latdiv = 1 << chisel3.util.log2Up((suggestedRows.toFloat / latency / rDivs).ceil.toInt)
      val minMux = getMinMux(rows_per_latdiv, nPorts).get
      val mb = maxBits(nPorts, minMux, useECC = false)
      val nBanks = suggestedColumns.toFloat / mb
      val is_jagged = if (suggestedColumns < mb || suggestedColumns % mb != 0) true else false
      val macroArray = {
        val row = List.fill(nBanks.floor.toInt)((rows_per_latdiv, Math.min(mb, suggestedColumns))) ++
          (if (is_jagged)
            List((rows_per_latdiv, suggestedColumns % mb)) else {
            List()
          })
        List.fill(latency)(List.fill(rDivs)(row))
      }
      val flatArray = macroArray.flatten.flatten
      val passPorts = nPorts > 0 && nPorts <= 2
      val passMux = isPow2(minMux) && minMux >= 4 && minMux <= 32
      val passWords =
        rows_per_latdiv <= maxWords(minMux) &&
          rows_per_latdiv >= minWords(nPorts, minMux) &&
          (rows_per_latdiv % wordIncrement(minMux)) == 0
      if (!passPorts || !passMux || !passWords) {
//        System.err.println(s"Failed due to (#ports=${nPorts}, #mux=${minMux}, #words=${rows_per_latdiv}): (${!passPorts} ${!passMux} ${!passWords}")
        None
      } else {
        val chars = Map.from(flatArray.distinct.map { case (sram_rows, sram_width) =>
          generate(nPorts, sram_rows, sram_width, minMux, Seq(Generator.ascii), (sramDatDir, "dat"), useWE = withWE)
          val (pass, wc_chars) = checkPassTiming(get_inst_name(nPorts, rows_per_latdiv, minMux, sram_width, withWE), rDivs)
          if (!pass) {
//            System.err.println(s"Failure due to timing: ${wc_chars("t_cyc0")}")
            ((sram_rows, sram_width), None)
          }
          else ((sram_rows, sram_width), Some(wc_chars))
        })
        if (chars.values.exists(_.isEmpty)) {
          None
        } else {
          val area = flatArray.map { tup =>
            val characteristics = chars(tup).get
            characteristics("geomx") * characteristics("geomy") / 1000 / 1000
          }.product
          Some(SRAMArray(macroArray, Map("area" -> area)))
        }
      }
    }
    if (opts.isEmpty) None
    else Some(opts.minBy(_.characteristics("area").asInstanceOf[Float]))
  }

  def getSRAMArray(suggestedRows: Int,
                   suggestedColumns: Int,
                   nPort: Int,
                   latency: Int)(implicit p: Parameters): Option[SRAMArray] =
    getMemoryCascadeOpt(suggestedRows, suggestedColumns, nPort, latency, withWE = false)

  def getWESRAMArray(suggestedRows: Int,
                     suggestedColumns: Int,
                     nPort: Int,
                     latency: Int)(implicit p: Parameters): Option[SRAMArray] =
    getMemoryCascadeOpt(suggestedRows, suggestedColumns, nPort, latency, withWE = true)


  def generateMemoryFactory(nPorts: Int,
                            nRows: Int,
                            nColumns: Int,
                            withWE: Boolean)(implicit p: config.Parameters):
  () => BaseModule with HasMemoryInterface = {
    class ARM_SRAM_BB_SP extends BlackBox {
      val io = IO(new Bundle {
        val Q = Output(UInt(nColumns.W))
        val CLK = Input(Bool())
        val CEN = Input(Bool())
        val WEN = Input(UInt(if (withWE) (nColumns / 8).W else 1.W))
        val A = Input(UInt(log2Up(nRows).W))
        val D = Input(UInt(nColumns.W))
        val EMA = Input(UInt(3.W))
        val RETN = Input(Bool())
      })
      override val desiredName = get_inst_name(nPorts, nRows, getMinMux(nRows, nPorts).get, nColumns, withWE)
    }

    class DPSRAM_BBWrapper extends BlackBox {
      val io = IO(new Bundle {
        val QA = Output(UInt(nColumns.W))
        val CLKA = Input(Bool())
        val CENA = Input(Bool())
        val WENA = Input(UInt(if (withWE) (nColumns / 8).W else 1.W))
        val AA = Input(UInt(log2Up(nRows).W))
        val DA = Input(UInt(nColumns.W))

        val QB = Output(UInt(nColumns.W))
        val CLKB = Input(Bool())
        val CENB = Input(Bool())
        val WENB = Input(UInt(if (withWE) (nColumns / 8).W else 1.W))
        val AB = Input(UInt(log2Up(nRows).W))
        val DB = Input(UInt(nColumns.W))

        val EMAA = Input(UInt(3.W))
        val EMAB = Input(UInt(3.W))
        val RETN = Input(Bool())
      })
      override val desiredName = get_inst_name(nPorts, nRows, getMinMux(nRows, nPorts).get, nColumns, withWE)
    }


    class SRAM_BBWrapper extends RawModule with HasMemoryInterface {
      val io = IO(new Bundle {
        val data_in = Input(Vec(nPorts, UInt(nColumns.W)))
        val data_out = Output(Vec(nPorts, UInt(nColumns.W)))
        val addr = Input(Vec(nPorts, UInt(log2Up(nRows).W)))
        val chip_select = Input(Vec(nPorts, Bool()))
        val read_enable = Input(Vec(nPorts, Bool()))
        val write_enable = Input(Vec(nPorts, Bool()))
        val clocks = Input(Bool())
      })
      if (nPorts == 1) {
        val m = Module(new ARM_SRAM_BB_SP)
        m.io.A := io.addr(0)
        m.io.D := io.data_in(0)
        m.io.CLK := io.clocks(0)
        m.io.CEN := io.chip_select(0)
        m.io.WEN := io.write_enable(0)
        io.data_out(0) := m.io.Q

        m.io.RETN := 1.U
        m.io.EMA := 0.U
      } else {
        val m = Module(new DPSRAM_BBWrapper)
        m.io.AA := io.addr(0)
        m.io.DA := io.data_in(0)
        m.io.CLKA := io.clocks(0)
        m.io.CENA := io.chip_select(0)
        m.io.WENA := io.write_enable(0)
        io.data_out(0) := m.io.QA

        m.io.AB := io.addr(1)
        m.io.DB := io.data_in(1)
        m.io.CLKB := io.clocks(0)
        m.io.CENB := io.chip_select(1)
        m.io.WENB := io.write_enable(1)
        io.data_out(1) := m.io.QB


        m.io.RETN := 1.U
        m.io.EMAA := 0.U
        m.io.EMAB := 0.U
      }

      override def data_in: Seq[UInt] = io.data_in

      override def data_out: Seq[UInt] = io.data_out

      override def addr: Seq[UInt] = io.addr

      override def chip_select: Seq[Bool] = io.chip_select

      override def read_enable: Seq[Bool] = io.read_enable

      override def write_enable: Seq[Bool] = io.write_enable

      override def clocks: Seq[Bool] = Seq(io.clocks)
    }
    val minMux = getMinMux(nRows, nPorts).getOrElse(throw new Exception("No valid mux found for this configuration"))
    val instname = get_inst_name(nPorts, nRows, minMux, nColumns, withWE)

    p(BuildModeKey) match {
      case BuildMode.Simulation =>
        // link in verilogs
        val path = generate(nPorts, nRows, nColumns, nMux = minMux,
          useGenerators = Seq(Generator.verilog),
          roots = (sramVerilogDir, "verilog"),
          useWE = withWE)(p)("path").asInstanceOf[Path]
        BeethovenBuild.addSource(path / f"$instname.v")
      case BuildMode.Synthesis =>
        val map = generate(nPorts, nRows, nColumns, nMux = minMux,
          useWE = withWE,
          useGenerators = Seq(Generator.synopsys, Generator.gds2, Generator.vclef_fp, Generator.verilog), roots = (sramSynthDir, "synth"))
        libs = libs.map { case (k, v) => (k, v :+ pathWithCorner(sramSynthDir, instname, k._1, k._2, ".lib")) }
        gds2 = gds2 :+ sramSynthDir / instname / (instname + ".gds2")

        sram_paths = sram_paths :+ map("path").asInstanceOf[os.Path]
        val x = map("geomx").asInstanceOf[Float]
        val y = map("geomy").asInstanceOf[Float]
        tMem = tMem + x * y
        tPow = tPow + map("icc_p0").asInstanceOf[Float]
        tCap = tCap + nRows * nColumns / 8
        System.out.printf("\rTotal Mem: %.4f µm^2, Total MPow: %.4f mW, Total Capacity %.4fMB", tMem, tPow, tCap / 1024 / 1024)
    }

    () => Module(new SRAM_BBWrapper)
  }
}

object GMem {
  var tMem: Double = 0.0
  var tPow: Double = 0.0
  var tCap: Double = 0
}

