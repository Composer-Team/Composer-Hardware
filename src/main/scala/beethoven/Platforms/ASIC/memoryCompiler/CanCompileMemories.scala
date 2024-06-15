package beethoven.Platforms.ASIC.memoryCompiler

import beethoven.Generation._
import beethoven.MemoryStreams._
import beethoven.Platforms.ASIC.MemoryCompiler.{gds2, libs, sram_paths}
import beethoven.Platforms.ASIC.ProcessCorner.ProcessCorner
import beethoven.Platforms.ASIC.ProcessTemp.ProcessTemp
import beethoven.Platforms.ASIC.memoryCompiler.CanCompileMemories.getSRAMCharacteristics
import beethoven.Platforms._
import chipsalliance.rocketchip.config
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

  private val sramDatDir: Path = os.Path(BeethovenBuild.beethovenRoot()) / "sramDatGen"
  private val sramVerilogDir: Path = os.Path(BeethovenBuild.beethovenRoot()) / "sramVerilogGen"
  private val sramSynthDir: Path = os.Path(BeethovenBuild.beethovenRoot()) / "sramBuild"


  def get_inst_name(nPorts: Int, nWords: Int, nMux: Int, nBits: Int, withWE: Boolean): String

  def pathWithCorner(sram_base_dir: Path,
                     instname: String,
                     processCorner: ProcessCorner,
                     temperature: ProcessTemp,
                     suffix: String,
                     lib_mode: String = ""): Path

  val generatorsSupported: Seq[Generator.Generator]

  val supportedCorners: Seq[(ProcessCorner, ProcessTemp)]
  sram_paths = Seq.empty // Map.from(supportedCorners.map(q => (q, Seq.empty[Path])))


  val pathSets = sram_paths.distinct.map { dir =>
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
                           freqMHz: Int,
                           nMux: Int, // mux
                           useGenerators: Seq[Generator.Generator],
                           roots: (Path, String),
                           useECC: Boolean = false,
                           useWE: Boolean = false
                          ): Boolean

  def generate(nPorts: Int,
               nWords: Int,
               nBits: Int,
               freqMHz: Int,
               nMux: Int, // mux
               useGenerators: Seq[Generator.Generator],
               roots: (Path, String),
               useECC: Boolean = false,
               useWE: Boolean = false
              ): Path = {
    if (!isLegalConfiguration(nPorts, nWords, nBits, freqMHz, nMux, useGenerators, roots, useECC, useWE)) {
      throw new Exception("Illegal configuration")
    }
    val mb = maxBits(nPorts, nMux, useECC)
    assert(nBits >= 2 && nBits <= mb, f"Max bits supported for this mux ($nMux): $mb, $nBits bits requested")


    val (rootDir, remotePrefix) = roots
    val name = get_inst_name(nPorts, nWords, nMux, nBits, useWE)
    val sram_base_dir = rootDir / name
    if (!os.exists(sram_base_dir)) {
      os.makeDir.all(sram_base_dir)
      useGenerators.map(_.toString.replace("_", "-")) foreach { gen =>
        val (binPath, container) = nPorts match {
          case 1 => ("sram_sp_hdc_svt_rvt_hvt", Seq("ssh", "vlsi"))
          case 2 => ("sram_dp_hdc_svt_rvt_hvt", Seq("ssh", "vlsi"))
        }
        val libArgs = if (gen.equals("synopsys")) f" -libname $name" else ""

        val cmd = binPath + " " + f"$gen -mux $nMux -bits $nBits -words $nWords -write_mask ${if (useWE) "on" else "off"} -frequency $freqMHz -instname $name" +
          (if (useECC) " -ecc" else "") + libArgs

        // currently, the installation on ubuntu is unstable - especially for the sram compiler. Run it remotely on the
        // ECE vlsi node. Set up your ssh config to tunnel through to the machine via a node called `vlsi`
        val remote_cmd = f"mkdir -p srams/$remotePrefix/$name; cd srams/$remotePrefix/$name; " + cmd
        os.proc(container ++ Seq(remote_cmd)).call()
        os.proc(s"rsync", "-ar", f"vlsi:~/srams/$remotePrefix/$name", rootDir).call(stdout = os.Inherit, stderr = os.Inherit)

        if (gen.equals("synopsys")) {
          os.walk(sram_base_dir).filter(_.toString().split("\\.").last.equals("lib")).foreach { pth =>
            val fname = pth.toString().split("/").last
            val bn = pth.baseName
            val remove_syn = bn.substring(0, bn.lastIndexOf("_"))
            os.proc(Seq(
              "/data/install/snps_container/1.8/bin/snps_container",
              "lc_shell",
            )).call(sram_base_dir, stdin = s"read_lib $fname; write_lib -format db $remove_syn; exit"
            )
          }
        }
      }
    }
    sram_base_dir
  }

  def checkPassTiming(instname: String, freqMHz: Int): Boolean = {
    val passes = supportedCorners map { case (corner: ProcessCorner, temp: ProcessTemp) =>
      val chars = getSRAMCharacteristics(pathWithCorner(sramDatDir, instname, corner, temp, ".dat"))
      val t_ns_req = 1000.0 / freqMHz
      //        println("Configuration for " + instname + " at " + freqMHz + " MHz is " + chars("tcyc0") + " ns vs required " + t_ns_req + " ns")
      chars("tcyc0") <= t_ns_req
    } reduce (_ && _)
    //      println("______: pass timing? " + passes + " for " + instname + " at " + freqMHz + " MHz")
    passes
  }

  def getMemoryCascadeOpt(suggestedRows: Int, suggestedColumns: Int, nPorts: Int,
                          latency: Int, freqMHz: Int, withWE: Boolean): Option[CascadeDescriptor] = {
    if (latency < 1) return None

    // figure out rows first
    // minimize mux parameter unless we really need that many rows. Mux makes us multiplicatively wide and
    //   and makes timing harder
    val rows_per_lat = 1 << chisel3.util.log2Up((suggestedRows.toFloat / latency).ceil.toInt)
    val minMux = getMinMux(rows_per_lat, nPorts).getOrElse(return None)
    val mb = maxBits(nPorts, minMux, useECC = false)
    val nBanks = suggestedColumns.toFloat / mb
    val cVec = Seq.fill(nBanks.toInt)(mb) ++
      (if (nBanks.toInt.toFloat == nBanks) Seq() else Seq(suggestedColumns - mb * nBanks.toInt))
    val widestWidth = cVec.max
    val passPorts = nPorts > 0 && nPorts <= 2
    val passMux = isPow2(minMux) && minMux >= 4 && minMux <= 32
    val passWords = rows_per_lat <= maxWords(minMux) && rows_per_lat >= minWords(nPorts, minMux) && (rows_per_lat % wordIncrement(minMux)) == 0
    if (!passPorts || !passMux || !passWords) return None
    cVec.distinct.foreach { sram_width =>
      generate(nPorts, rows_per_lat, sram_width, freqMHz, minMux, Seq(Generator.ascii), (sramDatDir, "dat"),
        useECC = false,
        useWE = withWE)
      if (!checkPassTiming(get_inst_name(nPorts, rows_per_lat, minMux, sram_width, withWE), freqMHz))
        return None
    }

    Some(CascadeDescriptor(Seq.fill(latency)(rows_per_lat), cVec))
  }

  def getMemoryCascade(suggestedRows: Int, suggestedColumns: Int, nPort: Int, latency: Int, reqFreqMHz: Int): Option[CascadeDescriptor] =
    getMemoryCascadeOpt(suggestedRows, suggestedColumns, nPort, latency, reqFreqMHz, withWE = false)

  def getWEMemoryCascade(suggestedRows: Int, suggestedColumns: Int, nPort: Int, latency: Int, reqFreqMHz: Int): Option[CascadeDescriptor] =
    getMemoryCascadeOpt(suggestedRows, suggestedColumns, nPort, latency, reqFreqMHz, withWE = true)


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
        val path = generate(nPorts, nRows, nColumns, p(PlatformKey).clockRateMHz, nMux = minMux,
          useGenerators = Seq(Generator.verilog),
          roots = (sramVerilogDir, "verilog"),
          useWE = withWE)
        BeethovenBuild.addSource(path / f"$instname.v")
      case BuildMode.Synthesis =>
        val path = generate(nPorts, nRows, nColumns, p(PlatformKey).clockRateMHz, nMux = minMux,
          useWE = withWE,
          useGenerators = Seq(Generator.synopsys, Generator.gds2, Generator.vclef_fp, Generator.verilog), roots = (sramSynthDir, "synth"))
        libs = libs.map { case (k, v) => (k, v :+ pathWithCorner(sramSynthDir, instname, k._1, k._2, ".lib")) }
        gds2 = gds2 :+ sramSynthDir / instname / (instname + ".gds2")
        sram_paths = sram_paths :+ path
    }

    () => Module(new SRAM_BBWrapper)
  }

}

