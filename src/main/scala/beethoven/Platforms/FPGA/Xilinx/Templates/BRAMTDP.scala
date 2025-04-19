package beethoven.Platforms.FPGA.Xilinx.Templates

import beethoven.Floorplanning.DeviceContext
import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._
import beethoven.MemoryStreams.HasMemoryInterface
import beethoven.Platforms.{HasXilinxMem, Platform, PlatformKey}
import beethoven.{BQuiet, BeethovenBuild, BeethovenConstraintHint, ConstraintHintsKey, _}
import beethoven.common.CLog2Up

import java.io.FileWriter
import scala.annotation.tailrec

case class FPGAMemoryPrimitiveConsumer(brams: Int,
                                       urams: Int,
                                       mux_degree: Option[Int],
                                       verilogAnnotations: String,
                                       fileNameAnnotation: String)

private[beethoven] class BRAMTDP(latency: Int,
                                 dataWidth: Int,
                                 nRows: Int,
                                 withWriteEnable: Boolean,
                                 debugName: String,
                               )(implicit p: Parameters) extends BlackBox with HasMemoryInterface {
  val weWidth = if (withWriteEnable) dataWidth / 8 else 1
  val adjDataW = if (withWriteEnable) 8 else dataWidth
  val io = IO(new Bundle {
    val CE = Input(Bool())
    val WEB1 = Input(UInt(weWidth.W))
    val WEB2 = Input(UInt(weWidth.W))
    val OEB1 = Input(Bool())
    val OEB2 = Input(Bool())
    val O1 = Output(UInt(dataWidth.W))
    val O2 = Output(UInt(dataWidth.W))
    val I1 = Input(UInt(dataWidth.W))
    val I2 = Input(UInt(dataWidth.W))
    val A1 = Input(UInt(log2Up(nRows).W))
    val A2 = Input(UInt(log2Up(nRows).W))
    val CSB1 = Input(Bool())
    val CSB2 = Input(Bool())
  })

  override def addr: Seq[UInt] = Seq(io.A1, io.A2)

  override def data_out: Seq[UInt] = Seq(io.O1, io.O2)

  override def data_in: Seq[UInt] = Seq(io.I1, io.I2)

  override def chip_select: Seq[Bool] = Seq(io.CSB1, io.CSB2)

  override def read_enable: Seq[Bool] = Seq(io.OEB1, io.OEB2)

  override def write_enable = Seq(io.WEB1, io.WEB2)

  override def clocks: Seq[Bool] = Seq(io.CE)

  require(
    latency >= 1,
    "Scratchpad latency must be at least 1. Found " + latency
  )
  private val addrWidth = log2Up(nRows)

  val dname_prefix = beethoven.MemoryStreams.Memory.get_name(latency, dataWidth, nRows, 0, 0, 2)
  val (memoryAnnotations, dname_suffix) = {
    if (
      p(ConstraintHintsKey).contains(BeethovenConstraintHint.MemoryConstrained)
    ) {
      val info = BRAMTDP.getMemoryResources(nRows, dataWidth, debugName, isSimple = false, canMux = withWriteEnable)
      BRAMTDP.allocateBRAM(info.brams)
      BRAMTDP.allocateURAM(info.urams)
      (info.verilogAnnotations, info.fileNameAnnotation)
    } else ("", "")
  }

  override val desiredName = f"$dname_prefix$dname_suffix"

  private val memoryRoot = os.pwd / ".memories"
  if (!os.exists(memoryRoot)) os.makeDir(memoryRoot)

  private val component = memoryRoot / f"$desiredName.v"

  BeethovenBuild.addSource(component)

  private val mvR = if (latency >= 3) {
    f"""for (i = 0; i < ${latency-1}; i = i+1) begin
       |      mem_pipe_reg1[i+1] <= mem_pipe_reg1[i];
       |      mem_pipe_reg2[i+1] <= mem_pipe_reg2[i];
       |  end
       |""".stripMargin
  } else ""

  // We need keep hierarchy because in some rare circumstances, cross boundary optimization
  // prevents the memory from being inferred, and further, the memory is completely unrecongized,
  // mapped to a black box, and causes unrecoverable errors during logic synthesis... (Vivado 2022.1)

  // In Vivado 2019.2, this template will complain about multiple drivers to the same memory. In other
  // versions, this code _may_ complain about not being able to infer a memory because they actually
  // expect the read and write procedures to be in separate processes (always blocks).

  def fetch(i: Int): String = {
    if (latency == 1)
      f"memreg$i"
    else {
      if (latency > 2)
        f"mem_pipe_reg$i[${latency-1}]"
      else
        f"mem_pipe_reg$i"
    }
  }

  def pipe(i: Int): String = {
    if (latency >= 2) {
      val deep = if (latency > 2) f" [0:${latency-2}]" else ""
      f"reg [${dataWidth - 1}:0] mem_pipe_reg$i$deep;\n"
    } else ""
  }

  def fwd(i: Int): String = {
    if (latency >= 2) {
      val d = if (latency > 2) "[0]" else ""
      f"  mem_pipe_reg$i$d <= memreg$i;\n"
    } else ""
  }

  val src =
    f"""
       |(* keep_hierarchy = "yes" *)
       |module $desiredName (
       |  input CE,
       |  input [${weWidth - 1}:0] WEB1,
       |  input [${weWidth - 1}:0] WEB2,
       |  input OEB1,
       |  input OEB2,
       |  output [${dataWidth - 1}:0] O1,
       |  input [${dataWidth - 1}:0] I1,
       |  input [${addrWidth - 1}:0] A1,
       |  output [${dataWidth - 1}:0] O2,
       |  input [${dataWidth - 1}:0] I2,
       |  input [${addrWidth - 1}:0] A2,
       |  input CSB1,
       |  input CSB2);
       |
       |$memoryAnnotations
       |reg [${adjDataW - 1}:0] mem [${weWidth - 1}:0] [${nRows - 1}:0];        // Memory Declaration
       |reg [${dataWidth - 1}:0] memreg1;
       |reg [${dataWidth - 1}:0] memreg2;
       |${pipe(1)}${pipe(2)}
       |integer i, gi;
       |always @ (posedge CE)
       |begin
       |${writeF("1", withWriteEnable, weWidth, dataWidth)}
       |${writeF("2", withWriteEnable, weWidth, dataWidth)}
       |${fwd(1)}${fwd(2)}
       |  $mvR
       |end
       |assign O1 = ${fetch(1)};
       |assign O2 = ${fetch(2)};
       |
       |endmodule
       |
       |""".stripMargin

  val fw = new FileWriter(component.toString())
  fw.write(src)
  fw.close()

}

object BRAMTDP {
  private[beethoven] var warningsIssued = 0
  private var bram_used: Map[Int, Int] = Map.empty
  private var uram_used: Map[Int, Int] = Map.empty

  private val uram_dwidth = 72
  private val uram_nrows = 4 * 1024

  private[beethoven] def bram_dwidth(isSimple: Boolean) = List(1, 2, 4, 9, 18, 36) ++ (if (isSimple) Seq(72) else Seq())

  def get_bram_width(requested: Int, isSimple: Boolean): Int = {
    @tailrec
    def help(h: List[Int]): Int = {
      if (h.isEmpty) bram_dwidth(isSimple).max
      else if (requested <= h.head) h.head
      else help(h.tail)
    }

    help(bram_dwidth(isSimple))
  }

  // get bram and uram usage respectively for a given memory
  def getMemoryResources(nRows: Int,
                         dwidth: Int,
                         debugName: String,
                         isSimple: Boolean,
                         canMux: Boolean)(implicit p: Parameters): FPGAMemoryPrimitiveConsumer = {
    def get_n_brams(widthRequested: Int, rowsRequested: Int): Int = {
      val w = get_bram_width(widthRequested, isSimple)
      val rows_per_bram = bram_width2rows(isSimple)(w)
      // if asking for a super wide BRAM, then they're likely going to be cascaded together
      val cascade = if (widthRequested > w) {
        (widthRequested.toFloat / w).ceil.toInt
      } else 1
      (rowsRequested.toFloat / rows_per_bram).ceil.toInt * cascade
    }

    def get_n_urams(widthRequested: Int, rowsRequested: Int): Int = {
      val row_consumption = (rowsRequested.toFloat / uram_nrows).ceil.toInt
      val width_mult = (widthRequested.toFloat / uram_dwidth).ceil.toInt
      width_mult * row_consumption
    }

    val isBigEnoughForURAM = nRows * dwidth >= 2 * 1024 * 64
    val (useMux, canURAM, uRows, uWidth) = if (dwidth >= 64 || !(isBigEnoughForURAM && (dwidth % 8) == 0)) {
      // For cases where we either fit naturally into URAM width or we're trivially unqualified, then just
      // pass along the existing width/height
      (None, dwidth >= 64 && isBigEnoughForURAM, nRows, dwidth)
    } else {
      // Otherwise, see if we can use byte-wise write enable to get away with a wider data bus
      val mult = 1 << (Math.log10(72 / dwidth) / Math.log10(2)).floor.toInt
      if (mult > 1) {
        (Some(mult), true, (nRows.toFloat / mult).ceil.toInt, dwidth * mult)
      } else {
        (None, false, nRows, dwidth)
      }
    }

    // this warning is probably not useful anymore because of the above
//    if (nRows >= 4 * 1024 && dwidth < 64) {
//      if (!p(BeethovenQuiet) && warningsIssued < 1) {
//        System.err.println(
//          s"At least one of the memory modules ($debugName) has a data width less than 64 ($dwidth) but has a total\n" +
//            s"data capacity that makes it appropriate for URAM (applicable for Ultrascale+ devices)\n" +
//            s"This may lead to poor URAM cascading. Consider one of the following optimizations.\n" +
//            s"1. Increase the data width and manually mux the intended bits\n" +
//            s"2. Enable write-enable muxing from the scratchpad configuration."
//        )
//        warningsIssued += 1
//      }
//    }

    val uram_consumption = get_n_urams(uWidth, uRows)
    val bram_consumption = get_n_brams(dwidth, nRows)
    // appropriate data width and at least 90% capacity

    val currentContext = DeviceContext.currentDevice.getOrElse(0)

    val (have_enough_uram, have_enough_bram) = p(PlatformKey) match {
      case pxm: Platform with HasXilinxMem =>
        (uram_used.getOrElse(currentContext, 0) + uram_consumption <= pxm.nURAMs(currentContext),
          bram_used.getOrElse(currentContext, 0) + bram_consumption <= pxm.nBRAMs(currentContext))
      case _ => (true, true)
    }
    val usage = if (canURAM && have_enough_uram)
      FPGAMemoryPrimitiveConsumer(0, uram_consumption, useMux, "(* ram_style = \"ultra\" *)", "_URAM")
    else if (have_enough_bram)
      FPGAMemoryPrimitiveConsumer(bram_consumption, 0, None, "(* ram_style = \"block\" *)", "_BRAM")
    else if (have_enough_uram)
      FPGAMemoryPrimitiveConsumer(0, uram_consumption, None, "(* ram_style = \"ultra\" *)", "_URAM")
    else FPGAMemoryPrimitiveConsumer(0, 0, None, "", "")

    if (platform.isInstanceOf[HasXilinxMem]) {
      val pxm = p(PlatformKey).asInstanceOf[HasXilinxMem]
      //      if (!have_printed.contains(currentContext)) {
      //        have_printed += currentContext
      //      }

      if (!p(BQuiet)) {
        System.err.print(s"\rURAM (d$currentContext): ${uram_used.getOrElse(currentContext, 0)} / ${pxm.nURAMs(currentContext)}\t" +
          f"BRAM (d$currentContext): ${bram_used.getOrElse(currentContext, 0)} / ${pxm.nBRAMs(currentContext)}")
        if (!have_enough_bram && !have_enough_uram) {
          System.err.println(
            s"Memory module $debugName requires $bram_consumption BRAMs and $uram_consumption URAMs,\n" +
              s" but only ${pxm.nBRAMs(currentContext) - bram_used.getOrElse(currentContext, 0)} BRAMs and ${pxm.nURAMs(currentContext) - uram_used.getOrElse(currentContext, 0)} URAMs" +
              s"are available.")
        }
      }
    }

    usage
  }

//  private val have_printed = scala.collection.mutable.Set[Int]()

  private def bram_maxdwidth(isSimple: Boolean) = if (isSimple) 72 else 36

  private def bram_width2rows(isSimple: Boolean) = Map.from({
    Seq(
      (1, 32 * 1024),
      (2, 16 * 1024),
      (4, 8 * 1024),
      (9, 4 * 1024),
      (18, 2 * 1024),
      (36, 1024)) ++ (if (isSimple) Seq((72, 512)) else Seq())
  })

  def allocateBRAM(nBRAM: Int): Unit = {
    val currentContext = DeviceContext.currentDevice.getOrElse(0)
    bram_used = bram_used.updated(currentContext,
      bram_used.getOrElse(currentContext, 0) + nBRAM)
  }

  def allocateURAM(nURAM: Int): Unit = {
    val currentContext = DeviceContext.currentDevice.getOrElse(0)
    uram_used = uram_used.updated(currentContext,
      uram_used.getOrElse(currentContext, 0) + nURAM)
  }

}