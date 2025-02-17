package beethoven.common

import beethoven.BeethovenBuild
import beethoven.MemoryStreams.Memory
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.ValName

import scala.annotation.tailrec

/**
 * I've run into trouble in the past where Chisel3 shiftregisters give me
 * unexpected behavior, so I have these instead.
 */

class ShiftReg[T <: Data](n: Int,
                          gen: T,
                          resetVal: Option[T] = None,
                          with_enable: Boolean = false,
                          allow_fpga_shreg: Boolean = true) extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Bool())
    val reset = if (resetVal.isDefined) Some(Input(Bool())) else None
    val enable = if (with_enable) Some(Input(Bool())) else None
    val in = Input(gen.cloneType)
    val out = Output(gen.cloneType)
  })

  val (resetString, resetName, reset_code_sig, reset_sig) = resetVal match {
    case Some(a) =>  ((0 until n).map{ i => f"${space}shift_reg[${i}] <= ${a.asUInt.litValue};"}.mkString("\n"), a.asUInt.litValue.toString, "reset", "   input reset,")
    case None => ("", "_", "1'b0", "")
  }
  val (enable, enable_sig) = if (with_enable) {
    ("enable", "input enable,")
  } else {
    ("1'b1", "")
  }

  override def desiredName: String = f"ShiftReg${gen.getClass.getName.split("\\.").last}_l${n}_w${width}_r${resetName}_e${with_enable}"
  val width = gen.getWidth
  os.makeDir.all(os.pwd / "SRs")
  val space = "      "
  val assigns = (0 until n-1).map{ i => f"${space}shift_reg[${i+1}] <= shift_reg[$i];"}.mkString("\n")
  val path = os.pwd / "SRs" / f"$desiredName.v"
  val annot = if (allow_fpga_shreg) "" else "(* shreg_extract = \"no\" *)\n    "
  os.write.over(path,
    f"""
       |module $desiredName (
       |  input clock,
       |  $reset_sig
       |  $enable_sig
       |  input [${width-1}:0] in,
       |  output [${width-1}:0] out);
       |
       |  ${annot}reg [${width-1}:0] shift_reg [0:${n-1}];
       |
       |  always @(posedge clock)
       |  begin
       |    if ($reset_code_sig) begin
       |$resetString
       |    end else if ($enable) begin
       |      shift_reg[0] <= in;
       |$assigns
       |    end
       |  end
       |
       |  assign out = shift_reg[${n-1}];
       |endmodule
       |
       |""".stripMargin)
  BeethovenBuild.addSource(path)
}

object ShiftReg {
  def apply[T <: Data](t: T,
                       latency: Int,
                       clock: Clock,
                       useMemory: Boolean = false,
                       allow_fpga_shreg: Boolean = true,
                       withWidth: Option[Int] = None)(implicit valName: ValName, p: Parameters): T = {
    if (useMemory) {
      val mem = Memory(2, withWidth.getOrElse(t.getWidth), latency+1, 1, 1, 0, allowFallbackToRegister = false)
      mem.initLow(clock = chisel3.Module.clock)
      val read = mem.getReadPortIdx(0)
      val write = mem.getWritePortIdx(0)
      val read_ptr = Reg(UInt(log2Up(latency+1).W))
      when (chisel3.Module.reset.asBool) {
        read_ptr := 0.U
      }
      when (read_ptr === latency.U) {
        read_ptr := 0.U
      }.otherwise {
        read_ptr := read_ptr + 1.U
      }
      val write_ptr = RegNext(read_ptr)
      mem.addr(read) := read_ptr
      mem.write_enable(read) := false.B
      mem.read_enable(read) := true.B
      mem.chip_select(read) := true.B

      mem.addr(write) := write_ptr
      mem.write_enable(write) := true.B
      mem.read_enable(write) := true.B
      mem.chip_select(write) := true.B
      mem.data_in(write) := t.asUInt

      mem.data_out(read).asTypeOf(t)
    } else {
      val sr = Module(new ShiftReg[T](latency, t.cloneType, allow_fpga_shreg = allow_fpga_shreg))
      sr.suggestName("shiftReg" + valName.name)
      sr.io.in := t
      sr.io.clock := clock.asBool
      sr.io.out
    }
  }
}

object ShiftRegWithReset {
  def apply[T <: Data](t: T,
                       latency: Int,
                       resetVal: T,
                       clock: Clock,
                       reset: Reset): T = {
    if (latency == 0) t
    else {
      val m = Module(new ShiftReg(latency, t, Some(resetVal), false))
      m.io.clock := clock.asBool
      m.io.reset.get := reset.asBool
      m.io.in := t
      m.io.out
    }
  }
}

object ShiftRegEnable {
  def apply[T <: Data](t: T,
                       depth: Int,
                       enable: Bool,
                       clock: Clock): T = {
    if (depth == 0) t
    else {
      val m = Module(new ShiftReg(depth, t, with_enable = true, allow_fpga_shreg = true))
      m.io.clock := clock.asBool
      m.io.enable.get := enable
      m.io.in := t
      m.io.out
    }
  }
}