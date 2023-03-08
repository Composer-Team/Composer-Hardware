package composer.common
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.CXbarMaxDegree

class MultiLevelArbiter[T <: Data](gen: T, nIn: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle() {
    val in = Vec(nIn, Flipped(Decoupled(gen)))
    val out = Decoupled(gen)
    val chosen = Output(UInt(log2Up(nIn).W))
  })

  val width = p(CXbarMaxDegree)
  require(isPow2(width), "Using this arbiter implementation requires the max XBar degree to be a power of 2")

  class ArbIOQ(cWidth: Int) extends Bundle {
    val dat = gen.cloneType
    val chosen = UInt(cWidth.W)
  }

  def recursivelyGroup(group: Seq[DecoupledIO[T]]): DecoupledIO[ArbIOQ] = {
    if (group.length <= width) {
      val arb = Module(new RRArbiter[T](gen, group.length))
      arb.io.in zip group foreach {case (a_io, g_io) => a_io <> g_io }
      val i = Wire(Decoupled(new ArbIOQ(arb.io.chosen.getWidth)))
      i.valid := arb.io.out.valid
      arb.io.out.ready := i.ready
      i.bits.chosen := arb.io.chosen
      i.bits.dat := arb.io.out.bits
      val q = Queue(i, 1)
      q
    } else {
      // pad group to be divisble by group length
      val group_pad = group ++ Seq.fill(group.length % width) {
        val a = Wire(Decoupled(gen.cloneType))
        a.bits := DontCare
        a.valid := false.B
        a
      }
      val grouped = group_pad.grouped(width).map(recursivelyGroup).toSeq
      val rr = Module(new RRArbiter[ArbIOQ](new ArbIOQ(grouped(0).bits.chosen.getWidth), grouped.length))
      rr.io.in zip grouped foreach { case (rrio, gio) => rrio <> gio }
      val actualChosen = Cat(rr.io.chosen, rr.io.out.bits.chosen)
      val i = Wire(Decoupled(new ArbIOQ(rr.io.out.bits.chosen.getWidth + rr.io.chosen.getWidth)))
      i.bits.chosen := actualChosen
      i.bits.dat := rr.io.out.bits.dat
      i.valid := rr.io.out.valid
      rr.io.out.ready := i.ready
      i
    }
  }
  if (nIn == 1) {
    io.in(0) <> io.out
    io.chosen := 0.U
  } else {
    val finalIO = recursivelyGroup(io.in)
    io.out.valid := finalIO.valid
    require(finalIO.bits.chosen.getWidth == io.chosen.getWidth, s"Require same widths. Found ${finalIO.bits.chosen.getWidth} and ${io.chosen.getWidth}")
    io.chosen := finalIO.bits.chosen
    io.out.bits := finalIO.bits.dat
    finalIO.ready := io.out.ready
  }
}
