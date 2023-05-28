package composer.common
import chipsalliance.rocketchip.config._


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



  class ArbIOQ extends Bundle() {
    val data = gen.cloneType
    val chosen = UInt(log2Up(nIn).W)
  }


  def condense(grp: Seq[DecoupledIO[ArbIOQ]]): DecoupledIO[ArbIOQ] = {
    val arb = Module(new RRArbiter[ArbIOQ](new ArbIOQ, grp.length))
    arb.io.in zip grp foreach { case (a_io, g_io) => a_io <> g_io }
    arb.io.out
  }

  def recursivelyGroup(group: Seq[DecoupledIO[ArbIOQ]]): DecoupledIO[ArbIOQ] = {
    if (group.length <= width)
      condense(group)
    else
      recursivelyGroup(group.grouped(width).map(condense).toSeq)
  }
  if (nIn == 1) {
    io.in(0) <> io.out
    io.chosen := 0.U
  } else {
    val in_conv = io.in.zipWithIndex.map { case (dat, idx) =>
      dat.map{ d => val wire = Wire(new ArbIOQ); wire.chosen := idx.U; wire.data := d; wire }
    }
    val finalIO = recursivelyGroup(in_conv)
    io.out.valid := finalIO.valid
    require(finalIO.bits.chosen.getWidth == io.chosen.getWidth, s"Require same widths. Found ${finalIO.bits.chosen.getWidth} and ${io.chosen.getWidth}")
    io.chosen := finalIO.bits.chosen
    io.out.bits := finalIO.bits.data
    finalIO.ready := io.out.ready
  }
}
