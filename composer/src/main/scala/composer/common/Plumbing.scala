package composer.common

import composer.DataChannelIO
import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import composer.common.Util._
import freechips.rocketchip.util.DecoupledHelper

import scala.language.postfixOps

///**
// * A Queue that carries stop and finished, ensuring everything is nice
// */
//class SFQueue(n: Int, numBytes: Int) extends Module {
//  def this(sfp: SFQueueParams) = {
//    this(sfp.n, sfp.numBytes)
//  }
//
//  val io = IO(new Bundle {
//    val in = Flipped(new DataChannelIO(numBytes))
//    val out = new DataChannelIO(numBytes)
//  })
//
//  val stopCache = RegInit(false.B)
//  val finishedCache = RegInit(false.B)
//  val q = Module(new Queue(UInt(), n))
//
//  when((stopCache || finishedCache) && (q.io.count === 0.U)) { // TODO: Is this correct?
//    stopCache := false.B
//    finishedCache := false.B
//  }
//  when(io.out.stop) {
//    stopCache := true.B
//  }
//  when(io.in.finished) {
//    finishedCache := true.B
//  }
//  q.io.deq <> io.out.data
//  q.io.enq <> io.in.data
//  when(stopCache || io.out.stop) {
//    q.io.deq.ready := true.B
//    q.io.enq.valid := false.B
//    io.out.data.valid := false.B
//    io.in.data.ready := true.B // Taking in values and throwing them out, stop is asserted
//  }
//  when(finishedCache || io.in.finished) {
//    io.in.data.ready := false.B
//    q.io.enq.valid := false.B
//    io.out.data.valid := q.io.deq.valid // || q.io.count === 0.U
//    // allowing queue to drain out
//    //// garbage on line, but still claim valid b/c are asserting finished
//  }
//
//  //c.busy := r.module.io.busy || (q.io.count =/= 0.U)
//  io.in.stop := (io.out.stop || stopCache)
//  io.out.finished := (io.in.finished || finishedCache) && (q.io.count === 0.U)
//}
//
//case class SFQueueParams(n: Int, numBytes: Int)
//
//object SFQueue {
//  def apply(n: Int, numBytes: Int)(in: DataChannelIO): DataChannelIO = {
//    val q = Module(new SFQueue(n, numBytes))
//    q.io.in <> in
//    q.io.out
//  }
//  //def apply(in: DataChannelIO)(implicit sfqp: SFQueueParams): DataChannelIO = {
//  //  val q = Module(new SFQueue(sfqp))
//  //  q.io.in <> in
//  //  q.io.out
//  //}
//}

//class Unordered2Combiner(numBytes: Int)(implicit p: Parameters) extends Module {
//  val io = IO(new Bundle {
//    val in1 = Flipped(new DataChannelIO(numBytes))
//    val in2 = Flipped(new DataChannelIO(numBytes))
//    val out = new DataChannelIO(numBytes)
//  })
//  val ins = Seq(io.in1, io.in2)
//
//  val arbiter = Module(new RRArbiter(UInt(), 2))
//  arbiter.io.in <> ins.map(_.data)
//  arbiter.io.in zip ins foreach { case (a, i) =>
//    a.valid := i.data.valid && !i.finished
//  }
//  io.out.data <> arbiter.io.out
//
//  ins.foreach {
//    _.stop := io.out.stop
//  }
//  io.out.finished := ins.map(_.finished).all
//}

//class SVD2Mux(numBytes: Int)(implicit p: Parameters) extends Module {
//  val io = IO(new Bundle {
//    val in = Flipped(new DataChannelIO(numBytes))
//    val sel = Input(UInt(1.W))
//    val out1 = new DataChannelIO(numBytes)
//    val out2 = new DataChannelIO(numBytes)
//  })
//
//  val outs = Seq(io.out1, io.out2)
//
//  //defaults
//  io.in.data.ready := false.B
//  io.in.stop := false.B
//
//  for ((out, i) <- outs zipWithIndex) {
//    out.data.bits := io.in.data.bits
//    //out.finished := io.in.finished //TODO: Is this the way we want to do it?
//    when(io.sel === i.U) {
//      out.finished <> io.in.finished
//      out.stop <> io.in.stop
//      out.data.ready <> io.in.data.ready
//      out.data.valid <> io.in.data.valid
//    }.otherwise {
//      out.finished := false.B
//      out.data.valid := false.B
//    }
//  }
//}
//
//class Doubler(numBytes: Int)(implicit p: Parameters) extends Module {
//  val io = IO(new Bundle {
//    val in = Flipped(new DataChannelIO(numBytes))
//    val out1 = new DataChannelIO(numBytes)
//    val out2 = new DataChannelIO(numBytes)
//  })
//
//  val outs = Seq(io.out1, io.out2)
//  val helper = DecoupledHelper(io.in.data.valid, io.out1.data.ready, io.out2.data.ready)
//
//  val anyStops = outs.map(_.stop).any
//  io.in.stop := anyStops
//  io.in.data.ready := helper.fire(io.in.data.valid)
//  outs.foreach { o =>
//    o.data.bits := io.in.data.bits
//    o.data.valid := helper.fire(o.data.ready)
//    o.finished := (io.in.finished || anyStops) && !o.stop
//  }
//}
//
//class Drain(numBytes: Int) extends Module {
//  val io = IO(new Bundle {
//    val in = Flipped(new DataChannelIO(numBytes))
//  })
//  io.in.stop := false.B
//  io.in.data.ready := true.B
//}
//
//object Drain {
//  def apply(numBytes: Int): DataChannelIO = {
//    val d = Module(new Drain(numBytes))
//    d.io.in
//  }
//}


