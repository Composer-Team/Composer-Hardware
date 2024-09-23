package beethoven.Protocol.tilelink

import beethoven.common.CLog2Up
import chisel3.util._
import chisel3._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy.LazyModuleImp

class TLToAXI4SRWImpl(outer: TLToAXI4SRW) extends LazyModuleImp(outer) {
  val tlManagers = outer.node
  tlManagers.in.foreach { in => require(log2Ceil(in._2.maxLgSize + 1) <= 4) }
  // ensure that all managers and clients have the same width
  val out_width = outer.node.out(0)._2.bundle.dataBits
  tlManagers.in foreach { in => in._2.bundle.dataBits == out_width }

  // require that the masters are excelusive
  val supportIntersection = outer.node.in.map(_._2.master)
  supportIntersection.length match {
    case 1 => ;
    case 2 =>
      val a = supportIntersection(0)
      val b = supportIntersection(1)
//      require(a.allSupportPutFull.max > 0 ^ b.allSupportPutFull.max > 0, "Incoming connections in TLSRW are not exclusive in write")
//      require(a.allSupportGet.max > 0 ^ b.allSupportGet.max > 0, s"Incoming connections in TLSRW are not exclusive in read: ${a.allSupportGet} ${b.allSupportGet}")
    case _ => throw new Exception("An illegal number of incoming connections!")
  }

  val write_in_seq = tlManagers.in.find(_._2.master.allSupportPutFull.max > 0) match {
    case None =>
      // tie off AXI write ports
      val aw = outer.node.out(0)._1.aw
      val w = outer.node.out(0)._1.w
      val b = outer.node.out(0)._1.b
      aw.valid := false.B
      w.valid := false.B
      b.ready := false.B
      aw.bits := DontCare
      w.bits := DontCare
      Seq()
    case Some((in, edgeIn)) => Seq((in, edgeIn, true))
  }
  val read_in_seq = tlManagers.in.find(_._2.master.allSupportGet.max > 0) match {
    case None =>
      // tie off AXI read ports
      val ar = outer.node.out(0)._1.ar
      val r = outer.node.out(0)._1.r
      ar.valid := false.B
      r.ready := false.B
      ar.bits := DontCare
      Seq()
    case Some((in, edgeIn)) => Seq((in, edgeIn, false))
  }
  val (out, _) = outer.node.out(0)
  (write_in_seq ++ read_in_seq) foreach {
    case (in, edgeIn, isWrite) =>
      // Fan out the ARW channel to AR and AW
      val axiLen = (UIntToOH(in.a.bits.size) >> CLog2Up(out_width / 8)).asUInt - 1.U
      if (isWrite) {
        // use queues to obey the irrevocability of AXI4 whereas TL components aren't
        // irrevocable
        val aw_queue = Module(new Queue(new AXI4BundleAW(out.params), entries = 4))
        val w_queue = Module(new Queue(new AXI4BundleW(out.params), entries = 4))
        val wBeatCounter = RegInit(0.U(axiLen.getWidth.W))
        val doingLongWriteTx = RegInit(false.B)

        aw_queue.io.enq.bits.id := in.a.bits.source
        aw_queue.io.enq.bits.addr := edgeIn.address(in.a.bits)
        aw_queue.io.enq.bits.burst := AXI4Parameters.BURST_INCR
        aw_queue.io.enq.bits.cache := 0.U
        aw_queue.io.enq.bits.len := axiLen
        aw_queue.io.enq.bits.size := CLog2Up(out_width / 8).U
        aw_queue.io.enq.bits.lock := 0.U // AXI3 backwards compat
        aw_queue.io.enq.bits.prot := AXI4Parameters.PROT_INSECURE
        aw_queue.io.enq.bits.qos := 0.U
        aw_queue.io.enq.bits.user := in.a.bits.user
        // Currently only support PutFull, so all the data will be
        // present in the same beat
        w_queue.io.enq.bits.last := (axiLen === 0.U) || (doingLongWriteTx && wBeatCounter === axiLen)
        when(doingLongWriteTx && w_queue.io.enq.fire) {
          wBeatCounter := wBeatCounter + 1.U
          when(wBeatCounter === axiLen) {
            wBeatCounter := 0.U
            doingLongWriteTx := false.B
          }
        }
        w_queue.io.enq.bits.user := in.a.bits.user
        w_queue.io.enq.bits.data := in.a.bits.data
        w_queue.io.enq.bits.strb := in.a.bits.mask
        aw_queue.io.enq.valid := in.a.valid && w_queue.io.enq.ready && !doingLongWriteTx
        w_queue.io.enq.valid := in.a.valid && (aw_queue.io.enq.ready || doingLongWriteTx)
        in.a.ready := (aw_queue.io.enq.ready || doingLongWriteTx) && w_queue.io.enq.ready
        when(aw_queue.io.enq.fire && axiLen > 0.U) {
          doingLongWriteTx := true.B
          wBeatCounter := wBeatCounter + 1.U
        }

        out.aw <> aw_queue.io.deq
        out.w <> w_queue.io.deq

        in.d.bits := edgeIn.AccessAck(
          toSource = out.b.bits.id,
          lgSize = 0.U,
          denied = out.b.bits.resp =/= AXI4Parameters.RESP_OKAY)
        in.d.valid := out.b.valid
        out.b.ready := in.d.ready
      } else {
        out.ar.bits.id := in.a.bits.source
        out.ar.bits.user := in.a.bits.user
        out.ar.bits.size := CLog2Up(out_width / 8).U
        out.ar.bits.len := axiLen
        out.ar.bits.prot := AXI4Parameters.PROT_INSECURE
        out.ar.bits.addr := edgeIn.address(in.a.bits)
        out.ar.valid := in.a.valid
        in.a.ready := out.ar.ready
        out.ar.bits.burst := AXI4Parameters.BURST_INCR
        out.ar.bits.cache := 0.U

        out.r.ready := in.d.ready
        in.d.bits := edgeIn.AccessAck(
          toSource = out.r.bits.id,
          lgSize = CLog2Up(out_width / 8).U,
          data = out.r.bits.data,
          denied = out.r.bits.resp === AXI4Parameters.RESP_DECERR,
          corrupt = out.r.bits.resp =/= AXI4Parameters.RESP_OKAY)
        in.d.valid := out.r.valid

      }
  }
}
