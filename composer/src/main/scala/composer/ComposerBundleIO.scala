package composer

import chipsalliance.rocketchip.config.Parameters
import chisel3._

class ComposerBundleIO[T1 <: Bundle, T2 <: Bundle](ccio: ComposerCoreIO, bundleIn: T1, bundleOut: T2)(implicit p: Parameters) extends Module {

  //val ccio = IO(new ComposerCoreIO())
  val io = IO(Output(new CustomIO(bundleIn.cloneType, bundleOut.cloneType)))
  io.req.bits.elements.foreach { case (string, data) => data := 0.U }
  io.resp.bits.elements.foreach { case (string, data) => data := 0.U }
  io.busy := false.B
  io.req.valid := false.B
  io.req.ready := false.B
  io.resp.ready := false.B
  io.resp.valid := false.B


//  when(ccio.req.fire) {
//    var counter1 = 0
//    var counter2 = 0
//    for(i <- io.getElements.indices) {
//      val start1 = counter1
//      val start2 = counter2
//      val end1 = counter1 + io.getElements(i).getWidth
//      val end2 = counter2 + io.getElements(i).getWidth
//      if (end1 < ccio.req.bits.rs1.getWidth - start1) {
//        io.getElements(i) := ccio.req.bits.rs1(start1, end1)
//        counter1 = end1
//      } else if (end2 < ccio.req.bits.rs2.getWidth - start2) {
//        io.getElements(i) := ccio.req.bits.rs1(start2, end2)
//        counter2 = end2
//      }
//    }
//  }


}
