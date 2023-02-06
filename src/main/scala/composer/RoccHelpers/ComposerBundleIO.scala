package composer

import chipsalliance.rocketchip.config.Parameters
import chisel3._

class ComposerBundleIO[T1 <: Bundle, T2 <: Bundle](ccio: ComposerCoreIO, bundleIn: T1, bundleOut: T2)(implicit p: Parameters) extends Module {

  val cio = IO(new ComposerCoreIO)
  val io = IO(new CustomIO(bundleIn.cloneType, bundleOut.cloneType))
  io.req.bits.elements.foreach { case (string, data) => data := 0.U }
  io.req.valid := false.B
  io.resp.ready := false.B
  //  io.resp.bits.elements.foreach { case (string, data) => data := 0.U }
  //  io.busy := false.B
  //  io.req.ready := false.B
  //  io.resp.valid := false.B

  cio.busy := io.req.ready
  cio.req.ready := io.req.ready
  cio.resp.valid := io.resp.valid

  ccio.req.valid := false.B
  ccio.resp.bits.data := 0.U
  ccio.req.bits.rs1 := 0.U


  when(cio.req.fire) {
    var counter1 = 0
    var counter2 = 0
    for(i <- io.req.bits.getElements.indices) {
      val nextElement = io.req.bits.getElements(i)
      val start1 = counter1
      val start2 = counter2
      val end1 = start1 + nextElement.getWidth
      val end2 = start2 + nextElement.getWidth

      val userBools = VecInit(nextElement.asUInt.asBools)

      if (end1 < cio.req.bits.rs1.getWidth) {

        val composerBools = VecInit(cio.req.bits.rs1.asBools)

        for(j <- 0 until nextElement.getWidth){
          userBools(j) := composerBools(start1 + j)
        }
        nextElement := userBools.asUInt

//        nextElement := cio.req.bits.rs1
        counter1 = end1

      } else if (end2 < cio.req.bits.rs2.getWidth) {
        val composerBools = VecInit(cio.req.bits.rs2.asBools)

        for(j <- 0 until nextElement.getWidth){
          userBools(j) := composerBools(start1 + j)
        }
        nextElement := userBools.asUInt

//        nextElement := cio.req.bits.rs2
        counter2 = end2
      }
    }
  }


}
