package composer

import chipsalliance.rocketchip.config.Parameters
import chisel3._

class ComposerBundleIO[T1 <: Bundle, T2 <: Bundle](bundleIn: T1, bundleOut: T2, composerCoreParams: ComposerCoreParams)(implicit p: Parameters) extends Module {

  val cio = IO(new ComposerCoreIO)
  val io = IO(new CustomIO(bundleIn.cloneType, bundleOut.cloneType))

  //zero initialize values
  io.req.bits.elements.foreach { case (_, data) => data := 0.U }
  cio.resp.bits.elements.foreach { case (_, data) => data := 0.U }

  //System initialized values
  cio.resp.bits.system_id := composerCoreParams.system_id.U
  cio.resp.bits.core_id := composerCoreParams.core_id.U

  //Only works for direct pass-through
  io.req.valid := cio.req.valid
  io.resp.ready := cio.resp.ready
  cio.busy := io.busy
  cio.req.ready := io.req.ready
  cio.resp.valid := io.resp.valid

//  //Direct pass-through
//  when(io.req.fire) {
//    io.req.bits.getElements(2) := cio.req.bits.inst.rs1
//    io.req.bits.getElements(1) := cio.req.bits.rs1
//    io.req.bits.getElements(0) := cio.req.bits.rs2
//  }

  when(io.resp.fire) {
    cio.resp.bits.data := io.resp.bits.getElements(0)
  }


  when(cio.req.fire) {
    var counter = 0
    for(i <- io.req.bits.getElements.indices) {
      val nextElement = io.req.bits.getElements(i)
      val start = counter
      val end = start + nextElement.getWidth

      val userBools = VecInit(nextElement.asUInt.asBools)

      if (end < cio.req.bits.rs2.getWidth) {
        val composerBools = VecInit(cio.req.bits.rs2.asBools)
        for(j <- 0 until nextElement.getWidth){
          userBools(j) := composerBools(start + j)
        }
        nextElement := userBools.asUInt
        counter = end
      }
    }
  }


}
