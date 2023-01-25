//package design.DT
//
//import chisel3._
//import chisel3.util._
//import fpnewWrapper.fpnew.{FPNBlackboxIO, FPNewBlackbox, FPNewFType}
//
//class FPWrapperWrapper(ftype: FPNewFType.FPNewFType,
//                       lanes: Int,
//                       stages: Int,
//                       tagWidth: Int,
//                       useFloat: Boolean
//                      ) extends Module {
//  require(lanes == 1, "This wrapper only supports 1 lane")
//  require(ftype == FPNewFType.FullPrecision)
//  val io = IO(new FPUNew(32, tagWidth))
//  if (useFloat) {
//    val floatModule = Module(new FPNewBlackbox(ftype, lanes, stages, tagWidth))
//    floatModule.io <> io
//  } else {
//    // fake it with signed integer arithmetic instead
//    val res = Wire(UInt(32.W))
//    val resDec = Decoupled(res)
//    val queue = Queue(resDec, stages, true, true)
//
//    io.result_o := queue.bits
//    queue.ready := io.out_ready_i
//    io.out_valid_o := queue.valid
//
//    when (io.)
//
//
//  }
//}
