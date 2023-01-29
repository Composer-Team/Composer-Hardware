package fpnewWrapper.fpnew

import chisel3._

class FPUNew(
    val ftype: FPNewFType.FPNewFType,
    val lanes: Int,
    val stages: Int,
    val tagWidth: Int = 0
) extends Module {
  val fLen = FPNewFType.toWidth(ftype) * lanes
  val io = IO(new FPIO(fLen, tagWidth))

  val blackbox = Module(
    new FPNewBlackbox(
      ftype,
      lanes,
      stages,
      tagWidth,
    )
  )

  // clock & reset
  blackbox.io.clk_i := clock
  blackbox.io.rst_ni := ~reset.asBool

  // request
  blackbox.io.operands_i := io.req.bits.operands.asUInt
  blackbox.io.rnd_mode_i := io.req.bits.roundingMode.asUInt
  blackbox.io.op_i := io.req.bits.op.asUInt
  blackbox.io.op_mod_i := io.req.bits.opModifier
  blackbox.io.src_fmt_i := io.req.bits.srcFormat.asUInt
  blackbox.io.dst_fmt_i := io.req.bits.dstFormat.asUInt
  blackbox.io.int_fmt_i := io.req.bits.intFormat.asUInt
  blackbox.io.vectorial_op_i := 1.B
  blackbox.io.tag_i := io.req.bits.tag
  blackbox.io.in_valid_i := io.req.valid
  io.req.ready := blackbox.io.in_ready_o

  // response
  io.resp.bits.result := blackbox.io.result_o
  io.resp.bits.status := blackbox.io.status_o.asTypeOf(io.resp.bits.status)
  io.resp.valid := blackbox.io.out_valid_o
  io.resp.bits.tag := blackbox.io.tag_o

  blackbox.io.out_ready_i := io.resp.ready

  // flush & flush
  blackbox.io.flush_i := io.flush
  io.busy := blackbox.io.busy_o
}


class FPNewBlackbox(ftype: FPNewFType.FPNewFType,
                    lanes: Int,
                    stages: Int,
                    tagWidth: Int,
                   ) extends BlackBox(
  Map(
    "FLEN" -> FPNewFType.toWidth(ftype) * lanes,
    "ENABLE_FP32" -> (if (ftype == FPNewFType.FullPrecision) 1 else 0),
    "ENABLE_FP64" -> (if (ftype == FPNewFType.DoublePrecision) 1 else 0),
    "ENABLE_FP16" -> (if (ftype == FPNewFType.HalfPrecision) 1 else 0),
    "ENABLE_FP16ALT" -> (if (ftype == FPNewFType.B16) 1 else 0),
    "TAG_WIDTH" -> tagWidth,
    "ENABLE_VECTORS" -> 1,
    "ENABLE_NAN_BOX" -> 1,
    "ENABLE_FP8" -> 0,
    "ENABLE_INT8" -> 0,
    "ENABLE_INT16" -> 0,
    "ENABLE_INT32" -> 0,
    "ENABLE_INT64" -> 0,
    "PIPELINE_STAGES" -> stages)
) {
  val fLen = FPNewFType.toWidth(ftype) * lanes
  val io = IO(new FPNBlackboxIO(fLen, tagWidth)).suggestName("io")
}
