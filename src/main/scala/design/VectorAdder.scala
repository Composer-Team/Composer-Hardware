package design
import chipsalliance.rocketchip.config._
import composer._
import chisel3._
import chisel3.util._

case class VectorConfig (dWidth: Int, // what is the datatype width?
                        )

class VectorAdder(composerCoreParams: ComposerCoreParams)(implicit p: Parameters) extends ComposerCore(composerCoreParams) {
  val s_idle :: s_load :: s_add :: s_store :: s_finish :: Nil = Enum(5)
  val vConfig = p(VectorAdderKey)
  require(isPow2(vConfig.dWidth), "The vector data width must be a power of two!")

  val state = RegInit(s_idle)
  val toAdd = Reg(UInt(vConfig.dWidth.W))
  val vDivs = readChannels(0).data.bits.getWidth / vConfig.dWidth
  val vLen = Reg(UInt(io.req.bits.rs1.getWidth.W))
  val finish = Reg(Bool())
  val wfinish = RegInit(false.B)
  val rfinish = RegInit(false.B)

  // high only one clock
  when(rfinish === true.B) {
    rfinish := false.B
  }

  when(wfinish === true.B) {
    wfinish := false.B
  }

  io.req.ready := state === s_idle
  io.busy := state =/= s_idle
  io.resp.valid := state === s_finish
  io.resp.bits.data := 0.U
  val rdreg = Reg(UInt(io.resp.bits.rd.getWidth.W))
  io.resp.bits.rd := rdreg

  val dArray = Seq.fill(vDivs)(Reg(UInt(vConfig.dWidth.W)))

  // user reverse to maintain order
  writeChannels(0).data.bits := Cat(dArray.reverse)
  writeChannels(0).finished := wfinish
  writeChannels(0).data.valid := false.B
  readChannels(0).stop := rfinish
  readChannels(0).data.ready := false.B

  when (state === s_idle) {
    // don't start if it's a 0-lengthed segment
    when (io.req.fire && io.req.bits.rs1 =/= 0.U) {
      // start address has already been loaded into reader, just need to start reading
      state := s_load
      toAdd := io.req.bits.rs2(vConfig.dWidth-1,0)
      vLen := io.req.bits.rs1
      wfinish := false.B
      rfinish := false.B
      rdreg := io.req.bits.inst.rd
    }
  }.elsewhen(state === s_load) {
    readChannels(0).data.ready := true.B
    when(readChannels(0).data.fire) {
      // break the input into data-sized segments defined by vConfig.dwidth
      dArray.zipWithIndex.foreach{ case (reg: UInt, idx: Int) =>
        reg := readChannels(0).data.bits((idx+1)*vConfig.dWidth-1, vConfig.dWidth*idx)
      }
      state := s_add
      finish := readChannels(0).finished
      vLen := vLen - 1.U
      when (vLen === 1.U) {
        rfinish := true.B
      }
    }
  }.elsewhen(state === s_add) {
    dArray.foreach{a: UInt => a := a + toAdd}
    state := s_store
  }.elsewhen(state === s_store) {
    writeChannels(0).data.valid := true.B
    when(writeChannels(0).data.fire) {
      when(finish) {
        state := s_idle
        wfinish := true.B
      }.otherwise {
        state := s_load
      }
    }
  }
}
