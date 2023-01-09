package composer

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule

class AXILWidget(implicit p: Parameters) extends Widget()(p) {
  override val crFile = LazyModule(new MCRFile(15)(p))
  crFile.node := node
  override lazy val module = new AXILWidgetModule(this)
}

class AXILWidgetModule(outer: AXILWidget) extends WidgetModule(outer) {

  val io = IO(new Bundle {
    val cmds = Decoupled(UInt(nastiXDataBits.W))
    val resp = Flipped(Decoupled(UInt(nastiXDataBits.W)))
    val mem_in  = Decoupled(UInt(nastiXDataBits.W))
    val mem_out = Flipped(Decoupled(UInt(nastiXDataBits.W)))
    val read_addrs = Decoupled(UInt(nastiXDataBits.W))
  })

  val roccCmdFifo = Module(new Queue(UInt(nastiXDataBits.W), 16))
  val roccRespFifo = Module(new Queue(UInt(nastiXDataBits.W), 16))
  val writeFifo = Module(new Queue(UInt(nastiXDataBits.W), 32))
  val readFifo = Module(new Queue(UInt(nastiXDataBits.W), 32))
  val readReqs = Module(new Queue(UInt(nastiXDataBits.W), 32))

  genROReg(roccRespFifo.io.deq.bits, "resp_bits")
  genROReg(roccRespFifo.io.deq.valid, "resp_valid")
  Pulsify(genWORegInit(roccRespFifo.io.deq.ready, "resp_ready", false.B), pulseLength = 1)

  genWOReg(roccCmdFifo.io.enq.bits, "cmd_bits")
  Pulsify(genWORegInit(roccCmdFifo.io.enq.valid, "cmd_valid", false.B), pulseLength = 1)
  genROReg(roccCmdFifo.io.enq.ready, "cmd_ready")

  genWOReg(writeFifo.io.enq.bits, "write_bits")
  Pulsify(genWORegInit(writeFifo.io.enq.valid, "write_valid", false.B), pulseLength = 1)
  genROReg(writeFifo.io.enq.ready, "write_ready")

  genWOReg(readReqs.io.enq.bits, "read_addr_bits")
  Pulsify(genWORegInit(readReqs.io.enq.valid, "read_addr_valid", false.B), pulseLength = 1)
  genROReg(readReqs.io.enq.ready, "read_addr_ready")

  genROReg(readFifo.io.deq.bits, "read_bits")
  genROReg(readFifo.io.deq.valid, "read_valid")
  Pulsify(genWORegInit(readFifo.io.deq.ready, "read_ready", false.B), pulseLength = 1)

  genCRFile()

  io.cmds <> roccCmdFifo.io.deq
  roccRespFifo.io.enq <> io.resp
  io.mem_in <> writeFifo.io.deq
  readFifo.io.enq <> io.mem_out
  io.read_addrs <> readReqs.io.deq
}

