package composer.RoccHelpers

import chisel3._
import freechips.rocketchip.amba.axi4.{AXI4Bundle, AXI4BundleParameters}

class AXI4Compat(param: AXI4BundleParameters) extends Bundle {
  val awid = Output(UInt(param.idBits.W))
  val awaddr = Output(UInt(param.addrBits.W))
  val awlen = Output(UInt(param.lenBits.W))
  val awsize = Output(UInt(param.sizeBits.W))
  val awburst = Output(UInt(param.burstBits.W))
  val awlock = Output(Bool())
  val awcache = Output(UInt(param.cacheBits.W))
  val awprot = Output(UInt(param.protBits.W))
  val awregion = Output(UInt(4.W))
  val awqos = Output(UInt(4.W))
  //  val awuser = Output(UInt(param.))
  val awvalid = Output(Bool())
  val awready = Input(Bool())

  val wdata = Output(UInt(param.dataBits.W))
  val wstrb = Output(UInt((param.dataBits / 8).W))
  val wlast = Output(Bool())
  val wvalid = Output(Bool())
  val wready = Input(Bool())

  val bid = Input(UInt(param.idBits.W))
  val bresp = Input(UInt(param.respBits.W))
  val bvalid = Input(Bool())
  val bready = Output(Bool())

  val arid = Output(UInt(param.idBits.W))
  val araddr = Output(UInt(param.addrBits.W))
  val arlen = Output(UInt(param.lenBits.W))
  val arsize = Output(UInt(param.sizeBits.W))
  val arburst = Output(UInt(param.burstBits.W))
  val arlock = Output(Bool())
  val arcache = Output(UInt(param.cacheBits.W))
  val arprot = Output(UInt(param.protBits.W))
  val arregion = Output(UInt(4.W))
  val arqos = Output(UInt(4.W))
  //  val awuser = Output(UInt(param.))
  val arvalid = Output(Bool())
  val arready = Input(Bool())

  val rid = Input(UInt(param.idBits.W))
  val rdata = Input(UInt(param.dataBits.W))
  val rresp = Input(UInt(param.respBits.W))
  val rlast = Input(Bool())
  val rvalid = Input(Bool())
  val rready = Output(Bool())
}

object AXI4Compat {
  def connectCompatMaster(compat: AXI4Compat, axi4: AXI4Bundle): Unit = {
    axi4.r.bits.id := compat.rid
    axi4.r.bits.data := compat.rdata
    axi4.r.bits.resp := compat.rresp
    axi4.r.bits.last := compat.rlast
    axi4.r.valid := compat.rvalid
    compat.rready := axi4.r.ready

    compat.arid := axi4.ar.bits.id
    compat.arqos := axi4.ar.bits.qos
    compat.arlen := axi4.ar.bits.len
    compat.arlock := axi4.ar.bits.lock
    compat.arprot := axi4.ar.bits.prot
    compat.araddr := axi4.ar.bits.addr
    compat.arburst := axi4.ar.bits.burst
    compat.arcache := axi4.ar.bits.cache
    compat.arregion := 0.U
    compat.arsize := axi4.ar.bits.size
    axi4.ar.ready := compat.arready
    compat.arvalid := axi4.ar.valid

    axi4.b.bits.id := compat.bid
    axi4.b.bits.resp := compat.bresp
    compat.bready := axi4.b.ready
    axi4.b.valid := compat.bvalid

    compat.awid := axi4.aw.bits.id
    compat.awqos := axi4.aw.bits.qos
    compat.awlen := axi4.aw.bits.len
    compat.awlock := axi4.aw.bits.lock
    compat.awprot := axi4.aw.bits.prot
    compat.awaddr := axi4.aw.bits.addr
    compat.awburst := axi4.aw.bits.burst
    compat.awcache := axi4.aw.bits.cache
    compat.awsize := axi4.aw.bits.size
    compat.awregion := 0.U
    axi4.aw.ready := compat.awready
    compat.awvalid := axi4.aw.valid

    compat.wdata := axi4.w.bits.data
    compat.wstrb := axi4.w.bits.strb
    compat.wlast := axi4.w.bits.last
    compat.wvalid := axi4.w.valid
    axi4.w.ready := compat.wready
  }

  def connectCompatSlave(compat: AXI4Compat, axi4: AXI4Bundle): Unit = {
    compat.rid := axi4.r.bits.id
    compat.rdata := axi4.r.bits.data
    compat.rresp := axi4.r.bits.resp
    compat.rlast := axi4.r.bits.last
    compat.rvalid := axi4.r.valid
    axi4.r.ready := compat.rready

    axi4.ar.bits.id := compat.arid
    axi4.ar.bits.qos := compat.arqos
    axi4.ar.bits.len := compat.arlen
    axi4.ar.bits.lock := compat.arlock
    axi4.ar.bits.prot := compat.arprot
    axi4.ar.bits.addr := compat.araddr
    axi4.ar.bits.burst := compat.arburst
    axi4.ar.bits.cache := compat.arcache
    axi4.ar.bits.size := compat.arsize
    compat.arready := axi4.ar.ready
    axi4.ar.valid := compat.arvalid

    axi4.aw.bits.id := compat.awid
    axi4.aw.bits.qos := compat.awqos
    axi4.aw.bits.len := compat.awlen
    axi4.aw.bits.lock := compat.awlock
    axi4.aw.bits.prot := compat.awprot
    axi4.aw.bits.addr := compat.awaddr
    axi4.aw.bits.burst := compat.awburst
    axi4.aw.bits.cache := compat.awcache
    axi4.aw.bits.size := compat.awsize
    compat.awready := axi4.aw.ready
    axi4.aw.valid := compat.awvalid

    compat.bid := axi4.b.bits.id
    compat.bresp := axi4.b.bits.resp
    axi4.b.ready := compat.bready
    compat.bvalid := axi4.b.valid

    axi4.w.bits.data := compat.wdata
    axi4.w.bits.strb := compat.wstrb
    axi4.w.bits.last := compat.wlast
    axi4.w.valid := compat.wvalid
    compat.wready := axi4.w.ready
  }

}
