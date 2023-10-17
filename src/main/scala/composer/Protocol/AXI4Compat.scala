package composer.Protocol

import chisel3._
import chisel3.util.log2Up
import composer.Protocol.AXI4Compat._
import freechips.rocketchip.amba.axi4.{AXI4Bundle, AXI4BundleParameters}
import freechips.rocketchip.subsystem.MasterPortParams

class AXI4Compat(param: MasterPortParams) extends Bundle {
  val addrBits = log2Up(param.size)
  val dataBits = param.beatBytes * 8
  val awid = Output(UInt(param.idBits.W))
  val awaddr = Output(UInt(addrBits.W))
  val awlen = Output(UInt(lenWidth.W))
  val awsize = Output(UInt(sizeWidth.W))
  val awburst = Output(UInt(burstWidth.W))
  val awlock = Output(Bool())
  val awcache = Output(UInt(cacheWidth.W))
  val awprot = Output(UInt(protWidth.W))
  val awregion = Output(UInt(regionWidth.W))
  val awqos = Output(UInt(qosWidth.W))
  //  val awuser = Output(UInt(param.))
  val awvalid = Output(Bool())
  val awready = Input(Bool())

  val wdata = Output(UInt(dataBits.W))
  val wstrb = Output(UInt((dataBits / 8).W))
  val wlast = Output(Bool())
  val wvalid = Output(Bool())
  val wready = Input(Bool())

  val bid = Input(UInt(param.idBits.W))
  val bresp = Input(UInt(respWidth.W))
  val bvalid = Input(Bool())
  val bready = Output(Bool())

  val arid = Output(UInt(param.idBits.W))
  val araddr = Output(UInt(addrBits.W))
  val arlen = Output(UInt(lenWidth.W))
  val arsize = Output(UInt(sizeWidth.W))
  val arburst = Output(UInt(burstWidth.W))
  val arlock = Output(Bool())
  val arcache = Output(UInt(cacheWidth.W))
  val arprot = Output(UInt(protWidth.W))
  val arregion = Output(UInt(regionWidth.W))
  val arqos = Output(UInt(qosWidth.W))
  //  val awuser = Output(UInt(param.))
  val arvalid = Output(Bool())
  val arready = Input(Bool())

  val rid = Input(UInt(param.idBits.W))
  val rdata = Input(UInt(dataBits.W))
  val rresp = Input(UInt(respWidth.W))
  val rlast = Input(Bool())
  val rvalid = Input(Bool())
  val rready = Output(Bool())

  def initLow(): Unit = {
    Seq(awid, awaddr, awlen, awsize, awburst, awlock, awcache, awprot, awregion, awqos, awvalid, wdata, wstrb,
      wlast, wvalid, bready, arid, araddr, arlen, arsize, arburst, arlock, arcache, arprot, arregion, arqos,
      arvalid, rready) foreach (_ := 0.U)
  }
}

object AXI4Compat {
  val cacheWidth = 4
  val protWidth = 3
  val qosWidth = 4
  val regionWidth = 4
  val burstWidth = 2
  val sizeWidth = 3
  val lenWidth = 8
  val respWidth = 2

  def connectCompatMaster(compat: AXI4Compat, axi4: AXI4Bundle, makeCoherent: Boolean): Unit = {
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
    if (makeCoherent) {
      compat.arcache := 1.U
    } else {
      compat.arcache := 0.U
    }
    //    compat.arcache := axi4.ar.bits.cache
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
    if (makeCoherent) {
      compat.awcache := 1.U
    } else {
      compat.awcache := 0.U
    }

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

  def apply(bundleParameters: AXI4BundleParameters): AXI4Compat = {
    println(bundleParameters.addrBits)
    new AXI4Compat(
      MasterPortParams(0, 1L << bundleParameters.addrBits, bundleParameters.dataBits / 8,
        bundleParameters.idBits))
  }

}
