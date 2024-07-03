package beethoven.Protocol.AXI

import beethoven.Protocol.AXI.AXI4Compat._
import chisel3._
import chisel3.util.log2Up
import freechips.rocketchip.amba.axi4.{AXI4Bundle, AXI4BundleParameters, AXI4Parameters}
import freechips.rocketchip.subsystem.MasterPortParams

class AXI4Compat(val param: MasterPortParams, userBits: Int = 0) extends Bundle {
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
  val awuser = Output(UInt(userBits.W))
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
  val aruser = Output(UInt(userBits.W))
  val arvalid = Output(Bool())
  val arready = Input(Bool())

  val rid = Input(UInt(param.idBits.W))
  val rdata = Input(UInt(dataBits.W))
  val rresp = Input(UInt(respWidth.W))
  val rlast = Input(Bool())
  val rvalid = Input(Bool())
  val rready = Output(Bool())

  def initFromMasterLow(): Unit = {
    Seq(awid, awaddr, awlen, awsize, awburst, awlock, awcache, awprot, awregion, awqos, awvalid, wdata, wstrb,
      wlast, wvalid, bready, arid, araddr, arlen, arsize, arburst, arlock, arcache, arprot, arregion, arqos,
      arvalid, rready, aruser, awuser) foreach(_ := 0.U)
  }

  def initFromSlaveLow(): Unit = {
    Seq(awready, arready, rvalid, rdata, rresp, rid, rlast, wready, bvalid, bid, bresp) foreach (_ := 0.U)
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

  val prot_level = AXI4Parameters.PROT_INSECURE
  val cache_level = 0xF.U

  def connectCompatMaster(s: AXI4Compat, m: AXI4Bundle): Unit = {
    m.r.bits.id := s.rid
    m.r.bits.data := s.rdata
    m.r.bits.resp := s.rresp
    m.r.bits.last := s.rlast
    m.r.valid := s.rvalid
    s.rready := m.r.ready

    s.arid := m.ar.bits.id
    s.arqos := m.ar.bits.qos
    s.arlen := m.ar.bits.len
    s.arlock := m.ar.bits.lock
    s.arprot := prot_level // m.ar.bits.prot
    s.araddr := m.ar.bits.addr
    s.arburst := m.ar.bits.burst
    s.aruser := m.ar.bits.user.asUInt
    s.arcache := cache_level // 0xF.U //axi4.ar.bits.cache
    s.arregion := 0.U
    s.arsize := m.ar.bits.size
    m.ar.ready := s.arready
    s.arvalid := m.ar.valid

    m.b.bits.id := s.bid
    m.b.bits.resp := s.bresp
    s.bready := m.b.ready
    m.b.valid := s.bvalid

    s.awid := m.aw.bits.id
    s.awqos := m.aw.bits.qos
    s.awlen := m.aw.bits.len
    s.awlock := 0.U
    s.awprot := prot_level
    s.awaddr := m.aw.bits.addr
    s.awburst := m.aw.bits.burst
    s.awcache := cache_level // 0xF.U // axi4.aw.bits.cache
    s.awsize := m.aw.bits.size
    s.awuser := 0.U
    s.awregion := 0.U
    m.aw.ready := s.awready
    s.awvalid := m.aw.valid

    s.wdata := m.w.bits.data
    s.wstrb := m.w.bits.strb
    s.wlast := m.w.bits.last
    s.wvalid := m.w.valid
    m.w.ready := s.wready
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
    axi4.ar.bits.lock := 0.U //compat.arlock
    axi4.ar.bits.prot := prot_level // compat.arprot
    axi4.ar.bits.addr := compat.araddr
    axi4.ar.bits.burst := compat.arburst
    axi4.ar.bits.cache := cache_level // compat.arcache
    axi4.ar.bits.size := compat.arsize
//    axi4.ar.bits.user. := compat.aruser
    compat.arready := axi4.ar.ready
    axi4.ar.valid := compat.arvalid

    axi4.aw.bits.id := compat.awid
    axi4.aw.bits.qos := compat.awqos
    axi4.aw.bits.len := compat.awlen
    axi4.aw.bits.lock := 0.U //compat.awlock
    axi4.aw.bits.prot := prot_level // compat.awprot
    axi4.aw.bits.addr := compat.awaddr
    axi4.aw.bits.burst := compat.awburst
    axi4.aw.bits.cache := cache_level // compat.awcache
    axi4.aw.bits.size := compat.awsize
//    axi4.aw.bits.user := compat.awuser
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
//    println(bundleParameters.addrBits)
    new AXI4Compat(
      MasterPortParams(0, 1L << bundleParameters.addrBits, bundleParameters.dataBits / 8,
      bundleParameters.idBits))
  }

}
