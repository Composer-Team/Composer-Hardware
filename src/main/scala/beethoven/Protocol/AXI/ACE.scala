package beethoven.Protocol

import freechips.rocketchip.amba.axi4.AXI4BundleParameters
import chisel3._
import beethoven.Protocol.ACE._
import beethoven.Protocol.AXI.AXI4Compat
import freechips.rocketchip.subsystem.MasterPortParams

object ACE {
  val snoopWidth = 4
  val domainWidth = 2
  val barWidth = 2
  val respWidth = 4
}

//noinspection ScalaUnusedSymbol
class ACE(param: MasterPortParams) extends AXI4Compat(param, 16) {
  val arsnoop = Output(UInt(snoopWidth.W))
  val ardomain = Output(UInt(domainWidth.W))
  val arbar = Output(UInt(barWidth.W))

  val awsnoop = Output(UInt(snoopWidth.W))
  val awdomain = Output(UInt(domainWidth.W))
  val awbar = Output(UInt(barWidth.W))
  // This seems to only crop up in certain implementations? AXI5
//  val awunique = Output(Bool())

  override val rresp = Input(UInt(4.W))

  val acvalid = Input(Bool())
  val acready = Output(Bool())
  val acaddr = Input(UInt(addrBits.W))
  val acsnoop = Input(UInt(snoopWidth.W))
  val acprot = Input(UInt(AXI4Compat.protWidth.W))

  val crvalid = Output(Bool())
  val crready = Input(Bool())
  val crresp = Output(UInt(5.W))

  val cdvalid = Output(Bool())
  val cdready = Input(Bool())
  val cddata = Output(UInt(dataBits.W))
  val cdlast = Output(Bool())

  /**
   * RESET REQUIREMENTS:
   * When reset is driven, rack, wack, crvalid and cdvalid must be low
   */
  val rack = Output(Bool())
  val wack = Output(Bool())

  override def initFromMasterLow(): Unit = {
    super.initFromMasterLow()
    Seq(arsnoop, ardomain, arbar, awsnoop, awdomain, awbar, acready, crvalid, crresp, cdvalid, cddata, cdlast,
      rack, wack) foreach (_ := 0.U)
  }
}
