package composer.Protocol

import freechips.rocketchip.amba.axi4.AXI4BundleParameters
import chisel3._

//noinspection ScalaUnusedSymbol
class ACE(param: AXI4BundleParameters) extends AXI4Compat(param) {
  val arsnoop = Output(UInt(4.W))
  val ardomain = Output(UInt(2.W))
  val arbar = Output(UInt(2.W))

  val awsnoop = Output(UInt(3.W))
  val awdomain = Output(UInt(2.W))
  val awbar = Output(UInt(2.W))
  val awunique = Output(Bool())

  override val rresp = Output(UInt(4.W))

  val acvalid = Input(Bool())
  val acready = Output(Bool())
  val acaddr = Input(UInt(param.addrBits.W))
  val acsnoop = Input(UInt(4.W))
  val acprot = Input(UInt(3.W))

  val crvalid = Output(Bool())
  val crready = Input(Bool())
  val crresp = Output(UInt(5.W))

  val cdvalid = Output(Bool())
  val cdready = Input(Bool())
  val cddata = Output(UInt(param.dataBits.W))
  val cdlast = Output(Bool())

  /**
   * RESET REQUIREMENTS:
   * When reset is driven, rack, wack, crvalid and cdvalid must be low
   */
  val rack = Output(Bool())
  val wack = Output(Bool())
}
