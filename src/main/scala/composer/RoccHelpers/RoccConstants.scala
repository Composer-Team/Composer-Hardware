package composer.RoccHelpers

import chisel3.UInt
import chisel3.util.log2Up
import composer.common.ComposerRoccCommand
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tile.OpcodeSet

object ComposerFunc extends Enumeration {
  val START = 1
  val ADDR = 0
  type ComposerFunc = Int
}

object ComposerOpcode extends Enumeration {
  val FLUSH = OpcodeSet.custom0.opcodes(0)
  val ACCEL = OpcodeSet.custom3.opcodes(0)
  type ComposerOpcode = Int
}

object ComposerConsts {
  val InternalCommandWidth = log2Up(ComposerRoccCommand.packLengthBytes)

  def getInternalCmdRoutingAddressSet(systemID: Int): AddressSet =
    AddressSet(systemID << InternalCommandWidth, (1 << InternalCommandWidth) - 1)

  def getInternalCmdRoutingAddress(systemID: UInt): UInt =
    (systemID << InternalCommandWidth).asUInt

}

object RDReserves {
  // stats about AXI transactions
  val arCnt = 16
  val awCnt = 17
  val rCnt = 18
  val wCnt = 19
  val bCnt = 20
  val rWait = 21
  val bWait = 22
}