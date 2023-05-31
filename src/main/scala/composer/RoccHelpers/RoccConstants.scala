package composer.RoccHelpers

import chipsalliance.rocketchip.config.Parameters
import chisel3.UInt
import chisel3.util.log2Up
import composer.common.ComposerRoccCommand
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tile.OpcodeSet

object ComposerOpcode extends Enumeration {
  val FLUSH = OpcodeSet.custom0.opcodes(0)
  val ACCEL = OpcodeSet.custom3.opcodes(0)
  type ComposerOpcode = Int
}

object ComposerConsts {
  def InternalCommandWidth(implicit p: Parameters): Int = log2Up(ComposerRoccCommand.packLengthBytes)

  def getInternalCmdRoutingAddressSet(systemID: Int)(implicit p: Parameters): AddressSet =
    AddressSet(systemID << InternalCommandWidth, (1 << InternalCommandWidth) - 1)

  def getInternalCmdRoutingAddress(systemID: UInt)(implicit p: Parameters): UInt =
    (systemID << InternalCommandWidth).asUInt

}
