package beethoven.Protocol.RoCC.Helpers

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.log2Up
import beethoven.Parameters.AcceleratorSystems
import beethoven.common.AccelRoccCommand
import beethoven.common.Misc.round2Pow2
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tile.OpcodeSet

object BeethovenOpcode extends Enumeration {
  val FLUSH = OpcodeSet.custom0.opcodes(0)
  val ACCEL = OpcodeSet.custom3.opcodes(0)
  type BeethovenOpcode = Int
}

object BeethovenConsts {

  def InternalCommandSizeBytes()(implicit p: Parameters): Int = round2Pow2(AccelRoccCommand.packLengthBytes)

  def getInternalCmdRoutingAddressSet(systemID: Int)(implicit p: Parameters): AddressSet =
    AddressSet(systemID * InternalCommandSizeBytes(), InternalCommandSizeBytes() - 1)

  def getInternalCmdRoutingAddress(systemID: UInt)(implicit p: Parameters): UInt =
    systemID * InternalCommandSizeBytes().U

  def getInternalCmdRoutingAddressWidth()(implicit p: Parameters): Int =
    log2Up((p(AcceleratorSystems).size+1) * InternalCommandSizeBytes())

}
