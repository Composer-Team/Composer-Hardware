package composer.Protocol

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4.AXI4BundleParameters
import composer.MemoryStreams.Memory


object ACERegionManager {
  val ace_cmd_add :: ace_cmd_remove :: ace_cmd_flush_invalidate :: Nil = Enum(3)
  val ace_resp_err_none :: ace_resp_err_no_mapping :: ace_resp_err_no_room :: Nil = Enum(3)
}

class ACERegionManagementCommand(params: AXI4BundleParameters) extends Bundle {
  val addr = Output(UInt(params.addrBits.W))
  val len = Output(UInt(params.lenBits.W)) // n cache lines
  /**
   * Operations
   * ADD
   * REMOVE
   * FLUSH & INVALIDATE
   */
  val operation = Output(UInt(2.W))
}

class ACERegionManagementResponse extends Bundle {
  /**
   * Errors
   * 0 - no error
   * 1 - mapping not found when REMOVE
   * 2 - no more room when ADD
   */
  val error = Output(UInt(2.W))
}

class ACERegionManager(params: AXI4BundleParameters,
                       maxSegments: Int)(implicit p: Parameters) extends Module {
  val out = IO(new ACE(params))
  val mapping_memory = Memory(3, params.addrBits + params.lenBits,
    maxSegments, 0, 0, 1, debugName = Some("ACERegionManagerMemory"))
  Seq(out, mapping_memory).foreach(_ <> DontCare)
  out.rack := false.B
  out.wack := false.B
  out.awvalid := false.B
  out.arvalid := false.B
  out.wvalid := false.B
  out.rready := false.B
  out.bready := false.B
  out.acready := false.B
  out.crvalid := false.B
  out.cdvalid := false.B

  val in_cmd = IO(Flipped(Decoupled(new ACERegionManagementCommand(params))))
  val out_resp = IO(Decoupled(new ACERegionManagementResponse))


  val s_idle :: s_trivial_resp :: s_read_flush :: s_emit :: Nil = Enum(0)
  val state = RegInit(s_idle)

  val addrHold = Reg(UInt(params.addrBits.W))
  val snoopCmd = Reg(UInt(4.W))
  val snoopProt = Reg(UInt(3.W))

  when (state === s_idle) {
    // always accept incoming snoop requests first
    out.acready := true.B
    in_cmd.ready := true.B
    when (out.acvalid) {
      in_cmd.ready := false.B
      addrHold := out.acaddr

    }.elsewhen(in_cmd.fire) {

    }

  }



}
