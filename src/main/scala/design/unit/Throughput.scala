package design.unit

import chipsalliance.rocketchip.config.{Config, Parameters}
import composer._
import composer.MemoryStreams.{CReadChannelParams, txEmitAsOneTx}
import chisel3._
import chisel3.util._
import composer.RoccHelpers.ComposerFunc
import composer.common.ComposerRoccCommand
import design.Composer
import freechips.rocketchip.subsystem.ExtMem

/**
  * Read 1 << 14B from memory and relinquish control back to dispatcher
  *
  */
class Throughput(constructor: ComposerConstructor)(implicit p: Parameters) extends ComposerCore(constructor) {
  val s_idle :: s_dispatch :: s_read :: s_finishing :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val (reqChannel, dataChannel) = getReaderModule(name = "Reader",
    useSoftwareAddressing = false,
    dataBytes = 64,
    vlen = 1,
    idx = 0,
    transactionEmitBehavior = txEmitAsOneTx())

  val addrWidth = log2Up(p(ExtMem).get.master.size)
  val addr = Reg(UInt(addrWidth.W))

  reqChannel.bits.addr := addr
  reqChannel.valid := false.B
  reqChannel.bits.len := (1 << 14).U

  dataChannel.stop := false.B
  dataChannel.data.ready := true.B

  io.req.ready := state === s_idle
  io.busy := state =/= s_idle

  io.resp.valid := false.B
  io.resp.bits.data := getCoreID.U

  switch(state) {
    is(s_idle) {
      when(io.req.fire) {
        addr := io.req.bits.payload2(addrWidth - 1, 0)
        state := s_dispatch
      }
    }
    is(s_dispatch) {
      reqChannel.valid := true.B
      when(reqChannel.fire) {
        state := s_read
      }
    }
    is(s_read) {
      dataChannel.data.ready := true.B
      when(reqChannel.ready) {
        state := s_finishing
      }
    }
    is(s_finishing) {
      io.resp.valid := true.B
      when(io.resp.fire) {
        state := s_idle
      }
    }
  }
}

class ThroughputDispatch(constructor: ComposerConstructor)(implicit p: Parameters) extends ComposerCore(constructor) {
  val s_idle :: s_active :: s_allocate :: s_wait :: s_finishing :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val addrWidth = log2Up(p(ExtMem).get.master.size)
  val addr = Reg(UInt(addrWidth.W))
  val len_remaining = Reg(UInt(32.W))

  val cycleCount = Reg(UInt(io.resp.bits.data.getWidth.W))
  println("Cycle counter bits: " + io.resp.bits.data.getWidth + "b")
  val coreParams = p(ComposerSystemsKey).filter(_.name == "TCores")(0)
  val nCores = coreParams.nCores

  val coreIdleBits = Reg(Vec(nCores, Bool()))
  val haveIdleCore = coreIdleBits.reduce(_ || _)
  val coreAlloc = Reg(UInt(log2Up(nCores).W))
  when(reset.asBool) {
    coreIdleBits.foreach(_ := true.B)
  }

  composer_command_io.valid := false.B
  composer_command_io.bits := DontCare

  composer_response_io.ready := true.B

  io.req.ready := state === s_idle
  io.busy := state =/= s_idle

  io.resp.bits.data := cycleCount
  io.resp.valid := false.B

  when(composer_response_io.fire) {
    val coreResponseId = composer_response_io.bits.data(p(CoreIDLengthKey) - 1, 0)
    coreIdleBits(coreResponseId) := true.B
  }

  cycleCount := cycleCount + 1.U

  switch(state) {
    is(s_idle) {
      when(io.req.fire) {
        addr := io.req.bits.payload2(addrWidth - 1, 0)
        cycleCount := 0.U
        state := s_active
        len_remaining := (1L << 31).U
      }
    }
    is(s_active) {
      when(haveIdleCore && len_remaining > 0.U) {
        coreAlloc := PriorityEncoder(coreIdleBits)
        state := s_allocate
      }
      when(len_remaining === 0.U) {
        state := s_wait
      }
    }
    is(s_allocate) {
      composer_command_io.valid := true.B
      composer_command_io.bits := ComposerRoccCommand(genCoreCommand(
        name = "TCores",
        expectResponse = true.B,
        rs1 = 0.U,
        rs2 = 0.U,
        rd = 0.U,
        xs2 = 0.U,
        coreId = coreAlloc,
        payload1 = 0.U,
        payload2 = addr,
        funct = ComposerFunc.START))
      when(composer_command_io.fire) {
        coreIdleBits(coreAlloc) := false.B
        addr := addr + (1 << 14).U
        len_remaining := len_remaining - (1 << 14).U
        state := s_active
      }
    }
    is(s_wait) {
      when(coreIdleBits.reduce(_ && _)) {
        state := s_finishing
      }
    }
    is(s_finishing) {
      io.resp.valid := true.B
      when(io.resp.fire) {
        state := s_idle
      }
    }
  }
}

class withThroughputTesters(n: Int) extends Config((site, here, up) => {
  case ComposerSystemsKey =>
    Seq(
      ComposerSystemParams(nCores = 1,
        name = "TDisp",
        buildCore = {
          case (a, b) => new ThroughputDispatch(a)(b)
        },
        coreParams = ComposerCoreParams(),
        canIssueCoreCommands = true),
      ComposerSystemParams(nCores = n,
        name = "TCores",
        buildCore = {
          case (a, b) => new Throughput(a)(b)
        },
        coreParams = ComposerCoreParams(
          memoryChannelParams = List(CReadChannelParams("Reader", 1))
        ),
        canReceiveSoftwareCommands = false)
    )
})

class ThroughputTest extends Config(
  new withThroughputTesters(8) ++ new WithComposer() ++ new WithAWSMem(4)
)

object TDriver extends App {
  Composer.buildConfig(new ThroughputTest)
}