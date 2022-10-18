package composer

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._
import freechips.rocketchip.config.{Field, Parameters}

import composer.common._

class ComposerCoreIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val req = Flipped(DecoupledIO(new ComposerRoccCommand))
  val resp = DecoupledIO(new ComposerRoccResponse)
  val busy = Output(Bool())
}

class DataChannelIO(maxBytes: Int) extends Bundle {
  val data = Decoupled(UInt((maxBytes * 8).W))
  val stop = Input(Bool())
  val finished = Output(Bool())
}

class ComposerCore(val composerCoreParams: ComposerCoreParams)(implicit p: Parameters) extends Module { // with HasCoreParams
  val io = IO(new ComposerCoreIO())

  val readChannels: Seq[DataChannelIO] = if (!composerCoreParams.customRead) {
    composerCoreParams.readChannelParams.map {
      ch => IO(Flipped(new DataChannelIO(ch.widthBytes)))
    }
  } else {
    Seq()
  }

  val writeChannels: Seq[DataChannelIO] = {
    composerCoreParams.writeChannelParams.map {
      ch => IO(new DataChannelIO(ch.widthBytes))
    }
  }

  io.resp.bits.system_id := composerCoreParams.system_id.U
  io.resp.bits.core_id := composerCoreParams.core_id.U
}

class ComposerSystemIO(implicit p: Parameters) extends Bundle {
  val cmd = Flipped(Decoupled(new ComposerRoccCommand))
  val resp = Decoupled(new ComposerRoccResponse())
  val busy = Output(Bool())
}