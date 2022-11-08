package composer
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.common.{ComposerRoccCommand, ComposerRoccResponse}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

abstract class CoreController(outer: LazyModule)(implicit p: Parameters) extends LazyModuleImp(outer) {
  def onRequest(crc: ComposerRoccCommand, cores: Seq[ComposerCore])
  def onCoreFinish(finishedCoreID: Int, cores: Seq[ComposerCore], resp: DecoupledIO[ComposerRoccResponse])
}
