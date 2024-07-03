package beethoven.Parameters

import beethoven.MemoryStreams._
import chipsalliance.rocketchip.config.Parameters

case class ScratchpadConfig(name: String,
                            dataWidthBits: Number,
                            nDatas: Number,
                            nPorts: Int,
                            latency: Number = 2,
                            features: ScratchpadFeatures = ScratchpadFeatures()) extends MemChannelConfig {
  override val nChannels: Int = 1

  private[beethoven] def make(implicit p: Parameters): MemoryScratchpad = {
    new MemoryScratchpad(this)
  }
}

/**
 * All Scratchpad specializtion types should announce themselves in this object
 */
object ScratchpadSpecialization {
  def packedSubword(wordSizeBits: Int, datsPerSubword: Int): PackedSubwordScratchpadConfig = {
    PackedSubwordScratchpadConfig(wordSizeBits, datsPerSubword)
  }

  def flatPacked: FlatPackScratchpadConfig = new FlatPackScratchpadConfig
}


case class ScratchpadFeatures(readOnly: Boolean = false,
                              supportWriteback: Boolean = false,
                              supportMemRequest: Boolean = true,
                              specialization: ScratchpadSpecialization = ScratchpadSpecialization.flatPacked,
                              nBanks: Int = 1,
                              writeEnableMuxing: Boolean = false)

