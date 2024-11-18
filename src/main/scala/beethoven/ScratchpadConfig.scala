package beethoven

import beethoven.MemoryStreams._
import chipsalliance.rocketchip.config.Parameters

case class ScratchpadConfig(name: String,
                            dataWidthBits: Int,
                            nDatas: Int,
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


case class ScratchpadFeatures(readOnly: Boolean = false,
                              supportWriteback: Boolean = false,
                              supportMemRequest: Boolean = true,
                              specialization: ScratchpadSpecialization = ScratchpadSpecialization.flatPacked,
                              nBanks: Int = 1,
                              writeEnableMuxing: Boolean = false)

