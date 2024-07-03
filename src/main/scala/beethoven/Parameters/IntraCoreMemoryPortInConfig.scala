package beethoven.Parameters

import beethoven.MemoryStreams.MemChannelConfig
import chisel3.util._

case class IntraCoreMemoryPortInConfig(name: String,
                                       nChannels: Int,
                                       portsPerChannel: Int,
                                       dataWidthBits: Int,
                                       nDatas: Int,
                                       readOnly: Boolean = false,
                                       latency: Number = 2) extends MemChannelConfig {
  require(isPow2(dataWidthBits) && dataWidthBits >= 8, "the width of CIntraCoreMemory ports is" +
    "currently restricted to power-of-2 sizes. If you need this changed, please contact developer.")
}

