package beethoven.Parameters

import beethoven.MemoryStreams.MemChannelConfig

case class IntraCoreMemoryPortOutConfig(name: String,
                                        toSystem: String,
                                        toMemoryPort: String,
                                        nChannels: Int = 1) extends MemChannelConfig {
  // this has to be a bit special because this port is coupled to another existing port declared somewhere else
}
