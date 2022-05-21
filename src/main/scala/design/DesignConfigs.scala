package design

import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chisel3.{Data, Module}
import composer.{ComposerChannelParams, ComposerCoreParams, ComposerSystemParams, ComposerSystemsKey, WithAWSMem, WithComposer}

case object LFSRConfigKey extends Field[LFSRConfig]

class WithLFSR(withNCores: Int) extends Config((site, here, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
      coreParams = ComposerCoreParams(
        readChannelParams = Seq(),
        writeChannelParams = Seq()
      ),
      nCores = withNCores,
      system_id = 0,
      buildCore = {
        case (coreParams: ComposerCoreParams, parameters: Parameters) =>
          Module(new LFSRCore(coreParams)(parameters))
      }))

  case LFSRConfigKey => LFSRConfig(7, Seq(7, 6, 5, 4))
})
class MyLFSRConfig extends Config (
  new WithLFSR(1) ++
    new WithComposer ++
    new WithAWSMem
)