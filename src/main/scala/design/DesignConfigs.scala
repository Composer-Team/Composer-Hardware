package design

import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chisel3.{Data, Module}
import composer.{ComposerChannelParams, ComposerCoreParams, ComposerSystemParams, ComposerSystemsKey, WithAWSMem, WithComposer}

case object LFSRConfigKey extends Field[LFSRConfig]
case object VectorAdderKey extends Field[VectorConfig]

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

class WithALUs(withNCores: Int) extends Config((site, here, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
    coreParams = ComposerCoreParams(
      readChannelParams = Seq(),
      writeChannelParams = Seq()
    ),
    nCores = withNCores,
    system_id = 1,
    buildCore = {
      case (coreParams: ComposerCoreParams, parameters: Parameters) =>
        Module(new SimpleALU(coreParams)(parameters))
    }))
})

class WithVectorAdder(withNCores: Int) extends Config((site, here, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
    coreParams = ComposerCoreParams(
      // just use default parameters, widthBytes=8 bytes
      readChannelParams = Seq(ComposerChannelParams()),
      writeChannelParams = Seq(ComposerChannelParams())
    ),
    nCores = withNCores,
    system_id = 2,
    buildCore = {
      case (composerCoreParams: ComposerCoreParams, parameters: Parameters) =>
        Module(new VectorAdder(composerCoreParams)(parameters))
    }
  ))

  // use 8-bit data divisions
  case VectorAdderKey => VectorConfig(dWidth = 8)
})
class MyLFSRConfig extends Config (
  new WithLFSR(1) ++
    new WithComposer ++
    new WithAWSMem
)

class MyALUConfig extends Config (
  new WithALUs(5) ++
    new WithComposer ++
    new WithAWSMem
)

class MyVectorAdderConfig extends Config (
  new WithVectorAdder(2) ++ new WithComposer ++ new WithAWSMem
)