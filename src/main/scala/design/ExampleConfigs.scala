package design

import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chisel3.{Data, Module}
import composer.{ComposerChannelParams, ComposerConstructor, ComposerCoreParams, ComposerSystemParams, ComposerSystemsKey, WithAWSMem, WithComposer}

case object LFSRConfigKey extends Field[LFSRConfig]
case object VectorAdderKey extends Field[VectorConfig]

class WithLFSR(withNCores: Int) extends Config((site, here, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
      coreParams = ComposerCoreParams(
        readChannelParams = Seq(),
        writeChannelParams = Seq()
      ),
      nCores = withNCores,
      name = "LFSRSystem",
      buildCore = {
        case (coreParams: ComposerConstructor, parameters: Parameters) =>
          new LFSRCore(coreParams)(parameters)
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
    name = "ALUSystem",
    buildCore = {
      case (coreParams: ComposerConstructor, parameters: Parameters) =>
        new SimpleALU(coreParams)(parameters)
    }))
})

class WithVectorAdder(withNCores: Int, dataWidth: Int) extends Config((site, here, up) => {
  case ComposerSystemsKey => up(ComposerSystemsKey, site) ++ Seq(ComposerSystemParams(
    coreParams = ComposerCoreParams(
      readChannelParams = Seq(ComposerChannelParams()),
      writeChannelParams = Seq(ComposerChannelParams())
    ),
    nCores = withNCores,
    name = "VectorSystem",
    buildCore = {
      case (composerCoreParams: ComposerConstructor, parameters: Parameters) =>
        new VectorAdder(composerCoreParams)(parameters)
    }
  ))

  // use 8-bit data divisions
  case VectorAdderKey => VectorConfig(dWidth = dataWidth)
})
class exampleConfig extends Config (
  // example configuration that has
  //  - 1 SimpleALU that supports add, sub, multiply
  //  - 1 VectorAdder that uses Readers/Writers to read/write to large chunks of memory
  //  - 1 GaloisLFSR that returns random numbers over the command/response interface
  new WithALUs(1) ++ new WithVectorAdder(1, 16) ++ new WithLFSR(1) ++
    new WithComposer() ++ new WithAWSMem(1)
)