package design.DT

import chipsalliance.rocketchip.config.Config
import composer.MemoryStreams._
import composer._
import design.Composer

//noinspection RedundantDefaultArgument
class WithDTAccelerator(nDTCores: Int, DTParams: DTParams) extends Config((site, _, up) => {
  case ComposerSystemsKey =>
    val prev = up(ComposerSystemsKey, site)
    // only one of this accelerator should exist for internal addressing reasons
    require(!prev.exists(_.name == "DTAccel"))
    prev ++ List(ComposerSystemParams(
      nCores = nDTCores,
      name = "DTCores",
      buildCore = {
        case (coreParams, parameters) =>
          new DecisionTreeCore(coreParams, DTParams)(parameters)
      },
      coreParams = ComposerCoreParams(
        memoryChannelParams = List(
          CScratchpadChannelParams(
            name = "feature",
            supportMemRead = true,
            supportWriteback = false,
            dataWidthBits = DTParams.featureCompression * 32,
            nDatas = DTParams.maxNFeatures / DTParams.featureCompression,
            latency = 2,
            specialization = new FlatPackScratchpadParams
          ),
          CScratchpadChannelParams(
            name = "treeThreshold",
            supportMemRead = true,
            supportWriteback = false,
            dataWidthBits = DTParams.thresholdCompression * 32,
            nDatas = (1 << DTParams.maxTreeDepth) * DTParams.treeParallelism / DTParams.thresholdCompression,
            latency = 2,
            specialization = new FlatPackScratchpadParams
          ),
          CScratchpadChannelParams(
            name = "featureIDs",
            supportMemRead = true,
            supportWriteback = false,
            dataWidthBits = DecisionTreeCore.getTotalIndexWidth(DTParams) * DTParams.indexCompression,
            nDatas = (1 << DTParams.maxTreeDepth) * DTParams.treeParallelism / DTParams.indexCompression,
            latency = 2,
            specialization = new FlatPackScratchpadParams
          )
        )
      ),
      canReceiveSoftwareCommands = false // only recieve commands from broaddcaster
    ),
      ComposerSystemParams(
        nCores = 1,
        name = "DTAccel",
        buildCore = {
          case (constr, params) =>
            new DecisionTreeDispatcher(constr, DTParams)(params)
        },
        coreParams = ComposerCoreParams(
          memoryChannelParams = List(new CWriteChannelParams(
            name = "OutputWriter",
            nChannels = 1
          ))
        ),
        canReceiveSoftwareCommands = true,
        canIssueCoreCommands = true
      )
    )
})

class DTConfig extends Config(
  new WithDTAccelerator(1, DTParams(4, 1, 1, 1, 1, 16, 4, 16)) ++ new WithComposer() ++ new WithAWSMem(1)
)

object DTDriver extends App {
  Composer.buildConfig(new DTConfig)
}
