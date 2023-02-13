package design.DT

import chipsalliance.rocketchip.config.Config
import composer.MemoryStreams._
import composer._
import design.Composer
import design.DT.DTConfig._

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
            supportWriteback = false,
            dataWidthBits = DTParams.featureCompression * 32,
            nDatas = DTParams.maxNFeatures / DTParams.featureCompression,
            latency = 2,
            supportReadLength = DTParams.getLongestTxBytes,
            specialization = new FlatPackScratchpadParams
          ),
          CScratchpadChannelParams(
            name = "treeThreshold",
            supportWriteback = false,
            dataWidthBits = DTParams.thresholdCompression * 32,
            nDatas = (1 << DTParams.maxTreeDepth) * DTParams.treeParallelism / DTParams.thresholdCompression,
            latency = 2,
            supportReadLength = DTParams.getLongestTxBytes,
            specialization = new FlatPackScratchpadParams
          ),
          CScratchpadChannelParams(
            name = "featureIDs",
            supportWriteback = false,
            dataWidthBits = DTParams.getTotalIndexWidth * DTParams.indexCompression,
            nDatas = (1 << DTParams.maxTreeDepth) * DTParams.treeParallelism / DTParams.indexCompression,
            latency = 2,
            supportReadLength = DTParams.getLongestTxBytes,
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
object DTConfig {
  val bigConfig = DTParams(
    maxTreeDepth = 13, // used in paper
    treeParallelism = 4, // how many trees does the unit hold. URAM cell is 36kB / 32kB.
    indexCompression = 8, // index is 32b
    thresholdCompression = 8,
    featureCompression = 8, //32B per cycle
    maxNExamples = 64,
    maxNTrees = 512,
    maxNFeatures = 4096)

  val smallConfig = DTParams(
    maxTreeDepth = 6, // used in paper
    treeParallelism = 1, // how many trees does the unit hold. URAM cell is 36kB / 32kB.
    indexCompression = 1, // index is 32b
    thresholdCompression = 1,
    featureCompression = 1, //32B per cycle
    maxNExamples = 4,
    maxNTrees = 128,
    maxNFeatures = 16)

}
class BigDTConfig extends Config(
  new WithDTAccelerator(64, bigConfig) ++ new WithComposer() ++ new WithAWSMem(1)
)

class BiggerDTConfig extends Config(
  new WithDTAccelerator(96, bigConfig) ++ new WithComposer() ++ new WithAWSMem(1)
)

class SmallDTConfig extends Config(
  new WithDTAccelerator(4, smallConfig) ++ new WithComposer() ++ new WithAWSMem(1)
)

object DTDriver extends App {
  Composer.buildConfig(new BiggerDTConfig)
}
