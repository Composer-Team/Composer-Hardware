package design.Gemm

import chipsalliance.rocketchip.config.Config
import composer.{WithAWSMem, WithComposer}
import design.Composer

object GP {
  val unitParams = GemmParam(dataWidthBytes = 4,
    rowColDim = 16,
    columnParallelism = 4,
    rowParallelism = 1,
    maxRCDim = 2048,
    prefetchAmt = 2)
  val F1Params = GemmParam(dataWidthBytes = 4,
    rowColDim = 256,
    columnParallelism = 2,
    rowParallelism = 8,
    maxRCDim = 2048,
    prefetchAmt = 8)
}

class WorkingF1NoDispatcher extends Config(
  new WithGemm(5, GP.F1Params) ++ new WithComposer(256 * 4 * 2) ++ new WithAWSMem(1)
)

class FirstTryF1WithDispatcher extends Config(
  new GemmWithDispatchConfig(5, GP.F1Params) ++ new WithComposer(256 * 4 * 2) ++ new WithAWSMem(1)
)

class SmallDispatcher extends Config(
  new GemmWithDispatchConfig(5, GP.unitParams) ++ new WithComposer(256 * 4 * 2) ++ new WithAWSMem(1)
)

//noinspection ScalaUnusedSymbol
class GemmTestF1 extends Config(
  new WithGemm(4, GP.unitParams) ++ new WithComposer() ++ new WithAWSMem(1)
)

//noinspection ScalaUnusedSymbol
class GemmTestF1Big extends Config(
  new WithGemm(6, GP.F1Params) ++ new WithComposer(256 * 4 * 2) ++ new WithAWSMem(1)
)
//object GemmFloatDriver extends App {
//  Composer.buildConfig(new GemmFloatConfig())
//}

class GemmDispatchF1Config extends Config (new GemmWithDispatchConfig(2, GP.unitParams) ++ new WithAWSMem(1) ++ new WithComposer())

object GemmSmallDriver extends App {
  Composer.buildConfig(new SmallDispatcher)
}

object GemmDispatchDriver extends App {
  Composer.buildConfig(new FirstTryF1WithDispatcher)
}