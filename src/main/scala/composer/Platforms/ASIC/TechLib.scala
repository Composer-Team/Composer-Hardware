package composer.Platforms.ASIC

import composer.Platforms.HasPostProccessorScript

abstract class TechLib extends HasPostProccessorScript {
  val memoryCompiler: MemoryCompiler
}
