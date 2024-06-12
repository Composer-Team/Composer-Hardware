package beethoven.Platforms.ASIC

import beethoven.Platforms.HasPostProccessorScript

abstract class TechLib extends HasPostProccessorScript {
  val memoryCompiler: MemoryCompiler
}
