package beethoven.Floorplanning

import freechips.rocketchip.diplomacy._

object LazyModuleWithSLRs {
  private[beethoven] var globalNameList: List[String] = List.empty
  private[beethoven] var toplevelObjectsPerSLR: List[(Int, LazyModule)] = List.empty
  private[beethoven] def clearObjectsPerSLR(): Unit = toplevelObjectsPerSLR = List.empty
  private[beethoven] var freezeSLRPush: Boolean = false
  private var gl_id = 0

  def LazyModuleWithFloorplan[T <: LazyModule](mod: T): T = {
    val name = s"anonymous_${mod.desiredName}_" + gl_id
    gl_id = gl_id + 1
    LazyModuleWithFloorplan(mod, name)
  }

  def LazyModuleWithFloorplan[T <: LazyModule](mod: T, name: String): T = {
    DeviceContext.currentDevice match {
      case None =>
        val s = LazyModule(mod)
        s.suggestName(name)
        s
      case Some(r) => LazyModuleWithFloorplan(mod, r, name)
    }
  }

  def LazyModuleWithFloorplan[T <: LazyModule](mod: T, slr_id: Int, name: String): T = {
    val freeze = freezeSLRPush
    freezeSLRPush = true
    val lm = LazyModule(mod)
    freezeSLRPush = freeze
    lm.suggestName(name)
    if (globalNameList.contains(name)) {
      throw new Exception(s"Name $name already exists in the globalNameList. Give this module($name) a name that will be globally unique")
    }
    globalNameList = name :: globalNameList
    if (!freezeSLRPush) {
      toplevelObjectsPerSLR = (slr_id, lm) :: toplevelObjectsPerSLR
    }
    ConstraintGeneration.addToSLR(name, slr_id)
    lm
  }

  def LazyModuleWithFloorplan[T <: LazyModule](mod: T, slr_id: Int): T = {
    val lm = LazyModule(mod)
    val name = s"anonymous_${mod.desiredName}_" + gl_id
    lm.suggestName(name)
    if (globalNameList.contains(name)) {
      throw new Exception(s"Name $name already exists in the globalNameList. Give this module($name) a name that will be globally unique")
    }
    gl_id = gl_id + 1
    if (!freezeSLRPush) {
      toplevelObjectsPerSLR = (slr_id, lm) :: toplevelObjectsPerSLR
    }
    ConstraintGeneration.addToSLR(name, slr_id)
    lm
  }

}
