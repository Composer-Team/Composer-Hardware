package composer.Floorplanning

import chipsalliance.rocketchip.config.Parameters
import composer.Floorplanning.LazyModuleWithSLRs.globalNameList
import freechips.rocketchip.diplomacy.{LazyModule, ValName}

object LazyModuleWithSLRs {
  private[composer] var globalNameList: List[String] = List.empty
}

abstract class LazyModuleWithSLRs()(implicit p: Parameters) extends LazyModule {
  var lazyClockMap: List[(LazyModule, Int)] = List.empty
  val baseName: String
  private var gl_id = 0

  def LazyModuleWithFloorplan[T <: LazyModule](mod: => T, slr_id: Int, name: String): T = {
    val lm = LazyModule(mod)
    if (!ConstraintGeneration.canDistributeOverSLRs()) return lm
    lm.suggestName(name)
    if (globalNameList.contains(name)) {
      throw new Exception(s"Name $name already exists in the globalNameList. Give this module($name) a name that will be globally unique")
    }
    globalNameList = name :: globalNameList
    ConstraintGeneration.addToSLR(name, slr_id)
    lazyClockMap = (lm, slr_id) :: lazyClockMap
    lm
//    annotations.foldRight(lm) { case (annot: AssignmentAnnotation, cs) => annot.transform(cs, slr_id, name) }
  }
  def LazyModuleWithFloorplan[T <: LazyModule](mod: => T, slr_id: Int)(implicit valName: ValName): T = {
    val lm = LazyModule(mod)
    if (!ConstraintGeneration.canDistributeOverSLRs()) return lm
    val name = baseName + "_" + valName.name
    lm.suggestName(name)
    if (globalNameList.contains(name)) {
      throw new Exception(s"Name $name already exists in the globalNameList. Give this module($name) a name that will be globally unique")
    }
    gl_id = gl_id + 1
    ConstraintGeneration.addToSLR(name, slr_id)
    lazyClockMap = (lm, slr_id) :: lazyClockMap
    lm
  }

}
