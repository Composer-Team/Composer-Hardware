package composer.Floorplanning

import freechips.rocketchip.diplomacy.LazyModule

abstract class AssignmentAnnotation {
  def transform[T <: LazyModule](d: T, slr_id: Int, name: String): T
}
