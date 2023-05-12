package composer.tuning

import firrtl.annotations.{ReferenceTarget, SingleTargetAnnotation}


case class ExportCSymbolAnnotation(target: ReferenceTarget, designObjective: DesignObjective)
  extends SingleTargetAnnotation [ReferenceTarget] {
  def targets = Seq(target)
  def duplicate(n: ReferenceTarget) = this.copy(n)

  override def toString: String = target.toString() + " " + designObjective.toString
}
