package beethoven.Generation.Stage

import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase}
import firrtl.stage.RunFirrtlTransformAnnotation


class ExportCSymbolPhase extends Phase {
  override def invalidates(a: Phase): Boolean = false

  override def prerequisites: Seq[Dependency[Phase]] = Seq(Dependency[chisel3.stage.phases.Convert])
  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    annotations :+ RunFirrtlTransformAnnotation(new ExportCSymbolTransform)
  }
}
