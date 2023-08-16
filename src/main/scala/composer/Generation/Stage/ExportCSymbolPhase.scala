package composer.Generation.Stage

import firrtl.AnnotationSeq
import firrtl.options.Phase
import firrtl.stage.RunFirrtlTransformAnnotation


class ExportCSymbolPhase extends Phase {
  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    annotations :+ RunFirrtlTransformAnnotation(new ExportCSymbolTransform)
  }
}
