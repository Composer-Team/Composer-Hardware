package composer.tuning

import firrtl.options.Phase
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.AnnotationSeq


class ExportCSymbolPhase extends Phase {
  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    annotations :+ RunFirrtlTransformAnnotation(new ExportCSymbolTransform)
  }
}
