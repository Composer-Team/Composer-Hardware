package beethoven.Generation.Stage

import chisel3.stage.ChiselOutputFileAnnotation
import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase}
import freechips.rocketchip.stage.phases.Checks

class TransformAnnotation extends Phase {
  override def invalidates(a: Phase): Boolean = false
  override val prerequisites = Seq(Dependency[Checks])
  override val dependents = Seq(Dependency[chisel3.stage.phases.AddImplicitOutputFile])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    /** Construct output file annotation for emission */
    new ChiselOutputFileAnnotation("Beethoven") +: annotations
  }
}
