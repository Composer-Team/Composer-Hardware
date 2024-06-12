package beethoven.Generation.Stage

import chisel3.stage.ChiselOutputFileAnnotation
import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase, PreservesAll}
import freechips.rocketchip.stage.phases.Checks

class TransformAnnotations extends Phase with PreservesAll[Phase] {

  override val prerequisites = Seq(Dependency[Checks])
  override val dependents = Seq(Dependency[chisel3.stage.phases.AddImplicitOutputFile])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    /** Construct output file annotation for emission */
    new ChiselOutputFileAnnotation("Beethoven") +: annotations
  }
}
