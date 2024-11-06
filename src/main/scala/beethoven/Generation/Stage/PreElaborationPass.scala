package beethoven.Generation.Stage

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3.RawModule
import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.options._
import firrtl.options.Viewer.view
import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.Flatten
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.stage.RocketChipOptions
import freechips.rocketchip.stage.phases.{Checks, TargetDirKey}
import freechips.rocketchip.util.HasRocketChipStageUtils


case class ConfigsAnnotation(literalConfig: Any) extends NoTargetAnnotation

class PreElaborationPass extends Phase with HasRocketChipStageUtils {
  override def invalidates(a: Phase): Boolean = false
  override val prerequisites = Seq(Dependency[Checks])
  override val dependents = Seq(Dependency[chisel3.stage.phases.Elaborate])

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {

    val stageOpts = view[StageOptions](annotations)
    val rOpts = view[RocketChipOptions](annotations)
    val topMod = rOpts.topModule.get
    val config = annotations.filter(_.isInstanceOf[ConfigsAnnotation])(0).asInstanceOf[ConfigsAnnotation].literalConfig match {
      case c: Config => c.alterPartial {
        case TargetDirKey => stageOpts.targetDir
      }
      case p: Parameters => p.alterPartial {
        case TargetDirKey => stageOpts.targetDir
      }
    }

    val gen = () =>
      topMod
        .getConstructor(classOf[Parameters])
        .newInstance(config) match {
        case a: RawModule => a
        case a: LazyModule => {
          val lm = LazyModule(a)
          lm.module
        }
      }

    ChiselGeneratorAnnotation(gen) +: annotations :+ RunFirrtlTransformAnnotation(new Flatten)
  }

}

