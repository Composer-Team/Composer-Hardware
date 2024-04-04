package composer.Generation.Stage

import chipsalliance.rocketchip.config.Config
import composer.Generation
import composer.Generation.ComposerBuild
import composer.Generation.ComposerBuild.targetDir
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.NoDCEAnnotation
import firrtl.{AnnotationSeq, CustomDefaultMemoryEmission, CustomDefaultRegisterEmission, EmitAllModulesAnnotation, MemoryNoInit, TargetDirAnnotation, VerilogEmitter}
import freechips.rocketchip.stage.TopModuleAnnotation
import os.Path

object ComposerAnnotations {
  /**
   * @param config                     configuration that contains all of the accelerator information
   * @param emitAllModules             enable if you want separate file for each module (recommended)
   * @param performDeadCodeElimination enable if you want dead code elimination (slow in FIRRTL, and makes debugging much more difficult)
   * @param targetDir                  output directory
   * @param c                          class for top module
   * @return
   */
  def apply(config: Config,
            c: Class[_],
            emitAllModules: Boolean = true,
            performDeadCodeElimination: Boolean = false,
            targetDir: Path = ComposerBuild.targetDir): AnnotationSeq =
    AnnotationSeq(Seq(
      new TargetDirAnnotation(targetDir.toString()),
      new TopModuleAnnotation(c),
      Generation.Stage.ConfigsAnnotation(config),
      CustomDefaultMemoryEmission(MemoryNoInit),
      CustomDefaultRegisterEmission(useInitAsPreset = false, disableRandomization = true),
      RunFirrtlTransformAnnotation(new VerilogEmitter)
    ) ++ (if (emitAllModules) Seq(new EmitAllModulesAnnotation(classOf[VerilogEmitter])) else Seq()
      ) ++ (if (performDeadCodeElimination) Seq(NoDCEAnnotation) else Seq()))

  /**
   * @param config                     configuration that contains all of the accelerator information
   * @param emitAllModules             enable if you want separate file for each module (recommended)
   * @param performDeadCodeElimination enable if you want dead code elimination (slow in FIRRTL, and makes debugging much more difficult)
   * @param targetDir                  output directory
   * @return
   */
  def no_class(config: Config,
               emitAllModules: Boolean = true,
               performDeadCodeElimination: Boolean = false,
               targetDir: Path = ComposerBuild.targetDir): AnnotationSeq =
    AnnotationSeq(Seq(
      new TargetDirAnnotation(targetDir.toString()),
      Generation.Stage.ConfigsAnnotation(config),
      CustomDefaultMemoryEmission(MemoryNoInit),
      CustomDefaultRegisterEmission(useInitAsPreset = false, disableRandomization = true),
      RunFirrtlTransformAnnotation(new VerilogEmitter)
    ) ++ (if (emitAllModules) Seq(new EmitAllModulesAnnotation(classOf[VerilogEmitter])) else Seq()
      ) ++ (if (performDeadCodeElimination) Seq(NoDCEAnnotation) else Seq()))

}
