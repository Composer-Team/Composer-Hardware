package composer

import firrtl.options.TargetDirAnnotation
import freechips.rocketchip.stage.{ConfigsAnnotation, TopModuleAnnotation}
import freechips.rocketchip.system.RocketChipStage


object Driver extends App {
  new RocketChipStage().run(Seq(
    new TargetDirAnnotation(System.getProperty("user.dir") + "/my_build/"),
    new TopModuleAnnotation(Class.forName("design.ComposerTop")),
    new ConfigsAnnotation(Seq("composer.TemplateConfig"))))
}