package design

import firrtl.options.StageMain
import freechips.rocketchip.system.RocketChipStage
import firrtl.stage.FirrtlMain

object Driver extends App {
  val c_dir = System.getProperty("user.dir")
  val hw_idr = c_dir + "/vsim/generated-src/"
  println("the cname is " + (new exampleConfig).getClass.getCanonicalName)
  new RocketChipStage().execute(Array("-td", hw_idr, "-T", "composer.ComposerTop", "-C", "design.exampleConfig"), Seq())
//  run(Seq(
//    new TargetDirAnnotation(hw_idr),
//    new TopModuleAnnotation(Class.forName("composer.ComposerTop")),
//    new ConfigsAnnotation(Seq("design.exampleConfig"))))
  FirrtlMain.stage.execute(Array("-i", hw_idr + "/composer.exampleConfig.fir", "-o", hw_idr + "composer.v", "-X", "verilog"), Seq())
}