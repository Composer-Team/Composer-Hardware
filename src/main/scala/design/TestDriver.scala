package design

import chipsalliance.rocketchip.config.Config
import composer.{WithAWSMem, WithComposer, WithKriaMem}
import firrtl.options.StageMain
import freechips.rocketchip.system.{RocketChipStage, RocketChiselStage}
import firrtl.stage.FirrtlMain

object Composer {
  def buildConfig(config: Config): Unit = {
    val c_dir = System.getProperty("user.dir")
    val hw_idr = c_dir + "/vsim/generated-src/"
    val full_name = config.getClass.getCanonicalName
    val short_name = full_name.split('.').last
    println(full_name + " " + short_name)
    new RocketChipStage().execute(Array("-td", hw_idr, "-T", "composer.ComposerTop", "-C", full_name, "--emission-options=disableMemRandomization,disableRegisterRandomization"), Seq())
    FirrtlMain.stage.execute(Array("-i", hw_idr + "/composer." + short_name + ".fir", "-o", hw_idr + "composer.v", "-X", "verilog", "--emission-options=disableMemRandomization,disableRegisterRandomization"), Seq())
  }
}

//object GemmDriver extends App {
//  Composer.buildConfig(new GemmTestF1)
//}

object TestDriver extends App {
  Composer.buildConfig(new exampleConfig)
}
