package design

import chipsalliance.rocketchip.config.Config
import chisel3.stage.PrintFullStackTraceAnnotation
import design.Gemm.{GemmTestF1, GemmTestF1Big}
import design.unit.{exampleConfig, exampleConfigKria}
import firrtl.annotations.DeletedAnnotation
import freechips.rocketchip.system._
import firrtl.stage.FirrtlMain

import java.io.FileWriter
import java.io.{File,FileInputStream,FileOutputStream}

object Composer {
  def buildConfig(config: Config): Unit = {
    val c_dir = System.getProperty("user.dir")
    val hw_idr = c_dir + "/vsim/generated-src/"
    val full_name = config.getClass.getCanonicalName
    val short_name = full_name.split('.').last
    println(full_name + " " + short_name)
    val outputAnnos = new RocketChipStage().execute(
      args = Array("-td", hw_idr,
        "-T", "composer.ComposerTop",
        "-C", full_name,
        "--emission-options=disableMemRandomization,disableRegisterRandomization"),
      annotations = Seq())
    val writer = new FileWriter("outputAnnos.txt")
    outputAnnos.foreach(a =>
      if (!a.isInstanceOf[DeletedAnnotation])
        writer.write(a.toString))
    writer.close()

    val firrtlAnnos = FirrtlMain.stage.execute(
      args = Array("-i", hw_idr + "/composer." + short_name + ".fir",
        "-o", hw_idr + "composer.v",
        "-X", "verilog",
        "--emission-options=disableMemRandomization,disableRegisterRandomization",
        "-fct", "firrtl.passes.InlineInstances",
        "--infer-rw", "ComposerTop",
//        "--repl-seq-mem", s"-c:ComposerTop:-o:$hw_idr/ComposerTop.conf"
        ),
      annotations = Seq(PrintFullStackTraceAnnotation))

    val fWriter = new FileWriter("firrtlAnnos.txt")
    firrtlAnnos.foreach(a => fWriter.write(a.toString))
    val dest = new File(hw_idr + "/composer.sv")
    val src = new File(hw_idr + "/composer.v")
    new FileOutputStream(dest).getChannel.transferFrom(
      new FileInputStream(src).getChannel, 0, Long.MaxValue)

  }
}

object GemmDriver extends App {
  Composer.buildConfig(new GemmTestF1)
}

object GemmBig extends App {
  Composer.buildConfig(new GemmTestF1Big)
}

object TestDriver extends App {
  Composer.buildConfig(new exampleConfig)
}

object TestDriverKria extends App {
  Composer.buildConfig(new exampleConfigKria)
}