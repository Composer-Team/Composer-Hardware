package composer.Platforms.FPGA.Xilinx

import chipsalliance.rocketchip.config.{Config, Parameters}
import composer.Generation._
import composer.Platforms._
import composer.Platforms.FPGA._
import os.Path

object AWS_sole {
  def apply(c: Config, pths: Seq[Path]): Unit = {
  }
}

class AWSF1Platform(memoryNChannels: Int) extends U200Platform(memoryNChannels) with HasPostProccessorScript {
  override val platformDies = Seq(
    DieName("pblock_CL_mid", memoryBus = true),
    DieName("pblock_CL_bot", frontBus = true),
    DieName("pblock_CL_top"))

  override val platformPreferedDieCmdRespRoutingPath: Seq[String] = Seq(
    "pblock_CL_bot",
    "pblock_CL_mid",
    "pblock_CL_top")

  override def postProcessorMacro(c: Config, paths: Seq[Path]): Unit = {
    if (c(BuildModeKey) == BuildMode.Synthesis) {
      // rename composer.v to composer.sv
      val aws_dir = os.Path(ComposerBuild.composerGenDir) / "aws"
      val top_file = os.Path(ComposerBuild.composerGenDir) / "aws" / "composer.sv"
      os.makeDir.all(os.Path(ComposerBuild.composerGenDir) / "aws")
      os.remove.all(top_file)
      os.proc("touch", top_file.toString()).call()
//      os.proc("cp", "-r", os.Path(ComposerBuild.composerGenDir) / "composer.build" / "*", aws_dir).call()
      os.copy.over(os.Path(ComposerBuild.composerGenDir) / "composer.build", aws_dir)
      os.move(os.Path(ComposerBuild.composerGenDir) / "aws" / "ComposerTop.v", top_file)
      os.walk(os.Path(ComposerBuild.composerGenDir)).foreach(
        p =>
          if (p.last.endsWith(".cc") || p.last.endsWith(".h") || p.last.endsWith(".xdc"))
            os.copy(p, os.Path(ComposerBuild.composerGenDir) / "aws" / p.last)
      )
      val hdl_srcs = os.walk(os.Path(ComposerBuild.composerGenDir) / "aws").filter(p => p.last.endsWith(".v") || p.last.endsWith(".sv")).map {
        fname =>
          val rel_path = fname.relativeTo(os.Path(ComposerBuild.composerGenDir) / "aws")
          f"$$::env(HOME)/build-dir/design/$rel_path"
      }
      os.write.over(os.Path(ComposerBuild.composerGenDir) / "aws" / "src_list.tcl",
         f"set hdl_sources [list ${hdl_srcs.mkString(" \\\n")} ]")
      // write ip tcl
      val ip_tcl = os.Path(ComposerBuild.composerGenDir) / "aws" / "ip.tcl"
      os.write.over(ip_tcl, ComposerBuild.postProcessorBundles.map {
        case tclMacro(cmd, _) => cmd
        case _ => ""
      }.mkString("\n"))

      val xci_list = ComposerBuild.postProcessorBundles.filter(_.isInstanceOf[tclMacro]).collect {
        case tclMacro(_, xciPath) => xciPath
      }
      os.write.append(ip_tcl, f"\nset xci_list [list ${xci_list.map{p => f"$$::env(HOME)/build-dir/design/${p.relativeTo(ComposerBuild.targetDir / "aws")}"}.mkString(" \\\n")} ]")
    }
  }
}

case class tclMacro(cmd: String, xciPath: Path)