package beethoven.Platforms.FPGA.Xilinx

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3.{Clock, Reset}
import beethoven.Floorplanning.LazyModuleWithSLRs
import beethoven.Generation._
import beethoven.Platforms.FPGA.Xilinx.Templates.SynthScript
import beethoven.Platforms._
import beethoven.Platforms.FPGA._
import beethoven.Protocol.FrontBus.{AXIFrontBusProtocol, FrontBusProtocol}
import freechips.rocketchip.amba.axi4.{AXI4MasterNode, AXI4MasterParameters, AXI4MasterPortParameters, AXI4ToTL}
import freechips.rocketchip.diplomacy.{IdRange, LazyModule}
import freechips.rocketchip.tilelink.TLIdentityNode
import os.Path

object AWS_sole {
  def apply(c: Config, pths: Seq[Path]): Unit = {
  }
}

object AWSF1Platform {
  def check_if_setup(ip: String): Boolean = {
    val res = os.proc("ssh", "ec2-user@" + ip, "ls", "~/aws-fpga").call()
    res.exitCode == 0
  }

  def initial_setup(ip: String): Unit = {
    val croot = sys.env("BEETHOVEN_ROOT")
    os.proc("rsync", "-azr", f"$croot/bin", f"ec2-user@$ip:~/bin").call()
    os.proc("ssh", f"ec2-user@$ip", "~/bin/aws/scripts/initial_setup.sh").call()
  }
}

class AWSF1Platform(memoryNChannels: Int,
                    val clock_recipe: String = "A0") extends U200Platform(memoryNChannels) with HasPostProccessorScript with
  PlatformHasSeparateDMA {
  override val DMAIDBits: Int = 6
  override val clockRateMHz: Int = clock_recipe match {
    case "A0" => 125
    case "A1" => 250
    case "A2" => 15
    case _ => throw new Exception("Invalid clock recipe. Only supporting A0, A1, A2 for main clock right now.")
  }

  override val defaultReadTXConcurrency = clockRateMHz match {
    case 125 => 4
    case 250 => 8
    case 15 => 1
  }

  override val defaultWriteTXConcurrency: Int = defaultReadTXConcurrency

  override def postProcessorMacro(c: Config, paths: Seq[Path]): Unit = {
    if (c(BuildModeKey) == BuildMode.Synthesis) {
      // rename beethoven.v to beethoven.sv
      val aws_dir = os.Path(BeethovenBuild.beethovenGenDir) / "aws"
      val top_file = os.Path(BeethovenBuild.beethovenGenDir) / "aws" / "beethoven.sv"
      os.makeDir.all(os.Path(BeethovenBuild.beethovenGenDir) / "aws")
      os.remove.all(top_file)
      os.proc("touch", top_file.toString()).call()
      //      os.proc("cp", "-r", os.Path(BeethovenBuild.beethovenGenDir) / "beethoven.build" / "*", aws_dir).call()
      os.copy.over(os.Path(BeethovenBuild.beethovenGenDir) / "beethoven.build", aws_dir)
      os.move(os.Path(BeethovenBuild.beethovenGenDir) / "aws" / "BeethovenTop.v", top_file)
      os.walk(os.Path(BeethovenBuild.beethovenGenDir)).foreach(
        p =>
          if (p.last.endsWith(".cc") || p.last.endsWith(".h") || p.last.endsWith(".xdc"))
            os.copy(p, os.Path(BeethovenBuild.beethovenGenDir) / "aws" / p.last)
      )
      val hdl_srcs = os.walk(os.Path(BeethovenBuild.beethovenGenDir) / "aws").filter(p => p.last.endsWith(".v") || p.last.endsWith(".sv")).map {
        fname =>
          val rel_path = fname.relativeTo(os.Path(BeethovenBuild.beethovenGenDir) / "aws")
          f"$$::env(HOME)/build-dir/design/$rel_path"
      }
      os.write.over(os.Path(BeethovenBuild.beethovenGenDir) / "aws" / "src_list.tcl",
        f"set hdl_sources [list ${hdl_srcs.mkString(" \\\n")} ]")
      // write ip tcl
      val ip_tcl = os.Path(BeethovenBuild.beethovenGenDir) / "aws" / "ip.tcl"
      os.write.over(ip_tcl,
        """
          |set ipDir ips
          |exec rm -rf $ipDir/*
          |exec mkdir -p $ipDir
          |""".stripMargin + SynthScript(
          "",
          "",
          "",
          "",
          clockRateMHz.toString,
          precompile_dependencies = getTclMacros()
        ).ip_script + "\nupdate_compile_order -fileset sources_1\n")

      // get aws address from stdio input
      println("Compilation is done.")
      println("Enter the AWS F1 instance IP address (blank if ignore transfer):")
      var in = scala.io.StdIn.readLine().trim
      if (in.nonEmpty) {
        var fail = true
        while (fail) {
          fail = false
          try {
            println("Transfering...")
            os.proc("ssh", f"ec2-user@$in", "rm", "-rf", "~/build-dir/generated-src/*")
            os.proc("rsync", "-avz", f"${BeethovenBuild.beethovenGenDir}/aws/", f"ec2-user@$in:~/build-dir/generated-src/").call(
              stdout = os.Inherit
            )
          } catch {
            case e: Exception =>
              println(e)
              println("Error in rsync. Will try again with new IP address (blank if give up): ")
              in = scala.io.StdIn.readLine().trim
              fail = true
          }
        }
        if (!AWSF1Platform.check_if_setup(in)) {
          AWSF1Platform.initial_setup(in)
        }
      }
    }
  }

  override val physicalInterfaces: List[PhysicalInterface] = List(
    PhysicalHostInterface(0),
    PhysicalMemoryInterface(1, 0)
  ) ++ (if (memoryNChannels <= 1) List() else (1 until memoryNChannels).map(a => PhysicalMemoryInterface(a-1, a)))
  override val physicalConnectivity: List[(Int, Int)] = List((0,1), (1,2))

  override val physicalDevices: List[DeviceConfig] = List(
    DeviceConfig(0, "pblock_CL_bot"),
    DeviceConfig(1, "pblock_CL_mid"),
    DeviceConfig(2, "pblock_CL_top")
  )
}

case class tclMacro(cmd: String, xciPath: Path)