package beethoven.Platforms.FPGA.Xilinx.F2

import beethoven.Platforms.FPGA.Xilinx.F2.AWSF2Platform._
import beethoven.Platforms.FPGA.Xilinx.{Templates, getTclMacros}
import beethoven.Platforms.PlatformType.PlatformType
import beethoven.Platforms._
import beethoven.Protocol.FrontBus.{AXIFrontBusProtocol, FrontBusProtocol}
import beethoven._
import chipsalliance.rocketchip.config._
import os.Path

object AWSF2Platform {
  def check_if_setup(ip: String): Boolean = {
    val res = os.proc("ssh", "ubuntu@" + ip, "ls", "~/aws-fpga").call()
    res.exitCode == 0
  }

  def initial_setup(ip: String): Unit = {
    val croot = sys.env("BEETHOVEN_PATH")
    os.proc("rsync", "-azr", f"$croot/bin", f"ubuntu@$ip:~/bin").call()
    os.proc("ssh", f"ubuntu@$ip", "~/bin/aws/scripts/initial_setup.sh").call()
  }
}

class AWSF2Platform extends
  Platform with
  HasPostProccessorScript with
  PlatformHasSeparateDMA with
  HasXilinxMem {

  override val memoryNChannels: Int = 1
  override val platformType: PlatformType = PlatformType.FPGA
  override val hasDiscreteMemory: Boolean = true

  override val frontBusBaseAddress: Long = 0
  override val frontBusAddressNBits: Int = 16
  override val frontBusAddressMask: Long = 0xFFFF
  override val frontBusBeatBytes: Int = 4
  override val frontBusProtocol: FrontBusProtocol = new AXIFrontBusProtocol(true)

  override val physicalMemoryBytes: Long = 0x400000000L
  override val memorySpaceAddressBase: Long = 0x0
  override val memorySpaceSizeBytes: BigInt = BigInt(2).pow(64)
  println(memorySpaceSizeBytes)
  override val memoryControllerIDBits: Int = 16
  override val memoryControllerBeatBytes: Int = 64

  override val prefetchSourceMultiplicity: Int = 64

  override val isActiveHighReset: Boolean = false

  override val DMAIDBits: Int = 6
  override val clockRateMHz: Int = 250

  override val defaultReadTXConcurrency = 8
  override val defaultWriteTXConcurrency: Int = defaultReadTXConcurrency

  override def postProcessorMacro(c: Parameters, paths: Seq[Path]): Unit = {
    if (c(BuildModeKey) == BuildMode.Synthesis) {
      // rename beethoven.v to beethoven.sv
      val aws_dir = BeethovenBuild.top_build_dir / "aws"
      val gen_dir = aws_dir / "cl_beethoven_top" / "generated-src"
      val run_dir = aws_dir / "cl_beethoven_top" / "build" / "scripts"
      val top_file = gen_dir / "beethoven.sv"
      os.makeDir.all(gen_dir)
      os.makeDir.all(run_dir)
      os.proc("touch", top_file.toString()).call()
      Shell.write(BeethovenBuild.hw_build_dir / "cl_beethoven_top.sv")(c)
      Shell.write_header(BeethovenBuild.hw_build_dir / "cl_beethoven_top_defines.vh")(c)

      os.copy.over(BeethovenBuild.hw_build_dir, gen_dir)
      os.move(gen_dir / "BeethovenTop.v", top_file)
      os.walk(BeethovenBuild.top_build_dir, followLinks=false, maxDepth = 1).foreach(
        p =>
          if (p.last.endsWith(".cc") || p.last.endsWith(".h") || p.last.endsWith(".xdc")) {
//            println("copying " + p + " to " + gen_dir / p.last)
            os.copy.over(p, gen_dir / p.last)
          }
      )
      val hdl_srcs = os.walk(gen_dir).filter(p =>
        (p.last.endsWith(".v") || p.last.endsWith(".sv")) && !(p.last.contains("VCS"))).map {
        fname =>
          fname.relativeTo(run_dir)
      }
      os.write.over(run_dir / "src_list.tcl",
        f"set hdl_sources [list ${hdl_srcs.mkString(" \\\n")} ]")


      // write ip tcl
      val ip_tcl = BeethovenBuild.top_build_dir / "aws" / "ip.tcl"
      os.write.over(ip_tcl,
        """
          |set ipDir ips
          |exec rm -rf $ipDir/*
          |exec mkdir -p $ipDir
          |""".stripMargin + Templates.SynthScript(
          "",
          "",
          "",
          "",
          clockRateMHz.toString,
          precompile_dependencies = getTclMacros()
        ).ip_script + "\nupdate_compile_order -fileset sources_1\n")
      SynthScript.write(BeethovenBuild.top_build_dir / "synth_cl_beethoven_top.tcl")

      // get aws address from stdio input
      println("Compilation is done.")
      println("Enter the AWS F2 instance EC2 instance IP address (blank if store locally) :")
      var in = scala.io.StdIn.readLine().trim
      if (in.nonEmpty) {
        var fail = true
        while (fail) {
          fail = false
          try {
            println("Transfering...")
            // should be using the Ubuntu Xilinx Vivado 2024.1 image which has username ubunutu
            os.proc("ssh", f"ubuntu@$in",
              "rm", "-rf", "~/cl_beethoven_top/design/generated-src", "~/cl_beethoven_top/design", "~/cl_beethoven_top/generated-src", "&&",
              "mkdir", "-p", "~/cl_beethoven_top").call()
            os.proc("rsync", "-avz", f"$gen_dir", f"ubuntu@$in:~/cl_beethoven_top/").call()
            os.proc("ssh", f"ubuntu@$in",
              "mkdir", "-p", "~/cl_beethoven_top/build/scripts/", "&&",
              "mkdir", "-p", "~/cl_beethoven_top/build/constraints", "&&",
              "mv", "~/cl_beethoven_top/generated-src", "~/cl_beethoven_top/design", "&&",
              "cp", "-r", "~/aws-fpga/hdk/common/shell_stable/build/scripts ~/cl_beethoven_top/build/", "&&",
              "cp", "-r", "~/aws-fpga/hdk/cl/examples/CL_TEMPLATE/design/cl_id_defines.vh ~/cl_beethoven_top/design/", "&&",
              "cp", "~/aws-fpga/hdk/common/shell_stable/build/constraints/small_shell_level_1_fp_cl.xdc", "~/cl_beethoven_top/build/constraints/small_shell_cl_pnr_user.xdc", "&&",
              "touch", "~/cl_beethoven_top/build/constraints/cl_synth_user.xdc", "&&",
              "touch", "~/cl_beethoven_top/build/constraints/cl_timing_user.xdc").call()
            os.proc("rsync", "-avz", (run_dir / "src_list.tcl").toString(), f"ubuntu@$in:~/cl_beethoven_top/design/").call()
            os.proc("rsync", "-avz", (BeethovenBuild.top_build_dir / "synth_cl_beethoven_top.tcl").toString(), f"ubuntu@$in:~/cl_beethoven_top/build/scripts/").call()
            os.proc("rsync", "-avz", (BeethovenBuild.top_build_dir / "user_constraints.xdc").toString(), f"ubuntu@$in:~/cl_beethoven_top/build/scripts/").call()
          } catch {
            case e: Exception =>
              println(e)
              println("Error in rsync. Will try again with new IP address (blank if give up): ")
              in = scala.io.StdIn.readLine().trim
              fail = true
          }
        }
        if (!check_if_setup(in)) {
          initial_setup(in)
        }
      }
    }
  }

  override val physicalInterfaces: List[PhysicalInterface] = List(
    PhysicalHostInterface(0),
    PhysicalMemoryInterface(1, 0)
  ) ++ (if (memoryNChannels <= 1) List() else (1 until memoryNChannels).map(a => PhysicalMemoryInterface(a - 1, a)))
  override val physicalConnectivity: List[(Int, Int)] = List((0, 1), (1, 2))

  override val physicalDevices: List[DeviceConfig] = List(
    DeviceConfig(0, "pblock_CL_bot"),
    DeviceConfig(1, "pblock_CL_mid"),
    DeviceConfig(2, "pblock_CL_top")
  )
  /**
   * We won't _fail_ if we run out of memory, but there will be a warning and the memories will no longer be annotated
   * with a specific memory type (e.g., URAM/BRAM). This should give Vivado the freedom it needs to potentially not
   * fail placement
   */
    // 320 * (2/3) = 212
    // try to only use up to 80% (Xilinx Recommendation)
  override val nURAMs: Map[Int, Int] = Map.from(List((0, 160), (1, 160), (2, 256))) // 960 (320 per) but try not to get too close to overallocation
  override val nBRAMs: Map[Int, Int] = Map.from(List((0, 384), (1, 384), (2, 576))) // 2160 (720 per) but the shell takes about 30%

  override val net_intraDeviceXbarLatencyPerLayer: Int = 1
  override val net_intraDeviceXbarTopLatency: Int = 2
  override val net_fpgaSLRBridgeLatency: Int = 1

  override def placementAffinity: Map[Int, Double] = Map.from(Seq((0, 1.0), (1, 1.0), (2, 1.7)))

}

case class tclMacro(cmd: String, xciPath: Path)