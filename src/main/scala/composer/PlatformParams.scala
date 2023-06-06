package composer

import chipsalliance.rocketchip.config._
import composer.FrontBusProtocol.FrontBusProtocol
import composer.PlatformType.PlatformType
import composer.ProcessCorner.ProcessCorner
import composer.ProcessOperatingConditions.ProcessOperatingConditions
import composer.ProcessTemp.ProcessTemp
import composer.ProcessVoltageThreshold.ProcessVoltageThreshold
import freechips.rocketchip.subsystem._

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object PlatformType extends Enumeration {
  val FPGA, ASIC = Value
  type PlatformType = Value
}

case object PlatformTypeKey extends Field[PlatformType]

object FrontBusProtocol extends Enumeration {
  val AHB, AXIL, AXI4 = Value
  type FrontBusProtocol = Value
}

/** **** MEMORY *****
 */
case object HasDiscreteMemory extends Field[Boolean]

case object HasDMA extends Field[Option[Int]]

case object HasDisjointMemoryControllers extends Field[Boolean]

// memory capacities
case object PlatformNBRAM extends Field[Int]

case object PlatformNURAM extends Field[Int]

// mmio
case object FrontBusBaseAddress extends Field[Long]

case object FrontBusAddressMask extends Field[Long]

case object FrontBusBeatBytes extends Field[Int]

case object FrontBusProtocolKey extends Field[FrontBusProtocol]

// default clock rates (MHz)(used in simulation) - can be overriden
case object DefaultClockRateKey extends Field[Int]

// implementation specifics
case object PlatformSLRs extends Field[Option[Seq[SLRName]]]

case object PlatformNumSLRs extends Field[Int]

case object IsAWS extends Field[Boolean]

case object PostProcessorMacro extends Field[() => Unit]

case class SLRName(
                    name: String,
                    frontBus: Boolean = false,
                    memoryBus: Boolean = false
                  )

class WithKriaPlatform(nMemoryChannels: Int = 1)
  extends Config((_, _, _) => {
    case ExtMem =>
      Some(
        MemoryPortParams(
          MasterPortParams(
            base = 0,
            size = 1L << 49,
            beatBytes = 16,
            idBits = 6
          ),
          nMemoryChannels
        )
      )
    // 4GB total physical memory
    case PlatformPhysicalMemoryBytes => 4L << 30
    case FrontBusBaseAddress => 0x2000000000L
    case FrontBusAddressMask => 0xffffL
    case HasDMA => None
    // TODO this can be tuned
    case CXbarMaxDegree => 8
    case HasDiscreteMemory => false
    case FrontBusBeatBytes => 4
    case CoreCommandLatency => 0

    case PlatformTypeKey => PlatformType.FPGA
    case FrontBusProtocolKey => FrontBusProtocol.AXI4
    case PlatformNBRAM => 144
    case PlatformNURAM => 64
    case DefaultClockRateKey => 100
    case PlatformNumSLRs => 1
    case PlatformSLRs => None

    case IsAWS => false
    case PostProcessorMacro => () => ;
    case HasDisjointMemoryControllers => false
  })

private[composer] class U200Base(nMemoryChannels: Int)
  extends Config((_, _, _) => {
    case PlatformNumSLRs => 3
    case ExtMem =>
      val q = Some(
        MemoryPortParams(
          MasterPortParams(
            base = 0,
            size = 0x400000000L,
            beatBytes = 64,
            idBits = 6
          ),
          nMemoryChannels
        )
      )
      require(1 <= nMemoryChannels && nMemoryChannels <= 4)
      q
    // 16GB memory per DIMM
    case PlatformPhysicalMemoryBytes => (16L << 30) * nMemoryChannels
    case HasDMA => Some(6)
    // TODO this can be tuned
    case CXbarMaxDegree => 32
    case HasDiscreteMemory => true

    case PlatformTypeKey => PlatformType.FPGA
    case FrontBusProtocolKey => FrontBusProtocol.AXIL
    case FrontBusAddressMask => 0xffffL
    case FrontBusBaseAddress => 0L
    case FrontBusBeatBytes => 4
    case PlatformNURAM => 960
    case PlatformNBRAM => 2160

    case CoreCommandLatency => 4
    case HasDisjointMemoryControllers => true

  })

private[composer] class U200_sole()
  extends Config((_, _, _) => {
    case PlatformSLRs =>
      Some(
        Seq(
          SLRName("0", frontBus = true),
          SLRName("1", memoryBus = true),
          SLRName("2")
        )
      )
    case DefaultClockRateKey => 300
    case IsAWS => false
    case PostProcessorMacro => () => ;
  })

private[composer] class AWS_sole(simulation: Boolean)
  extends Config((_, _, _) => {
    // why did this ever become 128? It's 2X the bus width... That doesn't seem to make much sense...
    //  case CacheBlockBytes => 128
    case PlatformSLRs =>
      Some(
        Seq(
          SLRName("pblock_CL_bot", frontBus = true),
          SLRName("pblock_CL_mid", memoryBus = true),
          SLRName("pblock_CL_top")
        )
      )
    case DefaultClockRateKey => 125
    case IsAWS => true
    case PostProcessorMacro =>
      () =>
        if (!simulation) {
          val cwd = ComposerBuild.composerVsimDir
          val cdir = ComposerBuild.composerBin
          val callable = os.proc(f"$cdir/aws-gen-build")
          callable.call(
            cwd = os.Path(cwd),
            stdin = os.Inherit,
            stdout = os.Inherit
          )
        }
  })

class WithU200Platform extends Config(new U200Base(1) ++ new U200_sole)

class WithAWSPlatform(nMemoryChannels: Int, simulation: Boolean = true)
  extends Config(new U200Base(nMemoryChannels) ++ new AWS_sole(simulation))

object SLRConstants {
  final val DEFAULT_SLR = 0
}

//noinspection ScalaUnusedSymbol
object ProcessCorner extends Enumeration {
  val Typical, Fast, Slow = Value
  type ProcessCorner = Value
}

//noinspection ScalaUnusedSymbol
object ProcessTemp extends Enumeration {
  val C25, CM40, C125 = Value
  type ProcessTemp = Value
}

//noinspection ScalaUnusedSymbol
object ProcessVoltageThreshold extends Enumeration {
  val High, Regular, Low, SuperLow = Value
  type ProcessVoltageThreshold = Value
}

//noinspection ScalaUnusedSymbol
object ProcessOperatingConditions extends Enumeration {
  type ProcessOperatingConditions = Value
  val NormalVoltage, LowVoltage = Value
}

case object BuildSynthesisKey extends Field[Boolean]


class WithChipKitPlatform(corner: ProcessCorner = ProcessCorner.Typical,
                          temp: ProcessTemp = ProcessTemp.C25,
                          threshold: ProcessVoltageThreshold = ProcessVoltageThreshold.Regular,
                          voltage: ProcessOperatingConditions = ProcessOperatingConditions.NormalVoltage,
                          clockRateMHz: Float = 1000,
                          synthesis: Boolean = false)
  extends Config((_, _, _) => {
    case ExtMem =>
      Some(
        MemoryPortParams(
          MasterPortParams(
            base = 0,
            size = 1L << 34,
            beatBytes = 16,
            idBits = 6
          ),
          1
        )
      )
    // 4GB total physical memory
    case PlatformPhysicalMemoryBytes => 1L << 34
    case FrontBusBaseAddress => 0x2000000000L
    case FrontBusAddressMask => 0xffffL
    case HasDMA => None
    case CXbarMaxDegree => 8
    case HasDiscreteMemory => false
    case FrontBusBeatBytes => 4
    case CoreCommandLatency => 0

    case BuildSynthesisKey => synthesis
    case PlatformTypeKey => PlatformType.ASIC
    case FrontBusProtocolKey => FrontBusProtocol.AHB
    case DefaultClockRateKey => 100

    case IsAWS => false
    case PostProcessorMacro => () => {
      val cwd = os.Path(ComposerBuild.composerGenDir)
      val timestamp = LocalDateTime.now()
      val synwd = cwd / ("asic_build_" + DateTimeFormatter.ofPattern("yy-MM-dd_HHMMSS").format(timestamp))
      val synout = synwd / "out"
      val synsrc = synwd / "src"
      os.makeDir.all(synout)
      os.makeDir.all(synsrc)
      ComposerBuild.sourceList.foreach { src =>
        if (!java.nio.file.Files.isRegularFile(java.nio.file.Paths.get(src.toString()))) {
          os.copy.over(src, synsrc / src.baseName, createFolders = true)
        }
      }
      os.copy.over(cwd / "composer.v", synsrc / "composer.sv")

      val cornerString = corner match {
        case ProcessCorner.Typical => "tt"
        case ProcessCorner.Fast => "ff"
        case ProcessCorner.Slow => "ss"
      }
      val voltageString = corner match {
        case ProcessCorner.Fast => voltage match {
          case ProcessOperatingConditions.NormalVoltage => "0p88v"
          case ProcessOperatingConditions.LowVoltage => "0p7v"
        }
        case ProcessCorner.Typical => voltage match {
          case ProcessOperatingConditions.NormalVoltage => "0p8v"
          case ProcessOperatingConditions.LowVoltage => "0p65v"
        }
        case ProcessCorner.Slow => voltage match {
          case ProcessOperatingConditions.NormalVoltage => "0p72v"
          case ProcessOperatingConditions.LowVoltage => "0p65v"
        }
      }
      val tempString = temp match {
        case ProcessTemp.C25 => "25c"
        case ProcessTemp.C125 => "125c"
        case ProcessTemp.CM40 => "m40c"
      }
      val pdks = s"/usr/xtmp/cmk91/install/pdks/"
      val stdcell_suffix = threshold match {
        case ProcessVoltageThreshold.Regular => "rvt"
        case ProcessVoltageThreshold.SuperLow => "slvt"
        case _ => "I haven't downloaded this stdcell yet. Let me know if you need it"
      }
      val processPath = os.Path(s"$pdks/stdcell_$stdcell_suffix/db_nldm/saed14${stdcell_suffix}_$cornerString$voltageString$tempString.db")
      val memoryPaths = ComposerBuild.symbolicMemoryResources.map(p => p / "db_nldm" / s"saed14sram_$cornerString$voltageString$tempString.db")
      val PDK = f"$pdks/SAED14nm_PDK_12142021/SAED14_PDK/"
      os.symlink(synwd / "proc.db", processPath)
      val libs = List((synwd / "proc.db").toString()) ++ memoryPaths.distinct.zipWithIndex.map{case(p, idx) =>
        val nPath = synwd / s"mem$idx.db"
        os.symlink(synwd / s"mem$idx.db", p)
        nPath}.toList
//      val libs = (memoryPaths ++ Seq(processPath)).map(_.toString()).distinct.reduce(_ + " " + _)
      os.write(synwd / "synth.tcl",
        f"""set project_path $synwd
           |set save_path ${synout.toString}
           |
           |### tech files
           |#set synopsys_tech_tf "$PDK/techfiles/saed14nm_1p9m_mw.tf"
           |
           |### library files
           |set_app_var target_library "proc.db"
           |set_app_var link_library "* ${libs.reduce(_ + " " + _)}"
           |define_design_lib work -path "$synwd/work"
           |
           |set_host_options -max_cores 16
           |report_host_options
           |
           |analyze -format sverilog [list $synsrc/composer.sv]
           |elaborate ComposerTop
           |current_design ComposerTop
           |uniquify
           |
           |set peri [expr 1000.0 / $clockRateMHz]
           |set_wire_load_model -name "ForQA"
           |create_clock clock -name "clock" -period $$peri
           |#set_switching_activity -type {registers black_boxes memory inputs outputs inouts ports nets} -hierarchy -static_probability 0.5
           |#compile -incremental_mapping -map_effort high -area_effort low -power_effort low -auto_ungroup delay
           |set_cost_priority -delay
           |compile_ultra -retime -scan
           |optimize_registers -delay_threshold=$$peri -minimum_period_only
           |check_design
           |
           |write -f verilog -output $synout/dc_netlist.v
           |report_cell > $synout/cell_usage.txt
           |report_area -hierarchy > $synout/area.txt
           |report_timing -nworst 32 > $synout/timing.txt
           |report_net_fanout > $synout/fanout.txt
           |report_power -cell -hierarchy -analysis_effort high > $synout/power.txt""".stripMargin
      )
    }
    case HasDisjointMemoryControllers => false

  })
