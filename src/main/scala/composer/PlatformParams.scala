package composer

import chipsalliance.rocketchip.config._
import composer.FrontBusProtocol.FrontBusProtocol
import composer.PlatformType.PlatformType
import composer.ProcessCorner.ProcessCorner
import composer.ProcessOperatingConditions.ProcessOperatingConditions
import composer.ProcessTemp.ProcessTemp
import composer.ProcessVoltageThreshold.ProcessVoltageThreshold
import freechips.rocketchip.subsystem._
import os.Path

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
      os.makeDir.all(synwd / "src")
      os.makeDir.all(synwd / "out")
      ComposerBuild.sourceList.foreach { src =>
        if (!java.nio.file.Files.isRegularFile(java.nio.file.Paths.get(src.toString()))) {
          os.copy.over(src, synwd / "src" / src.baseName, createFolders = true)
        }
      }
      os.copy.over(cwd / "composer.v", synwd / "src" / "composer.sv")

      def getFileWithEndingInDir(dir: Path, suffix: String): Path = {
        os.walk(dir).filter(_.toString().contains(suffix))(0)
      }
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
      val stdcell_suffix = threshold match {
        case ProcessVoltageThreshold.Regular => "rvt"
        case ProcessVoltageThreshold.SuperLow => "slvt"
        case _ => "I haven't downloaded this stdcell yet. Let me know if you need it"
      }
      val tech_file = "/usr/xtmp/cmk91/install/pdks/tech/milkyway/saed14nm_1p9m_mw.tf"
      val baseStdCell = f"/usr/xtmp/cmk91/install/pdks/stdcell_$stdcell_suffix"
      val processPath = f"$baseStdCell/db_nldm/saed14${stdcell_suffix}_$cornerString$voltageString$tempString.db"
//      val memoryPaths: Seq[Path] = ComposerBuild.symbolicMemoryResources.distinct.map(p => p / "db_nldm" / s"saed14sram_$cornerString$voltageString$tempString.db")
      val memoryRoots = ComposerBuild.symbolicMemoryResources.distinct
      val memoryDBs = memoryRoots.map(p => (p / "db_nldm" / f"${p.baseName}_$cornerString$voltageString$tempString.db").toString()).fold("")(_ + " " + _)
      val memoryLefs = memoryRoots.map(p => (p / (p.baseName + ".lef")).toString()).fold("")(_ + " " + _)
      val memoryNDMs = memoryRoots.map(p => (p / "ndm" / (p.baseName + "_frame_only.ndm")).toString()).fold("")(_ + " " + _)

      os.write(synwd / "synth.tcl",
        f"""set project_path $synwd
           |set save_path ./out
           |### tech files
           |
           |### library files
           |set_app_var target_library "$processPath"
           |set_app_var link_library "* $processPath $memoryDBs"
           |define_design_lib work -path "$synwd/work"
           |
           |# suppress annoying messages
           |suppress_message [list LINT-33 VER-318 OPT-1207 OPT-776 OPT-1206 OPT-777 OPT-1215]
           |
           |set_host_options -max_cores 16
           |report_host_options
           |
           |analyze -format sverilog [list src/composer.sv]
           |elaborate ComposerTop
           |current_design ComposerTop
           |link
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
           |link
           |check_design
           |
           |write -f verilog -output out/dc_netlist.v
           |report_cell > out/cell_usage.txt
           |report_area -hierarchy > out/area.txt
           |report_timing -nworst 32 > out/timing.txt
           |report_net_fanout > out/fanout.txt
           |report_power -cell -hierarchy -analysis_effort high > out/power.txt
           |
           |write_sdc composer.sdc
           |write -hierarchy -format verilog -output out/composer.netlist.v""".stripMargin

      )

//      val refLefs = ComposerBuild.symbolicMemoryResources.distinct.map(p => getFileWithEndingInDir(p, ".lef"))
      val mem_pnrs: Seq[String] = memoryRoots.map { mem =>
          getFileWithEndingInDir(mem / "ndm", "_frame_only.ndm").toString()
      }
//      val pnr_lib_string = (tech_pnr ++ refLefs).map(_.toString()).reduce(_ + " " + _)

//      val mw_lib = os.Path(PDK) / "techfiles" / "saed14nm_1p9m_mw.tf"
//
      val itf_map =  "starrc/saed14nm_tf_itf_tluplus.map"
      val cmax = "starrc/max/saed14nm_1p9m_Cmax.tluplus"
//      val starrc_libs = List(
//        cmax,
//        os.Path(PDK) / "starrc" / "min" / "saed14nm_1p9m_Cmin.tluplus",
//        os.Path(PDK) / "starrc" / "nominal" / "saed14nm_1p9m_nominal.tluplus",
//        itf_map
//      )
      val tempInt = temp match {
        case ProcessTemp.C25 => 25
        case ProcessTemp.CM40 => -40
        case ProcessTemp.C125 => 125
      }
      val periodGHz = 1000.0 / clockRateMHz
      val processSpeedStr = corner match {
        case ProcessCorner.Fast => "fast"
        case ProcessCorner.Typical => "typical"
        case ProcessCorner.Slow => "slow"
      }
      // drop 0p, drop v, divide by 100 to get decimal back
      val voltageFloat = voltageString.substring(2).dropRight(1).toFloat / 100
//      val referenceLibs = ComposerBuild.symbolicMemoryResources.map(p => p/)
      os.write(synwd / "pnr.tcl",
        f"""set top_module ComposerTop
           |# set design_verilog "src/composer.v"
           |set LIBRARY_CONFIGURATION_FLOW true
           |set_app_var link_library "* $processPath $memoryDBs"
           |create_lib -technology $tech_file -ref_libs [ list $baseStdCell/ndm/saed14${stdcell_suffix}_frame_timing_ccs.ndm $memoryLefs ] composer_layout.nlib
           |read_verilog -library composer_layout.nlib -design ComposerTop -top ComposerTop out/composer.netlist.v
           |
           |### Initialize Scenarios
           |remove_scenarios -all
           |remove_corners -all
           |remove_modes -all
           |
           |if { [get_modes func -quiet] == "" } {
           |    create_mode func
           |}
           |if { [get_corner $cornerString$voltageString${tempString}_cmax -quiet] == "" } {
           |    create_corner $cornerString$voltageString${tempString}_cmax
           |}
           |create_scenario -mode func -corner $cornerString$voltageString${tempString}_cmax -name func_$cornerString$voltageString${tempString}_cmax
           |current_scenario func_$cornerString$voltageString${tempString}_cmax
           |read_parasitic_tech -layermap $itf_map -tlup $cmax -name maxTLU
           |remove_sdc -scenarios [current_scenario]
           |
           |### Clock Settings
           |#create_clock -period $periodGHz -name clock [get_ports clock]
           |# set_clock_groups -name func_async -group [get_clocks clock]
           |source composer.sdc
           |
           |### Voltage Settings
           |set_parasitic_parameters -early_spec maxTLU -late_spec maxTLU
           |set_temperature $tempInt
           |set_process_number 0.99
           |set_process_label $processSpeedStr
           |set_voltage $voltageFloat  -object_list VDD
           |set_voltage 0.00  -object_list VSS
           |
           |### Timing Model
           |set_timing_derate -early 0.93 -cell_delay -net_delay
           |set clock uncertainty
           |set_max_transition 0.15 [get_clock clock] -clock_path
           |set_max_transition 0.25 [get_clock clock] -data_path
           |set_max_capacitance 150 [current_design]
           |
           |### Finalize Scenarios
           |set_scenario_status func_$cornerString$voltageString${tempString}_cmax -active true -setup true -hold true -max_capacitance true -max_transition true -min_capacitance true -leakage_power false -dynamic_power false
           |
           |### Floorplan
           |initialize_floorplan -honor_pad_limit
           |connect_pg_net -automatic -all_blocks
           |
           |####################################
           |### Place IO
           |######################################
           |place_io
           |
           |####################################
           |### Memory Placement
           |######################################
           |if [sizeof_collection [get_cells -hier -filter is_hard_macro==true -quiet]] {
           |   set all_macros [get_cells -hier -filter is_hard_macro==true]
           |   # Check top-level
           |   report_macro_constraints -allowed_orientations -preferred_location -alignment_grid -align_pins_to_tracks $$all_macros > out/report_macro_constraints.rpt
           |}
           |
           |### Placement
           |set_app_options -name place.coarse.continue_on_missing_scandef -value true
           |place_opt
           |
           |### Clock Tree Synthesis
           |clock_opt -from build_clock -to remote_clock
           |clock_opt -from final_opto -to final_opto
           |
           |### Route
           |route_auto
           |route_opt
           |
           |### Power Networks
           |connect_pg_net -automatic
           |
           |### Report Result
           |report_qor > out/pnr_qor.rpt
           |
           |### Save Block and Export GDS
           |save_block
           |write_gds out/composer.gds
           |
           |""".stripMargin)
    }
    case HasDisjointMemoryControllers => false

  })
