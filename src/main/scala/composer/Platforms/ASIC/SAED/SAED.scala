package composer.Platforms.ASIC.SAED

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3.experimental.BaseModule
import composer.MemoryStreams.{HasMemoryInterface, SD}
import composer._
import composer.Generation.ComposerBuild
import composer.MemoryStreams.RAM.RegMem
import composer.Platforms.ASIC._
import composer.Platforms._
import composer.Platforms.ASIC.ProcessCorner.ProcessCorner
import composer.Platforms.ASIC.ProcessOperatingConditions.ProcessOperatingConditions
import composer.Platforms.ASIC.ProcessTemp.ProcessTemp
import composer.Platforms.ASIC.ProcessVoltageThreshold.ProcessVoltageThreshold
import os.Path

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

class WithSAED(corner: ProcessCorner = ProcessCorner.Typical,
               temp: ProcessTemp = ProcessTemp.C25,
               threshold: ProcessVoltageThreshold = ProcessVoltageThreshold.Regular,
               voltage: ProcessOperatingConditions = ProcessOperatingConditions.NormalVoltage,
               clockRateMHz: Float = 1000) extends Config((_, _, _) => {
  case ASICMemoryCompilerKey => new MemoryCompiler {
    override def isTooSmall(nRows: Int, nCols: Int): Boolean = false
    override val mems: Map[Int, Seq[SD]] = Map.from(Seq(
      (1, Seq(
        SD(8, 128), SD(32, 64), SD(32, 256), SD(8, 1024), SD(8, 512), SD(48, 128),
        SD(50, 32), SD(34, 64), SD(46, 256), SD(46, 128), SD(8, 64), SD(128, 64),
        SD(128, 256), SD(8, 256), SD(48, 256), SD(128, 512), SD(32, 512))),
      (2, Seq(SD(4, 16), SD(4, 32), SD(4, 64), SD(8, 16), SD(8, 32), SD(8, 64),
        SD(8, 128), SD(16, 16), SD(16, 32), SD(16, 64), SD(16, 128), SD(32, 16),
        SD(32, 64), SD(32, 128), SD(22, 32), SD(39, 32)))))
    override val isActiveHighSignals: Boolean = false // active low

    override def generateMemoryFactory(nPorts: Int, nRows: Int, nColumns: Int)(implicit p: Parameters): () => BaseModule with HasMemoryInterface = {
      def makeMemory(): BaseModule with HasMemoryInterface = {
        if (nPorts == 1) {
          new SAED_1RW_SRAM(nRows, nColumns)
        } else if (nPorts == 2) {
          new SAED_2RW_SRAM(nRows, nColumns)
        } else {
          new RegMem(nRows, nColumns, nPorts, 1)
        }
      }
      makeMemory
    }

    override def getMemoryName(nPorts: Int, nRows: Int, nColumns: Int): String = {
      f"SRAM${nPorts}RW${nRows}x$nColumns"
    }
  }
  case PostProcessorMacro => (_: Parameters) => {
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
    val baseMemories = f"/usr/xtmp/cmk91/install/pdks/stdcell_memories/lib/sram"
    val basePDK = "/usr/xtmp/cmk91/install/pdks/SAED14PDK/SAED14_PDK/"
    val processPath = f"$baseStdCell/db_ccs/saed14${stdcell_suffix}_$cornerString$voltageString$tempString.db"
    //      val memoryPaths: Seq[Path] = ComposerBuild.symbolicMemoryResources.distinct.map(p => p / "db_nldm" / s"saed14sram_$cornerString$voltageString$tempString.db")
    //    val memoryDBs = List("dual", "single").map(p => f"$baseMemories/logic_synth/$p/saed14sram_$cornerString$voltageString$tempString.db").fold("")(_ + " " + _)
    val memoryDBs = List("dual").map(p => f"$baseMemories/logic_synth/$p/saed14sram_$cornerString$voltageString$tempString.db").fold("")(_ + " " + _)
    val mem_pnrs = List("1rw", "2rw").map { mem =>
      f"$baseMemories/ndm/saed14_sram_${mem}_frame_only.ndm"
    }.fold("")(_ + " " + _)

    os.write(synwd / "synth.ssp",
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

    val itf_map = f"$basePDK/starrc/saed14nm_tf_itf_tluplus.map"
    val cmax = f"$basePDK/starrc/max/saed14nm_1p9m_Cmax.tluplus"
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
         |
         |create_lib -technology $tech_file c1.nlib
         |
         |set_ref_libs -library c1.nlib -ref_libs {  $baseStdCell/lef/saed14nm_${stdcell_suffix}_1p9m.lef  $baseStdCell/ndm/saed14${stdcell_suffix}_frame_only.ndm $baseMemories/lef/saed14_sram_2rw.lef $baseMemories/ndm/saed14_sram_2rw_frame_only.ndm }
         |
         |derive_design_level_via_regions
         |
         |
         |read_verilog -library c1.nlib -design ComposerTop -top ComposerTop out/composer.netlist.v
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

})
