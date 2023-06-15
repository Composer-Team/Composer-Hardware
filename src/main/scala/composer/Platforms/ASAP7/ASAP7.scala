package composer.Platforms.ASAP7

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.experimental.BaseModule
import composer.MemoryStreams.RAM.{ASAP7_SP_SRAM, MemoryCompiler, RegMem}
import composer.MemoryStreams.{HasMemoryInterface, SD}
import composer.ProcessCorner.ProcessCorner
import composer._
import composer.ProcessVoltageThreshold.ProcessVoltageThreshold
import os.Path

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

class WithASAP7(corner: ProcessCorner = ProcessCorner.Typical,
                threshold: ProcessVoltageThreshold = ProcessVoltageThreshold.Regular,
                clockRateMHz: Float = 1000) extends Config((_, _, _) => {
  case ASICMemoryCompilerKey => new MemoryCompiler {
    override val isActiveHighSignals: Boolean = true
    override val mems: Map[Int, Seq[SD]] = Map.from(Seq((1, Seq(
      SD(16, 512), SD(18, 512), SD(20, 512), SD(32, 512), SD(34, 512),
      SD(36, 512), SD(40, 512), SD(48, 512), SD(64, 512), SD(72, 512),
      SD(74, 512), SD(80, 512),
      SD(16, 1024), SD(18, 1024), SD(20, 1024), SD(32, 1024), SD(34, 1024),
      SD(36, 1024), SD(40, 1024), SD(48, 1024), SD(64, 1024), SD(72, 1024),
      SD(74, 1024), SD(80, 1024),
      SD(16, 256), SD(18, 256), SD(20, 256), SD(32, 256), SD(34, 256),
      SD(36, 256), SD(40, 256), SD(48, 256), SD(64, 256), SD(72, 256),
      SD(74, 256), SD(80, 256),
    ))))

    override def generateMemoryFactory(nPorts: Int, nRows: Int, nColumns: Int)(implicit p: Parameters): () => BaseModule with HasMemoryInterface = {
      def makeMemory(): BaseModule with HasMemoryInterface = {
        if (nPorts == 1) {
          val mem = Module(new ASAP7_SP_SRAM(nRows, nColumns))
          mem.io.sdel := 0.U
          mem
        } else {
          Module(new RegMem(nRows, nColumns, nPorts))
        }
      }
      makeMemory
    }

    override def getMemoryName(nPorts: Int, nRows: Int, nColumns: Int): String = {
      require(nPorts == 1)
      f"srambank_${nRows / 4}x4x${nColumns}_6t122"
    }
  }
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

    def getFileWithNameApprox(dir: Path, contents: Seq[String]): Path = {
      os.walk(dir).filter(d => contents.map(c => d.toString().contains(c)).reduce(_ && _))(0)
    }

    val cornerString = corner match {
      case ProcessCorner.Typical => "TT"
      case ProcessCorner.Fast => "FF"
      case ProcessCorner.Slow => "SS"
    }
    val stdcell_suffix = threshold match {
      case ProcessVoltageThreshold.Regular => "RVT"
      case ProcessVoltageThreshold.Low => "LVT"
      case ProcessVoltageThreshold.SuperLow => "SLVT"
      case ProcessVoltageThreshold.High => "HVT"
    }
    val tech_file = "/data/install/pdks/asap7/asap7_snps/icc/asap07_icc.tf"
    val stdCellDBs = List("SIMPLE", "INVBUF", "AO", "SEQ").map { p =>
      getFileWithNameApprox(os.Path("/data/install/pdks/asap7/asap7sc7p5t_28/LIB/NLDM/"),
      Seq(f"asap7sc7p5t_${p}_${stdcell_suffix}_${cornerString}_", ".db")).toString()
    }.fold("")(_ + " " + _)
    val stdCellLEFBase = "/data/install/pdks/asap7/asap7sc7p5t_28/LEF"
    val stdCellLEFs = List(
      f"$stdCellLEFBase/asap7sc7p5t_28_L_1x_220121a.lef",
      f"$stdCellLEFBase/asap7sc7p5t_28_R_1x_220121a.lef",
      f"$stdCellLEFBase/asap7sc7p5t_28_SL_1x_220121a.lef",
      f"$stdCellLEFBase/asap7sc7p5t_28_SRAM_1x_220121a.lef"
    ).fold("")(_ + " " + _)

    val stdCellLEFs_scaled = List(
      f"$stdCellLEFBase/scaled/asap7sc7p5t_28_L_4x_220121a.lef",
      f"$stdCellLEFBase/scaled/asap7sc7p5t_28_R_4x_220121a.lef",
      f"$stdCellLEFBase/scaled/asap7sc7p5t_28_SL_4x_220121a.lef",
      f"$stdCellLEFBase/scaled/asap7sc7p5t_28_SRAM_4x_220121a.lef"
    ).fold("")(_ + " " + _)


    val baseMemoryDBs = "/data/install/pdks/asap7/ASAP7_SRAM_0p0/generated/LIB"
    val baseMemoryLEFs = "/data/install/pdks/asap7/ASAP7_SRAM_0p0/generated/LEF"

    val memoryDBs = os.walk(os.Path(baseMemoryDBs), maxDepth = 1).filter { p =>
      p.toString().contains(".db")
    }.map(_.toString()).fold("")(_ + " " + _)
    val mem_lefs = os.walk(os.Path(baseMemoryLEFs), maxDepth = 1).filter { mem =>
      mem.toString().contains(".lef")
    }.map(_.toString()).fold("")(_ + " " + _)
    val mem_lefs_scaled = os.walk(os.Path(baseMemoryLEFs) / "4xLEF", maxDepth = 1).filter { mem =>
      mem.toString().contains(".lef")
    }.map(_.toString()).fold("")(_ + " " + _)


    os.write(synwd / "synth.tcl",
      f"""set project_path $synwd
         |set save_path ./out
         |### tech files
         |
         |### library files
         |set_app_var target_library {$stdCellDBs}
         |set_app_var link_library "* $memoryDBs $stdCellDBs "
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
         |# 1/s^-6 -> s^12
         |set peri [expr 1000.0 / ($clockRateMHz / 1000.0)]
         |set_wire_load_model -name "ForQA"
         |create_clock clock -name "clock" -period $$peri
         |set_cost_priority -delay
         |compile_ultra -retime -scan
         |# enable this if timing is really tight
         |# optimize_registers -delay_threshold=$$peri -minimum_period_only
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

    val starrcDir = "/data/install/pdks/asap7/asap7_snps/starrc"
    val layermap = f"$starrcDir/asap07.layermap"
    val tluplus = f"$starrcDir/asap07.tluplus"
    //      val starrc_libs = List(
    //        cmax,
    //        os.Path(PDK) / "starrc" / "min" / "saed14nm_1p9m_Cmin.tluplus",
    //        os.Path(PDK) / "starrc" / "nominal" / "saed14nm_1p9m_nominal.tluplus",
    //        itf_map
    //      )
    val periodGHz = 1000.0 / clockRateMHz
    val processSpeedStr = corner match {
      case ProcessCorner.Fast => "fast"
      case ProcessCorner.Typical => "typical"
      case ProcessCorner.Slow => "slow"
    }
    // drop 0p, drop v, divide by 100 to get decimal back
    //      val referenceLibs = ComposerBuild.symbolicMemoryResources.map(p => p/)
    os.write(synwd / "pnr.tcl",
      f"""set top_module ComposerTop
         |# set design_verilog "src/composer.v"
         |set LIBRARY_CONFIGURATION_FLOW true
         |set_app_var link_library "* $stdCellDBs $memoryDBs"
         |
         |create_lib -scale_factor 4000 -technology $tech_file c1.nlib
         |
         |set_ref_libs -library c1.nlib -ref_libs { $mem_lefs_scaled $stdCellLEFs_scaled}
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
         |if { [get_corner ${cornerString}_cmax -quiet] == "" } {
         |    create_corner ${cornerString}_cmax
         |}
         |create_scenario -mode func -corner ${cornerString}_cmax -name func_${cornerString}_cmax
         |current_scenario func_${cornerString}_cmax
         |read_parasitic_tech -layermap $layermap -tlup $tluplus -name maxTLU
         |remove_sdc -scenarios [current_scenario]
         |
         |### Clock Settings
         |#create_clock -period $periodGHz -name clock [get_ports clock]
         |# set_clock_groups -name func_async -group [get_clocks clock]
         |source composer.sdc
         |
         |### Voltage Settings
         |set_parasitic_parameters -early_spec maxTLU -late_spec maxTLU
         |set_temperature 25
         |set_process_number 0.99
         |set_process_label $processSpeedStr
         |set_voltage 0.7 -object_list VDD
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
         |set_scenario_status func_${cornerString}_cmax -active true -setup true -hold true -max_capacitance true -max_transition true -min_capacitance true -leakage_power false -dynamic_power false
         |
         |### Floorplan
         |initialize_floorplan
         |
         |# Place output pins
         |set_app_options -name plan.pins.incremental -value false -block [current_block]
         |place_pins -self -ports *
         |
         |# Place design cells
         |set_app_options -name place.coarse.fix_hard_macros -value false
         |set_app_options -name plan.place.auto_create_blockages -value auto
         |create_placement -floorplan -effort high
         |
         |# optimize placement
         |place_opt
         |
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