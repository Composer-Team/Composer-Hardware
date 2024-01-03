package composer.Platforms.ASIC.ASAP7

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.experimental.BaseModule
import composer.Generation.Annotators.{UniqueMv, WalkPath}
import composer.Generation._
import composer.MemoryStreams.{SD, _}
import composer.MemoryStreams.RAM.SyncReadMemMem
import composer.Platforms._
import composer.Platforms.ASIC._
import composer.Platforms.ASIC.ProcessCorner.ProcessCorner
import composer.Platforms.ASIC.ProcessTemp.ProcessTemp
import composer.Platforms.ASIC.ProcessVoltageThreshold.ProcessVoltageThreshold
import os.Path

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

class ASAP7MemoryCompiler extends MemoryCompiler with CannotCompileMemories {
  override val isActiveHighSignals: Boolean = true
  override val supportedCorners: Seq[(ProcessCorner, ProcessTemp)] = Seq.empty

  override val mostPortsSupported: Int = 1
  override val availableMems: Map[Int, Seq[SD]] = Map.from(Seq((1, Seq(
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
        val mem = Module(new ASAP7SRAM(nRows, nColumns))
        mem.io.sdel := 0.U
        mem
      } else {
        Module(new SyncReadMemMem(0, 0, nPorts, nRows, nColumns, 1))
      }
    }

    makeMemory
  }

  def getMemoryName(nPorts: Int, nRows: Int, nColumns: Int): String = {
    require(nPorts == 1)
    f"srambank_${nRows / 4}x4x${nColumns}_6t122"
  }

}

class WithASAP7(corner: ProcessCorner = ProcessCorner.Typical,
                threshold: ProcessVoltageThreshold = ProcessVoltageThreshold.Regular,
                clockRateMHz: Float = 1000) extends Config((_, _, _) => {
  case ASICMemoryCompilerKey => new ASAP7MemoryCompiler
  case PostProcessorMacro => c: Config => {
    if (c(BuildModeKey) == BuildMode.Synthesis) {
      val cwd = os.Path(ComposerBuild.composerGenDir)
      val timestamp = LocalDateTime.now()
      val synwd = cwd / ("asic_build_" + DateTimeFormatter.ofPattern("yy-MM-dd_HHMMSS").format(timestamp))
      Seq("src", "syn_out", "imp_out") foreach (p => os.makeDir.all(synwd / p))

      os.copy.over(ComposerBuild.targetDir, synwd / "src")
      val allSynPaths = WalkPath(synwd / "src", depth=0).map(_.toString()).toList

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

      val nonTopSources = allSynPaths.filter { path =>
        val tmod = path.split("/").last.split("\\.").head
        !ComposerBuild.partitionModules.contains(tmod)
      }
      val tech_file = "/data/install/pdks/asap7/asap7_snps/icc/asap07_icc.tf"
      val stdCellDBs = List("SIMPLE", "INVBUF", "AO", "SEQ").map { p =>
        os.Path(f"/data/install/pdks/asap7/asap7sc7p5t_28/LIB/NLDM/" +
          f"asap7sc7p5t_${p}_${stdcell_suffix}_${cornerString}_nldm.db").toString()
      }.fold("")(_ + " " + _)
      val stdCellLEFBase = "/data/install/pdks/asap7/asap7sc7p5t_28/LEF"

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
      val mem_lefs_scaled = os.walk(os.Path(baseMemoryLEFs) / "4xLEF", maxDepth = 1).filter { mem =>
        mem.toString().contains(".lef")
      }.map(_.toString()).fold("")(_ + " " + _)


      ComposerBuild.partitionModules.distinct foreach { pMod =>
        val script_dir = synwd / "syn" / f"${pMod}_scripts"
        val save_path = synwd / "syn_out" / pMod
        val work_dir = script_dir / "work"
        val srcList = {
          val fileEnding = if (os.exists(synwd / "src" / f"$pMod.v")) ".v" else ".sv"
          nonTopSources :+ (synwd/"src"/ f"$pMod$fileEnding").toString()
        }.map{p =>
          println(p)
          os.Path(p).relativeTo(synwd / "src")}

        Seq(script_dir, save_path, work_dir).foreach(p => os.makeDir.all(p))

        os.write(script_dir / "1_setup.tcl",
          f"""set project_path $synwd
             |set save_path ${save_path.relativeTo(script_dir)}
             |set search_path "$$search_path ${(synwd / "src").relativeTo(script_dir)}"
             |### tech files
             |
             |### library files
             |set_app_var target_library {$stdCellDBs}
             |set_app_var link_library "* $memoryDBs $stdCellDBs "
             |define_design_lib work -path "$work_dir"
             |
             |# suppress annoying messages
             |suppress_message [list LINT-33 VER-318 OPT-1207 OPT-776 OPT-1206 OPT-777 OPT-1215]
             |
             |set_host_options -max_cores 16
             |report_host_options
             |
             |analyze -format sverilog [list ${srcList.mkString(" ")}]
             |elaborate $pMod
             |current_design $pMod
             |link
             |set peri 1000.0
             |create_clock clock -name "clock" -period $$peri
             |set_clock_uncertainty 100 [get_clocks clock]
             |set_cost_priority -delay
             |set_input_delay -min -max -rise -fall -clock clock 110 [get_ports -filter "direction==in"]
             |set_output_delay -min -max -rise -fall -clock clock 110 [get_ports -filter "direction==out"]
             |uniquify""".stripMargin)
        os.write(script_dir / "2_compile.tcl",
          f"""
             |compile_ultra -retime -no_autoungroup
             |# enable this if timing is really tight
             |# optimize_registers -delay_threshold=$$peri -minimum_period_only"""
        .stripMargin)
        os.write(script_dir / "3_save.tcl",
          f"""link
             |check_design
             |
             |# Perform the following command if no violations
             |write -hierarchy -format verilog -output $save_path/netlist.v""".
            stripMargin
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

        // create macro arrays
        //    val macroArrays = MemoryCompiler.registeredMemoryArrays.map { ar =>
        //      s"create_macro_array -fill_pattern by_row -horizontal_channel_height 10 -vertical_channel_height 10 -num_rows ${ar.rows} -num_cols ${ar.cols} -create_group true -name_edit_group ${ar.name} ${ar.cells reduce (_ + " " + _)}"
        //    }
        //        os.write(synwd / "pnr.tcl",
        //          f"""set top_module ComposerTop
        //             |# set design_verilog "src/composer.v"
        //             |set LIBRARY_CONFIGURATION_FLOW true
        //             |set_app_var link_library "* $stdCellDBs $memoryDBs"
        //             |
        //             |create_lib -scale_factor 4000 -technology $tech_file c1.nlib
        //             |
        //             |set_ref_libs -library c1.nlib -ref_libs { $mem_lefs_scaled $stdCellLEFs_scaled}
        //             |
        //             |derive_design_level_via_regions
        //             |
        //             |
        //             |read_verilog -library c1.nlib -design ComposerTop -top ComposerTop out/netlist.v
        //             |
        //             |### Initialize Scenarios
        //             |remove_scenarios -all
        //             |remove_corners -all
        //             |remove_modes -all
        //             |
        //             |if { [get_modes func -quiet] == "" } {
        //             |    create_mode func
        //             |}
        //             |if { [get_corner ${cornerString}_cmax -quiet] == "" } {
        //             |    create_corner ${cornerString}_cmax
        //             |}
        //             |create_scenario -mode func -corner ${cornerString}_cmax -name func_${cornerString}_cmax
        //             |current_scenario func_${cornerString}_cmax
        //             |read_parasitic_tech -layermap $layermap -tlup $tluplus -name maxTLU
        //             |remove_sdc -scenarios [current_scenario]
        //             |
        //             |### Clock Settings
        //             |create_clock -period ${periodGHz * 1000} -name clock [get_ports clock]
        //             |
        //             |### Voltage Settings
        //             |set_parasitic_parameters -early_spec maxTLU -late_spec maxTLU
        //             |set_temperature 25
        //             |set_process_number 0.99
        //             |set_process_label $processSpeedStr
        //             |set_voltage 0.7 -object_list VDD
        //             |set_voltage 0.00  -object_list VSS
        //             |
        //             |### Timing Model
        //             |set_timing_derate -early 0.93 -cell_delay -net_delay
        //             |set_clock_uncertainty 100 [get_clocks clock]
        //             |set_max_transition 150 [get_clock clock] -clock_path
        //             |set_max_transition 200 [get_clock clock] -data_path
        //             |set_max_capacitance 150 [current_design]
        //             |
        //             |### Finalize Scenarios
        //             |set_scenario_status func_${cornerString}_cmax -active true -setup true -hold true -max_capacitance true -max_transition true -min_capacitance true -leakage_power false -dynamic_power false
        //             |
        //             |### Floorplan
        //             |initialize_floorplan
        //             |
        //             |set_app_options -name plan.macro.style -value freeform
        //             |set_app_options -name plan.macro.integrated -value true
        //             |set_attribute [get_flat_cells -filter is_hard_macro] -name physical_status -value unplaced
        //             |set_host_options -max_cores 48
        //             |
        //             |# Place output pins
        //             |# set_app_options -name plan.pins.incremental -value false -block [current_block]
        //             |# place_pins -self -ports *
        //             |
        //             |# Place design cells
        //             |# set_app_options -name place.coarse.fix_hard_macros -value false
        //             |# set_app_options -name plan.place.auto_create_blockages -value auto
        //             |# create_placement -floorplan -effort high
        //             |
        //             |# optimize placement
        //             |# place_opt
        //             |""".stripMargin)
      }
    }
  }
})