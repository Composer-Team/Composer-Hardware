package composer.Platforms.FPGA.Xilinx.Templates

import org.fusesource.scalate._

object SynthScript {
  def apply(projectName: String,
            outputDir: String,
            fpga_part_name: String,
            board_part: String,
            clock_rate: String,
            verilog_file: String = "composer.v",
            top_module: String = "ComposerTop"): String = {
    val engine = new TemplateEngine
    engine.resourceLoader = (uri: String) => {
      val text = os.read(os.resource / "composer" / "FPGA" / uri)
      Some(TemplateSource.fromText(uri, text))
    }
    val environment = Map(
      "project_name" -> projectName,
      "output_dir" -> outputDir,
      "part_name" -> fpga_part_name,
      "board_part" -> board_part,
      "verilog_file" -> verilog_file,
      "top_module" -> top_module,
      "clock_rate" -> clock_rate
    )
    val setup = engine.layout("KriaSetup.ssp", environment)
    val run = engine.layout("synth.ssp", environment)
    setup + "\n" + run
  }
}