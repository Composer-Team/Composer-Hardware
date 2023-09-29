package composer.Platforms.FPGA.Xilinx

import os.Path
import org.fusesource.scalate._
import org.fusesource.scalate.util.FileResourceLoader
import org.fusesource.scalate.util._

object SynthScript {
  def apply(projectName: String,
            outputDir: String,
            fpga_part_name: String,
            board_part: String,
            clock_rate: String,
            verilog_file: String = "composer.v",
            top_module: String = "ComposerTop"): String = {
    System.err.println("INFO: You may see a warning about fail to load a logger class for SLF4J. " +
      "This is expected behavior. Ignore.")
    val engine = new TemplateEngine
    engine.resourceLoader = new FileResourceLoader {
      override def resource(uri: String): Option[TemplateSource] = {
        val text = os.read(os.resource / "composer" / "FPGA" / uri)
        Some(TemplateSource.fromText(uri, text))
      }
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