package composer.Generation.Annotators

import os.Path

import java.util.regex.Pattern
import java.nio.file

object Uniqueify {
  def apply(sourceList: Seq[Path], targetDir: Path, gsrc_dir: Path): Unit = {

    // first, get module names in targetDir / "ComposerTop.v"
    val pattern = Pattern.compile("module (.*) *\\(")
    val matcher = pattern.matcher(os.read(targetDir / "ComposerTop.v"))
    var existingModules = Seq.empty[String]
    while (matcher.find()) {
      existingModules = existingModules :+ matcher.group(1)
    }

    sourceList.distinct foreach { src =>
      val srcFileName = src.segments.toSeq.last
      if (file.Files.isRegularFile(java.nio.file.Paths.get(src.toString()))) {
        if (srcFileName.endsWith(".v")) {
          val matcher = pattern.matcher(os.read(src))
          var srcModules = Seq.empty[String]
          while (matcher.find()) {
            srcModules = srcModules :+ matcher.group(1)
          }
          // get a list of repeated modules
          val repeats = srcModules.filter(existingModules.contains(_)).distinct
          // for each create a sed command to remove the module
          //        println("In " + src.toString() + " repeats are " + repeats)
          if (repeats.nonEmpty) {
            val sedCmds = repeats.map { mod: String => s"/module ${addEscapes(mod)}/,/endmodule/d" }
            // then, remove the repeated modules from the source
            // join sedCMds with ;
            val sedCmd = Seq("sed", "-E", sedCmds.mkString(";"), src.toString)
            //          println("sedCmd is " + sedCmd)

            os.proc(sedCmd).call(stdout = os.pwd / "tmp.v")
            os.write.append(targetDir / "ComposerTop.v", os.read(os.pwd / "tmp.v"))
            os.remove(os.pwd / "tmp.v")
          } else {
            os.write.append(targetDir / "ComposerTop.v", os.read(src))
          }
          existingModules = existingModules ++ srcModules
        } else {
          // not a verilog source, just copy it to the source directory
          os.copy.over(src, gsrc_dir / srcFileName)
        }
      } else {
        os.copy.over(src, gsrc_dir / srcFileName)
      }
    }

  }
}
