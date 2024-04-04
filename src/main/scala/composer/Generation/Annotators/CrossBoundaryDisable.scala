package composer.Generation.Annotators

import os.Path

object CrossBoundaryDisable {
  def apply(disableList: Seq[String], targetDir: Path): Unit = {
    // read file on targetDir / "ComposerTop.v" and replace module names in disableList with (* keep_hierarchy = "yes" *)
    val start_time = System.currentTimeMillis()
    //    val composerTop = os.read(targetDir / "ComposerTop.v")
    //    val newComposerTop = disableList.foldLeft(composerTop) { (acc, module) =>
    //      acc.replaceAll(s"module $module", s"(* keep_hierarchy = \"yes\" *) module $module")
    //    }
    //    os.write.over(targetDir / "ComposerTop.v", newComposerTop)
    // for each file, go through and look for module instances called $name
    // go through and replace <module_name> $name with (* keep_hierarchy = "yes" *) <module_name> $name
    // make sure to capture the module name and repeat it
    val files = os.walk(targetDir).filter(_.last.endsWith(".v"))
    disableList foreach { mname =>
      var found = false
      files.foreach { file =>
        // go through each line and look for module instances
        val lines = os.read.lines(file)
        // open file writer
        os.write.over(file,
          lines.map(line => {
            if (line.contains(mname + " (")) {
              found = true
              s"(* keep_hierarchy = \"yes\" *) $line"
            } else {
              line
            }
          }).mkString("\n")
        )
      }
      if (!found)
        println("Failed to find module " + mname) // if we didn't find the module, print a warning
    }
    println(s"CrossBoundaryDisable took ${System.currentTimeMillis() - start_time} ms")

  }

}
