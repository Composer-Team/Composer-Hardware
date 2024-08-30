package beethoven.Generation.Annotators

import beethoven.Generation.CLogger
import os.Path

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

object CrossBoundaryDisable {
  def apply(disableList: Seq[String], targetDir: Path): Unit = {
    // read file on targetDir / "BeethovenTop.v" and replace module names in disableList with (* keep_hierarchy = "yes" *)
    val start_time = System.currentTimeMillis()
    // for each file, go through and look for module instances called $name
    // go through and replace <module_name> $name with (* keep_hierarchy = "yes" *) <module_name> $name
    // make sure to capture the module name and repeat it
    val files = os.walk(targetDir).filter(_.last.endsWith(".v"))


    val sed_bin = if (os.proc("sed", "--version").call(check = false).out.text().contains("GNU sed"))
      "sed"
    else
      "gsed"
    // make sed command list for each module
    val sed_cmds = disableList map { mname =>
      f"/$mname (/ i (* keep_hierarchy = \"yes\" *)"
    }
    // write sed cmds to file
    os.write.over(os.pwd / "sed_script.sed", sed_cmds.mkString("\n"))

    //      var found = false
    files.toList.par.foreach { file =>
      os.proc(sed_bin, "-i", f"-f" + os.pwd / "sed_script.sed", file).call()
    }
//    CLogger.log(s"CrossBoundaryDisable took ${System.currentTimeMillis() - start_time} ms")

  }

}
