package composer.Generation.Annotators

import os.Path

object CrossBoundaryDisable {
  def apply(disableList: Seq[String], targetDir: Path): Unit = {
    // create sed command to add keep_hierarchy to SLR mappings
    val sedcmds = disableList.distinct.map { name =>
      s"s/(module ${addEscapes(name)}) /(* keep_hierarchy = \"yes\" *) \\1 /"
    }
    //      sedcmds foreach {println(_)}
    val sedcmd = Seq("sed") ++ get_sed_inline_opt() ++ Seq("-E", sedcmds.mkString(";"), (targetDir / "ComposerTop.v").toString())
    os.proc(sedcmd).call()
//    System.err.println("Called for cross boundary disables but currently broken...")
  }

}
