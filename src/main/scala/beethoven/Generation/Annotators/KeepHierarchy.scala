package beethoven.Generation.Annotators

import os.Path

object KeepHierarchy {
  def apply(fname: Path): Unit = {
    os.proc(Seq("sed") ++ get_sed_inline_opt() ++ Seq("-E",
      "s/(module BeethovenSystem)/(* keep_hierarchy = \"yes\" *)\\n\\1/",
      fname.toString())).call()
  }
}
