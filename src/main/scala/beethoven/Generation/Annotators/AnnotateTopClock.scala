package beethoven.Generation.Annotators

import os.Path

object AnnotateTopClock {
  def apply(annotation: String, fname: Path): Unit = {
    os.proc(Seq("sed") ++ get_sed_inline_opt() ++ Seq("-E",
      f"s/module BeethovenTop\\(/module BeethovenTop\\(\\n$annotation/", fname.toString())).call()
  }
}
