package composer.Generation

package object Annotators {
  def addEscapes(s: String): String = {
    val back = "\\\\"
    val doubleBack = "\\\\\\\\"
    s.replaceAll(back, doubleBack).replaceAll("\\$", "\\\\\\$")
  }

  private[Annotators] def get_os(): String = {
    os.proc("uname").call().out.trim
  }

  private[Annotators] def get_sed_inline_opt(): Seq[String] = {
    get_os() match {
      case "Darwin" => Seq("-I", "")
      case "Linux" => Seq("-i")
      case _ => throw new Exception("Couldn't figure out OS for " + get_os())
    }
  }
}
