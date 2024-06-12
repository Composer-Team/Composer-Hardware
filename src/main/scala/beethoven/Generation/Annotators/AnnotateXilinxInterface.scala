package beethoven.Generation.Annotators

import beethoven.Generation.Annotators.AnnotateXilinxInterface.XilinxInterface.{ACE, XilinxInterface}
import os._
object AnnotateXilinxInterface {
  object XilinxInterface extends Enumeration {
    val ACE, AXI4, AHB = Value
    type XilinxInterface = Value
  }
  def apply(prefix: String, fname: String, interface: XilinxInterface): Unit = {
    val interfaceName = interface match {
      case XilinxInterface.ACE => "xilinx.com:interface:acemm_rtl:1.0"
      case XilinxInterface.AXI4 => "xilinx.com:interface:aximm_rtl:1.0"
      case XilinxInterface.AHB => "xilinx.com:interface:ahblite_rtl:1.0"
    }
    val busName = prefix.toUpperCase()
    val sedCmds = Seq("input", "output").map { d: String =>
      s"s/( *)$d (.*) ${prefix}_([^,]*)(,)?/\\1\\(\\* X_INTERFACE_INFO = \"$interfaceName $busName \\3\" \\*\\)\\n" +
        s"\\1$d \\2 ${prefix}_\\3\\4/" }
//     then, remove the repeated modules from the source
//     join sedCMds with ;
    val sedCmd = Seq("sed") ++ get_sed_inline_opt() ++ Seq("-E", sedCmds.mkString(";"), fname)

    os.proc(sedCmd).call()
    (os.pwd / "tmp.v").toString()
  }

}
