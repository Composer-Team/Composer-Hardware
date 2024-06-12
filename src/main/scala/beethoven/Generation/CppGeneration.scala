package beethoven.Generation

import chisel3._
import beethoven.common._

object CppGeneration {

  private[beethoven] case class CppDefinition(ty: String, name: String, value: String)

  private[beethoven] case class PreprocessorDefinition(ty: String, value: String)

  private[beethoven] case class HookDef(sysName: String, cc: AccelCommand, resp: AccelResponse, opCode: Int) {
    cc.elements.foreach { p =>
      val data = p._2
      val paramName = p._1
      assert(data.getWidth <= 64 || data.getWidth % 8 == 0,
        "Bit fields that are longer than 64b MUST be byte aligned (for our own implementation-simplicity-sake).\n" +
          " If this is an absolutely necessary functionality, contact developers.\n" +
          s"Variable in question: $paramName. Length: ${data.getWidth}\n")
    }
  }

  private[beethoven] var user_enums: List[chisel3.ChiselEnum] = List()
  private[beethoven] var user_defs: List[CppDefinition] = List()
  private[beethoven] var user_cpp_defs: List[PreprocessorDefinition] = List()
  private[beethoven] var hook_defs: List[HookDef] = List()

  def addUserCppDefinition[t](ty: String, name: String, value: t,
                              resolveConflicts: Option[(String, t) => t]): Unit = {
    val ty_f = ty.trim
    val name_f = name.trim
    val existingDefs = user_defs.filter(_.name == name_f)
    if (existingDefs.isEmpty) user_defs = CppDefinition(ty_f, name_f, value.toString) :: user_defs
    else {
      resolveConflicts match {
        case Some(resolver) =>
          val without = user_defs.filter(_.name != name_f)
          user_defs = CppDefinition(ty_f, name_f, resolver(existingDefs(0).value, value).toString) :: without
        case None =>
          existingDefs.foreach(a => require(a.ty == ty_f && a.value == value.toString, s"Redefining ${a.name} from (${a.ty}, ${a.value}) to ($ty, $value)"))
      }
    }
  }

  private[beethoven] def addUserCppFunctionDefinition(systemName: String, cc: AccelCommand, resp: AccelResponse, opCode: Int): Unit = {
    if (!hook_defs.exists( q => q.sysName == systemName && q.opCode == opCode))
      hook_defs = HookDef(systemName, cc, resp, opCode) :: hook_defs
  }

  def addPreprocessorDefinition(elems: Seq[(String, Any)]): Unit = {
    elems.foreach(a => addPreprocessorDefinition(a._1, a._2.toString))
  }

  def addPreprocessorDefinition(name: String, value: Any = ""): Unit = {
    val ppd = PreprocessorDefinition(name, value.toString)
    if (!user_cpp_defs.contains(ppd)) {
      user_cpp_defs = ppd :: user_cpp_defs
    }
  }

  def addUserCppDefinition[t](elems: Seq[(String, String, t)],
                              resolver: Option[(String, t) => t] = None): Unit = {
    elems.foreach(a => addUserCppDefinition(a._1, a._2, a._3, resolver))
  }

  //noinspection ScalaUnusedSymbol
  def exportChiselEnum(enum: chisel3.ChiselEnum): Unit = {
    if (!user_enums.contains(enum))
      user_enums = enum :: user_enums
  }
}
