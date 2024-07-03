package beethoven.Floorplanning

import chisel3.experimental.BaseModule
import chisel3._
import beethoven.Floorplanning.LazyModuleWithSLRs.globalNameList
import freechips.rocketchip.diplomacy._

object LazyModuleImpWithSLRs {
  private var gl_id = 0
  /**
   * Automate SLR mapping a little bit with this interface. Wraps around standard Chisel `Module` interface and performs
   * some additonal bookkeeping whenever tieClocks(). If slrId is not manually defined, it assumes the Default SLR
   * specified by the Platform Configuration.
   */
  def ModuleWithFloorplan[T <: BaseModule](m: => T)(implicit valName: ValName): T = {
    DeviceContext.currentDevice match {
      case Some(q) => ModuleWithFloorplan(m, q)
      case None => Module(m)
    }
  }

  def ModuleWithFloorplan[T <: BaseModule](m: => T, name: String): T = {
    DeviceContext.currentDevice match {
      case Some(q) => ModuleWithFloorplan(m, q, name)
      case None =>
        val r = Module(m)
        r.suggestName(name)
        r
    }
  }

  def ModuleWithFloorplan[T <: BaseModule](m: => T, real_slrid: Int, name: String): T = {
    val mod = Module(m)
    mod.suggestName(name)
    if (globalNameList.contains(name)) {
      throw new Exception(s"Name $name already exists in the globalNameList. Give this module($name) a name that will be globally unique")
    }
    globalNameList = name :: globalNameList
    gl_id = gl_id + 1
    ConstraintGeneration.addToSLR(name, real_slrid)
    mod
  }

  /**
   * Automate SLR mapping a little bit with this interface. Wraps around standard Chisel `Module` interface and performs
   * some additonal bookkeeping whenever tieClocks(). If slrId is not manually defined, it assumes the Default SLR
   * specified by the Platform Configuration.
   */
  def ModuleWithFloorplan[T <: BaseModule](m: => T, real_slrid: Int)(implicit valName: ValName): T = {
    val mod = Module(m)
    val name = valName.name + "_" + gl_id
    gl_id = gl_id + 1
    mod.suggestName(name)
    if (globalNameList.contains(name)) {
      throw new Exception(s"Name $name already exists in the globalNameList. Give this module($name) a name that will be globally unique")
    }
    globalNameList = name :: globalNameList
    ConstraintGeneration.addToSLR(name, real_slrid)
    mod
  }

}
