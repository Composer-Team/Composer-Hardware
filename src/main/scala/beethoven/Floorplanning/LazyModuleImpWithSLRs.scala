package beethoven.Floorplanning

import chipsalliance.rocketchip.config.Parameters
import chisel3.experimental.BaseModule
import chisel3.{Module, RawModule}
import beethoven.Floorplanning.LazyModuleWithSLRs.globalNameList
import freechips.rocketchip.diplomacy.{LazyModuleImp, ValName}

class LazyModuleImpWithSLRs(wrapper: LazyModuleWithSLRs)(implicit p: Parameters) extends LazyModuleImp(wrapper) {
  private var clockMap: List[(BaseModule, Int)] = List.empty
  private var gl_id = 0

  /**
   * Automate SLR mapping a little bit with this interface. Wraps around standard Chisel `Module` interface and performs
   * some additonal bookkeeping whenever tieClocks(). If slrId is not manually defined, it assumes the Default SLR
   * specified by the Platform Configuration.
   */
  def ModuleWithSLR[T <: BaseModule](m: => T, real_slrid: Int)(implicit valName: ValName): T = {
    val mod = Module(m)
    val name = wrapper.baseName + "_" + valName.name + "_" + gl_id
    gl_id = gl_id + 1
    mod.suggestName(name)
    if (!ConstraintGeneration.canDistributeOverSLRs()) return mod
    if (globalNameList.contains(name)) {
      throw new Exception(s"Name $name already exists in the globalNameList. Give this module($name) a name that will be globally unique")
    }
    globalNameList = name :: globalNameList
    ConstraintGeneration.addToSLR(name, real_slrid)
    clockMap = (mod, real_slrid) :: clockMap
    mod
  }

  def ModuleWithSLR[T <: BaseModule](m: => T, real_slrid: Int, name: String)(implicit valName: ValName): T = {
    val mod = Module(m)
    mod.suggestName(name)
    if (!ConstraintGeneration.canDistributeOverSLRs()) return mod
    if (globalNameList.contains(name)) {
      throw new Exception(s"Name $name already exists in the globalNameList. Give this module($name) a name that will be globally unique")
    }
    globalNameList = name :: globalNameList
    gl_id = gl_id + 1
    ConstraintGeneration.addToSLR(name, real_slrid)
    clockMap = (mod, real_slrid) :: clockMap
    mod
  }


  /**
   * This is deprecated. I used to think we'd have to do more manual clocking than we really have to do.
   */
  def tieClocks(): Unit = {
    if (!ConstraintGeneration.canDistributeOverSLRs()) return
  }
}
