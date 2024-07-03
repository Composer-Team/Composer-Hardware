package beethoven.Protocol.RoCC

import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import beethoven.common._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import chisel3._
import chisel3.util._

class RoccFanin(implicit p: Parameters) extends LazyModule {
  val node = RoccNexusNode(
    dFn = { sm => sm(0) },
    uFn = {
      ss =>
        assert(ss.length == 1, "Cannot fanin to more than one slave")
        ss(0)
    }
  )

  lazy val module = new LazyModuleImp(this) {
    val ins = node.in
    val out = node.out(0)
    if (ins.length == 1) {
      println("SIMPLE")
      val ir = ins(0)._1.req
      val or = out._1.req
      ir.ready := or.ready
      or.valid := ir.valid
      or.bits := ir.bits

      val ire = ins(0)._1.resp
      val ore = out._1.resp
      ire.valid := ore.valid
      ore.ready := ire.ready
      ire.bits := ore.bits
    } else {
      // IFF a command passes through with 'xd' high (expect response bit), then we need to remember the master it came
      // from. The payload does not include identifying master behavior and adding such a thing would take up space
      // in the payload. SO, we need to be able to map from sys+cmd id pair to master
      val cmd_arbiter = Module(new Arbiter(new AccelRoccCommand(), ins.length))
      ins.map(_._1.req).zip(cmd_arbiter.io.in).foreach { case (a, b) => a <> b }
      cmd_arbiter.io.out <> out._1.req

      val sys_vecs = VecInit(out._2.up.system_core_ids.map { case (sid, cores) =>
        val nCores = cores._2 - cores._1
        val master_source = Reg(Vec(nCores, UInt(log2Up(ins.length).W)))
        val sys_match = out._1.req.bits.getSystemID === sid.U

        when(out._1.req.fire && sys_match && out._1.req.bits.inst.xd) {
          master_source(out._1.req.bits.getCoreID - cores._1.U) := cmd_arbiter.io.chosen
          assert(out._1.req.bits.getCoreID < cores._1.U && out._1.req.bits.getCoreID >= cores._2.U, "Core requested is out of range of available cores...")
        }
        master_source
      }.toSeq)

      ins.zipWithIndex.foreach { case (a, idx) =>
        a._1.resp.valid := sys_vecs(out._1.resp.bits.system_id)(out._1.resp.bits.core_id) === idx.U && out._1.resp.valid
        a._1.resp.bits := out._1.resp.bits
      }
    }
  }
}

object RoccFanin {
  def apply()(implicit p: Parameters): RoccNexusNode = LazyModuleWithFloorplan(new RoccFanin()).node
  def apply(name: String)(implicit p: Parameters): RoccNexusNode = LazyModuleWithFloorplan(new RoccFanin(), name).node
}