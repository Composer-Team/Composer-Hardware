package beethoven.Protocol.RoCC

import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import beethoven.common.AccelRoccResponse
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import chisel3._
import chisel3.util.Arbiter

class RoccFanout(implicit p: Parameters) extends LazyModule {
  val node = RoccNexusNode(
    dFn = { mp =>
      assert(mp.length == 1, "Cannot fan-in_cmd from multiple masters.")
      mp(0)
    },
    uFn = { sp =>
      val sids = sp.map(_.system_core_ids.map(_._1)).reduce(_ ++ _).toList.distinct
      val joined_core_maps = sids.map { sid =>
        val all_cores = sp.flatMap(_.system_core_ids.map(_._2))
        val start = all_cores.map(_._1).min
        val max = all_cores.map(_._2).max
        all_cores.foreach { case (low, high) =>
          assert(low == start || all_cores.exists(_._2+1 == low), all_cores.toString())
          assert(high == max || all_cores.exists(_._1-1 == high), all_cores.toString())
        }
        (sid, (start, max))
      }
      RoCCSlaveParams(joined_core_maps)
    }
  )
  lazy val module = new LazyModuleImp(this) {
    val in_cmd = node.in(0)._1.req
    in_cmd.ready := false.B
    val outs = node.out


    outs.zipWithIndex.foreach { case ((outB, outE), _) =>
      val cmd = outB.req
      val can_service_seq = outE.up.system_core_ids.map { case (sid, cores) =>
        val sys_match = in_cmd.bits.inst.system_id === sid.U
        val core_match = in_cmd.bits.inst.core_id >= cores._1.U && in_cmd.bits.inst.core_id <= cores._2.U
        (sys_match && core_match, sid)
      }
      val can_service = VecInit(can_service_seq.map(_._1).toSeq).reduceTree(_ || _)
      cmd.valid := can_service && in_cmd.valid
      cmd.bits := in_cmd.bits
      when(can_service) {
        in_cmd.ready := cmd.ready
      }
    }

    val in_resp = node.in(0)._1.resp
    // need to arbitrate for control of response
    val q = Module(new Arbiter(new AccelRoccResponse, outs.length))
    q.io.in.zip(outs.map(_._1.resp)).foreach { case (a, b) => a <> b }
    in_resp <> q.io.out
    val core_keys = outs.map(_._2.up.system_core_ids)
    val each_is_singular = core_keys.map { ck => ck.size == 1 && ck.map(a => a._2._1 == a._2._2).head }.forall(identity)
    // if the endpoint for a manager is for a single system/core, then we know exactly where it's from
    // Since cores are internally anonymous, the core is unable to provide backrouting information (e.g., core/sys ID)
    // The final endpoint is responsible for attaching this information for routing back
    if (each_is_singular) {
      val sys_select = VecInit(core_keys.map(a => a.head._1.U))
      val core_select = VecInit(core_keys.map(a => a.head._2._1.U))

      in_resp.bits.system_id := sys_select(q.io.chosen)
      in_resp.bits.core_id := core_select(q.io.chosen)
    }
  }
}

object RoccFanout {
  private var rocc_fanout_idx = 0
  def apply()(implicit p: Parameters): RoccNexusNode = LazyModuleWithFloorplan(new RoccFanout(), {
    val id = rocc_fanout_idx
    rocc_fanout_idx += 1
    s"zzrocc_fanout_$id"
  }).node
  def apply(name: String)(implicit p: Parameters): RoccNexusNode = LazyModuleWithFloorplan(new RoccFanout(), name).node
}