package beethoven.Systems

import beethoven.Parameters.AcceleratorSystems
import beethoven.Protocol.RoCC.{RoccBuffer, RoccFanout, RoccIdentityNode}
import beethoven.common.Misc
import beethoven._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.TLNode

class Subdevice(val deviceId: Int)(implicit p: Parameters) extends LazyModule {
  val configs = p(AcceleratorSystems)
  val constructDependencies = configs.map { a => a.canIssueCoreCommandsTo.map(b => (a.name, b)) }

  val submodules = Misc.topological_sort_depends(configs.map(_.name), constructDependencies.flatten).map { name =>
    val config = configs.find(_.name == name).get
    val (n, offset) = slr2ncores(deviceId, config.nCores)
    val lm = LazyModule(new AcceleratorSystem(n, offset)(p, config, deviceId))
    lm.suggestName(f"sd${deviceId}_sys${config.name}")
    lm
  }


  val Seq(r_nodes, w_nodes) = Seq(submodules.flatMap(_.r_nodes), submodules.flatMap(_.w_nodes)).map { nodes =>
      xbar_tree_reduce_sources[TLNode](nodes, platform.xbarMaxDegree, platform.memEndpointsPerDevice, make_tl_xbar, make_tl_buffer, tl_assign)
  }

  val host_rocc = {
    val fanout = LazyModule(new RoccFanout())
    submodules.foreach { sm =>
      val q = LazyModule(new RoccBuffer())
      sm.rocc_node := q.node := fanout.node
    }
    val buff = LazyModule(new RoccBuffer())
    fanout.node := buff.node
    val id = RoccIdentityNode()
    buff.node := id
    id
  }

  val source_rocc = {
    fanin_recursive(submodules.flatMap(_.rocc_oc).flatten.map(_._2), 2, submodules.length)
  }

  val outgoing_mem = {
    val mems = submodules.flatMap(q => q.intraCoreMemMasters)
    xbar_tree_reduce_sources[TLNode](mems.flatMap(_._2), platform.xbarMaxDegree, 1, make_tl_xbar, make_tl_buffer, tl_assign)
  }


  val incoming_mem = {
    val mems = submodules.flatMap(a => a.intraCoreMemSlaveNode)
    xbar_tree_reduce_sinks(mems, platform.xbarMaxDegree, 1, make_tl_xbar, make_tl_buffer, tl_assign)
  }

  lazy val module = new LazyModuleImp(this) {}
}
