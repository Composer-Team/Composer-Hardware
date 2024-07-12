package beethoven

import beethoven.Floorplanning.DeviceContext
import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import beethoven.Parameters.IntraCoreMemoryPortInConfig.IntraCoreCommunicationDegree
import beethoven.Parameters.{AcceleratorSystems, IntraCoreMemoryPortInConfig}
import beethoven.Protocol.RoCC.{RoccBuffer, RoccCompositeXbar, RoccFanin, RoccFanout, RoccIdentityNode, RoccNode}
import beethoven.common.CLog2Up
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.{Cat, log2Up}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.{TLBuffer, TLNode, TLXbar}

import scala.annotation.tailrec

package object Systems {

  @tailrec
  private def recursivelyDecide(allocations: Seq[(Int, Int, Double)], nCores: Int)(implicit p: Parameters): Seq[(Int, Int, Double)] = {
    if (nCores == 0) allocations
    else {
      val min = allocations.min(Ordering.by[(Int, Int, Double), Double](_._3))
      val idx = allocations.indexOf(min)
      val affinity = 1.0 / platform.placementAffinity(min._1)
      val newAllocations = allocations.updated(idx, (min._1, min._2 + 1, min._2 + affinity))
      recursivelyDecide(newAllocations, nCores - 1)
    }
  }

  def core2slr(coreIdx: Int)(implicit p: Parameters): Int = {
    val deviceIdxs = platform.physicalDevices.map(_.identifier)
    val allocations = recursivelyDecide(deviceIdxs.map(a => (a, 0, 0.0)), coreIdx)
    allocations.indexOf(allocations.min)
  }

  def slr2ncores(deviceId: Int, totalCores: Int)(implicit p: Parameters): (Int, Int) = {
    val deviceIdxs = platform.physicalDevices.map(_.identifier)
    val allocations = recursivelyDecide(deviceIdxs.map(a => (a, 0, 0.0)), totalCores)
    val sumUp = (0 until deviceId).map(b => allocations.find(_._1 == b).get._2).sum
    (allocations.find(_._1 == deviceId).get._2, sumUp)
  }


  def getCommMemSpaceBits()(implicit p: Parameters): Int = {
    val configs = p(AcceleratorSystems)
    val params = configs.flatMap(_.memoryChannelConfig.filter(_.isInstanceOf[IntraCoreMemoryPortInConfig])).map(_.asInstanceOf[IntraCoreMemoryPortInConfig])
    val biggest_on_chip_comm_mem = params.map { mp => mp.nDatas * mp.nChannels * mp.dataWidthBits / 8 }.max
    log2Up(biggest_on_chip_comm_mem)
  }

  def getCommMemCoreBits()(implicit p: Parameters): Int = {
    val configs = p(AcceleratorSystems)
    val max_cores = configs.filter(_.memoryChannelConfig.exists(_.isInstanceOf[IntraCoreMemoryPortInConfig])).map(co =>
      co.memoryChannelConfig.filter(_.isInstanceOf[IntraCoreMemoryPortInConfig]).map {
        case icm: IntraCoreMemoryPortInConfig =>
          icm.communicationDegree match {
            case IntraCoreCommunicationDegree.BroadcastAllCores | IntraCoreCommunicationDegree.BroadcastAllCoresChannels => 1
            case _ => co.nCores
          }
      }.max
    ).max
    log2Up(max_cores)
  }

  def getCommMemChannelBits()(implicit p: Parameters): Int = {
    // figure out memory space for on-chip memory comms
    val configs = p(AcceleratorSystems)
    val max_channels = configs.filter(_.memoryChannelConfig.exists(_.isInstanceOf[IntraCoreMemoryPortInConfig])).map(co =>
      co.memoryChannelConfig.filter(_.isInstanceOf[IntraCoreMemoryPortInConfig]).map {
        case icm: IntraCoreMemoryPortInConfig =>
          icm.communicationDegree match {
            case IntraCoreCommunicationDegree.BroadcastAllCoresChannels | IntraCoreCommunicationDegree.BroadcastAllChannels => 1
            case _ => co.nCores
          }
      }.max
    ).max

    log2Up(max_channels)
  }


  def getCommMemAddress(sys: String, core: Any, endpoint: String, channel: Any, spaceAddr: UInt)(implicit p: Parameters): UInt = {
    // figure out memory space for on-chip memory comms
    val configs = p(AcceleratorSystems)
    val max_endpoints_per_core = configs.map(_.memoryChannelConfig.filter(_.isInstanceOf[IntraCoreMemoryPortInConfig]).map(_.asInstanceOf[IntraCoreMemoryPortInConfig])).map(_.length).max

    val space_bits = getCommMemSpaceBits()
    val channel_bits = getCommMemChannelBits()
    val endpoint_bits = log2Up(max_endpoints_per_core)
    val core_bits = getCommMemCoreBits()

    val endpoint_idx = configs.find(_.name == sys).get.memoryChannelConfig.filter(_.isInstanceOf[IntraCoreMemoryPortInConfig]).indexWhere(_.name == endpoint)
    val sys_idx = configs.indexWhere(_.name == sys)
    require(spaceAddr.getWidth == space_bits)
    // sys, core, endpoint, channel, space
    Cat(sys_idx.U, core match {
      case a: Int => a.U(core_bits.W)
      case a: UInt => a
    }, endpoint_idx.U(endpoint_bits.W),
      channel match {
        case a: Int => a.U(channel_bits.W)
        case a: UInt => a
      }, spaceAddr)
  }

  def getCommMemAddressSet(sys: String, core: Int, endpoint: IntraCoreMemoryPortInConfig, channel: Int)(implicit p: Parameters): AddressSet = {
    // figure out memory space for on-chip memory comms
    val name = endpoint.name
    val configs = p(AcceleratorSystems)
    val max_endpoints_per_core = configs.map(_.memoryChannelConfig.filter(_.isInstanceOf[IntraCoreMemoryPortInConfig]).map(_.asInstanceOf[IntraCoreMemoryPortInConfig])).map(_.length).max

    val space_bits = getCommMemSpaceBits()
    val channel_bits = getCommMemChannelBits()
    val endpoint_bits = log2Up(max_endpoints_per_core)
    val core_bits = getCommMemCoreBits()

    val endpoint_idx = configs.find(_.name == sys).get.memoryChannelConfig.filter(_.isInstanceOf[IntraCoreMemoryPortInConfig]).indexWhere(_.name == name)
    val sys_idx = configs.indexWhere(_.name == sys)
    // sys, core, endpoint, channel, space

    val core_fix = endpoint.communicationDegree match {
      case IntraCoreCommunicationDegree.BroadcastAllCoresChannels | IntraCoreCommunicationDegree.BroadcastAllCores => 0
      case _ => core
    }

    val channel_fix = endpoint.communicationDegree match {
      case IntraCoreCommunicationDegree.BroadcastAllCoresChannels | IntraCoreCommunicationDegree.BroadcastAllChannels => 0
      case _ => channel
    }

    AddressSet(
      (sys_idx << (space_bits + channel_bits + endpoint_bits + core_bits)) |
        (endpoint_idx << (space_bits + channel_bits + core_bits)) |
        (core_fix << (space_bits + channel_bits)) |
        (channel_fix << space_bits), (1 << space_bits) - 1)
  }

  @tailrec
  def fanout_recursive(grp: Iterable[RoccNode], xbarDeg: Int)(implicit p: Parameters): RoccNode = {
    grp match {
      case a :: Nil =>
        a
      case _ =>
        val groups = grp.grouped(xbarDeg).map { q =>
          val fan_out = LazyModule(new RoccFanout).node
          val id = RoccIdentityNode()
          q.foreach { qp =>
            val buff = RoccBuffer()
            qp := buff
            buff := fan_out
          }
          fan_out := id
          id
        }.toSeq
        fanout_recursive(groups, xbarDeg)
    }
  }

  @tailrec
  def fanin_recursive(grp: Iterable[RoccNode], xbarDeg: Int, toDeg: Int)(implicit p: Parameters): Iterable[RoccNode] = {
    if (grp.size <= toDeg) grp
    else {
      val groups = grp.grouped(xbarDeg).map { q =>
        val fan_in = LazyModule(new RoccFanin)
        val id = RoccIdentityNode()
        q.foreach(fan_in.node := RoccBuffer() := _)
        id := fan_in.node
        id
      }.toSeq
      fanin_recursive(groups, xbarDeg, toDeg)
    }
  }

  @tailrec
  def xbar_tree_reduce_sources[T](eles: Seq[T],
                                  xbarDeg: Int,
                                  toDeg: Int,
                                  make_xbar: () => T,
                                  make_buffer: () => T,
                                  assign_to: (Seq[T], T) => Unit)(implicit p: Parameters): Seq[T] = {
    if (eles.length <= toDeg) {
      eles
    } else {
      val groups = eles.grouped(xbarDeg).map { q: Seq[T] =>
        val xbar: T = make_xbar()
        q.foreach { qp: T =>
          val buff: T = make_buffer()
          assign_to(Seq(buff), xbar)
          assign_to(Seq(qp), buff)
        }
        xbar
      }.toSeq
      xbar_tree_reduce_sources(groups, xbarDeg, toDeg, make_xbar, make_buffer, assign_to)
    }
  }


  @tailrec
  def xbar_tree_reduce_sinks[T <: NodeHandle[_, _, _, _, _, _, _, _]](eles: Seq[T],
                                                                      xbarDeg: Int,
                                                                      toDeg: Int,
                                                                      make_xbar: () => T,
                                                                      make_buffer: () => T,
                                                                      assign_to: (Seq[T], T) => Unit)(implicit p: Parameters): Seq[T] = {
    if (eles.length <= toDeg) {
      eles
    } else {
      val groups = eles.grouped(xbarDeg).map { q =>
        val xbar = make_xbar()
        eles.foreach { ele =>
          val buff = make_buffer()
          assign_to(Seq(ele), buff)
          assign_to(Seq(buff), xbar)
        }

        xbar
      }.toSeq
      xbar_tree_reduce_sinks(groups, xbarDeg, toDeg, make_xbar, make_buffer, assign_to)
    }
  }

  @tailrec
  def extend_eles_via_protocol_node[T <: NodeHandle[_, _, _, _, _, _, _, _]](eles: Seq[T],
                                                                             make_buffer: () => T,
                                                                             assign: (Seq[T], T) => Unit,
                                                                             buffer_depth: Int = 1)(implicit p: Parameters): Seq[T] = {
    buffer_depth match {
      case x if x <= 0 => eles
      case _ =>
        extend_eles_via_protocol_node(eles.map { ele =>
          val buffer = make_buffer()
          assign(Seq(ele), buffer)
          buffer
        }, make_buffer, assign, buffer_depth - 1)
    }
  }

  def extend_ele_via_protocol_node[T <: NodeHandle[_, _, _, _, _, _, _, _]](ele: T,
                                                                            make_buffer: () => T,
                                                                            assign: (Seq[T], T) => Unit,
                                                                            buffer_depth: Int = 1)(implicit p: Parameters): T =
    extend_eles_via_protocol_node(Seq(ele), make_buffer, assign, buffer_depth)(p)(0)


  def create_cross_chip_network[T <: NodeHandle[_, _, _, _, _, _, _, _]](sources: List[(T, Int)],
                                                                         devices_with_sinks: List[Int],
                                                                         make_buffer: () => T,
                                                                         make_xbar: () => T,
                                                                         assign: (Seq[T], T) => Unit,
                                                                        )(implicit p: Parameters): Map[Int, List[T]] = {
    val connectivity = platform.physicalConnectivity
    val devices = platform.physicalDevices.map(_.identifier)
    if (connectivity.nonEmpty) {
      val root_source = connectivity.find(a => !connectivity.exists(_._2 == a._1)).get._1
      val root_sink = connectivity.find(a => !connectivity.exists(_._1 == a._2)).get._2
      assert(connectivity.forall(a => (a._1 == root_source) || connectivity.exists(b => b._2 == a._1)), "Connectivity graph must be a SUG")
      assert(connectivity.forall(a => (a._2 == root_sink) || connectivity.exists(b => b._1 == a._2)), "Connectivity graph must be a SUG")
    }

    var idx = 0
    val carry_inits = Map.from(devices.map { did =>
      val carries = sources.filter(_._2 == did).map { b =>
        val tidx = idx
        idx = idx + 1
        (b._1, List(tidx))
      }
      (did, carries)
    })
    val commit_inits = Map.from(devices.map { did =>
      val commits = List.empty[(T, List[Int])]
      (did, commits)
    })
    val fpass = topological_xbarReduce_over_SUG(
      devices,
      connectivity,
      carry_inits,
      commit_inits,
      make_buffer,
      make_xbar,
      assign)(p, devices_with_sinks)
    val bpass = topological_xbarReduce_over_SUG(
      devices,
      connectivity.map(_.swap),
      carry_inits,
      fpass,
      make_buffer,
      make_xbar,
      assign)(p, devices_with_sinks)

    bpass.map { case (k, v) => (k, v.map(_._1)) }
  }

  /**
   * ----------- PROBLEM SETUP ----------
   * The problem: We have a set of sources and a set of sinks, each scattered over potentially
   * many devices. Devices-crossings are possible but need to instrumented with a buffer on either side, like so:
   *
   * DEVICE A               |    DEVICE B
   * source (a)--->buffer---X-->buffer--> sink(b)
   * |
   *
   * Buffers have one input and one output. Crossbars (xbars) can have multiple inputs and outputs. The input and
   * output degree of crossbar networks should be limited. Hence, we recursively reduce xbar networks by degree 2 at
   * each layer. The degree can be tuned.
   *
   * Any constructed points in the network CANNOT be "tied-off" - they must eventually connect to a sink.
   * Cycles in the graph are illegal. Multiple paths from a source to a sink are illegal. Backtracking (i.e., using
   * try-catch to catch these aforementioned problems) is illegal. In this context, we mean "constructed" in the
   * language context, an xbar or buffer object cannot be constructed without eventually connecting this object
   * to a sink object. Device crossings should be minimized.
   *
   * Significant complication: Xbars cannot be separated. That is, if xbar `j` joins sources (`a`, `b`), then
   * these requests cannot be separated back into components `a` and `b` to form more optimized networks for `a` and
   * `b`. I'll discuss why this is complicated later.
   *
   * Devices are connected as a "Strongly Unambiguous Graph" (SUG): There exists only one path from any source to any
   * sink. This is overly restrictive and forbids bi-directional connectivity or rings, which necessarily exist with
   * FPGA SLLs, for instance. I provide a solution for this issue in the solution.
   *
   * Context: when I say node, I mean it to be either a xbar, buffer, or a source
   *
   * ----------- PROBLEM SOLUTION DISCUSSION ----------
   *
   * Given a directed graph adjacency list (`connectivity`) and list of sources (`carries`) as a Map, accumulate the
   * nodes to form as the final inputs to a crossbar to be assigned to the devices (`commits`).
   * Carries and commits a map from DeviceID -> List of (node, List of sourceIDs)
   * The lambdas `make_buffer` and `make_xbar` form a buffer or xbar that is compatible with the on-chip protocol
   * inferred from type arguments.
   * `devicesWithSinks` is a list of the devices that actually have sinks on them.
   *
   * Previously, I said that we would talk about what to do with connectivity for generic graphs. To resolve the
   * problem, the caller needs to mess with the connectivity graph before calling this function. Consider the
   * following graph with 4 undirected edges:
   * A - B
   * |   |
   * C - D
   * The user will need to construct a set SUGs such that the union of these SUGs forms the same connectivity as the
   * original undirected graph. This can always be done with a hamiltonian path of the graph and its reverse. For
   * example, the aforementioned graph can be deconstructed as
   * A  >  B          A  <  B
   * \/    and        /\
   * C  <  D          C  >  D
   * However, some graphs will require more passes, and our algorithm currently doesn't support this, and there are
   * some other path-length inefficiencies. For instance, consider the above graphs. Although the undirected
   * connectivity diagram supports connectivity directly from C->A, the network that we produce will induce latencies
   * of 3 hops instead of 1. This cannot be resolved to provide minimum latency for all possible source-sink
   * combinations without supporting cycles in the network. If we support cycles, then the paths are easily
   * deduced using all-source shortest path algorithms (e.g., Floyd-Warshall [2]).
   *
   * That said, there are still optimizations that can result with better results, even with these drawbacks.
   * First, the connectivity graph provided to the algorithm can take into account the original locations of the
   * sinks and sources. For instance, if there is a heavy concentration of sources on node C, then it makes sense
   * to minimize the longest path from C to any sink. This is some sort of optimization problem that exists but I
   * don't know the name of it.
   *
   * -----------  ALGORITHM ----------
   *
   * Perform topological walk of the `connectivity` graph (induced SUG from the undirected graph).
   * Initialize carries with the mapping of [deviceID -> List of [sources Node (hardware construct), source ID] ]
   * Initialize commits to be blank.
   * On each step, find `roots`: vertices with no incoming edges.
   * For each root `r`, consider the set of carries: `q := carries(r)`
   * IFF device `r` has sinks on the device:
   * For each carry `c`, whose sourceID set is not already assigned in `commits(r)`, push `c` to `commits(r)`.
   * IFF sinks are reachable from neighbor of `r`:
   * xbar down all sources in `carries(r)`, buffer this xbar on both device `r` and on destination device `neigh := neighbor(r)`
   * Add the resulting buffer and the union of the source lists of the inputs to the carry list of device `neigh`.
   * Remove all paths `r` -> `_` from `connectivity`.
   * Recursively call this algorithm until `connectivity` is empty.
   *
   * Call this algorithm on the reverse graph, with `carries` initialized to the same as before, but with
   * `commits` using the output of the first call.
   *
   *
   * [1] https://en.wikipedia.org/wiki/Directed_acyclic_graph#Related_families_of_graphs
   * [2] https://en.wikipedia.org/wiki/Shortest_path_problem#Directed_graph
   */
  @tailrec
  private def topological_xbarReduce_over_SUG[T <: NodeHandle[_, _, _, _, _, _, _, _]](devices: Seq[Int],
                                                                               connectivity: Seq[(Int, Int)],
                                                                               carries: Map[Int, List[(T, List[Int])]],
                                                                               commits: Map[Int, List[(T, List[Int])]],
                                                                               make_buffer: () => T,
                                                                               make_xbar: () => T,
                                                                               assign: (Seq[T], T) => Unit,
                                                                              )(implicit p: Parameters,
                                                                                devicesWithSinks: List[Int]): Map[Int, List[(T, List[Int])]] = {
    // for the rootsets that exist on devices that do not have memory xbar endpoints, but where a device
    // can be reached from that endpoint, bridge those rootsets to the neighboring device.
    // Because the graph is a DAG, the search is easy
    @tailrec
    def sink_reachable_from_device(device: Int)(implicit p: Parameters): Boolean = {
      if (devicesWithSinks.contains(device)) true
      else {
        //noinspection DuplicatedCode
        connectivity.find(_._1 == device) match {
          case Some((_, b)) => sink_reachable_from_device(b)
          case None => false
        }
      }
    }

    def sink_reachable_from_neighbor(device: Int)(implicit p: Parameters): Boolean = {
      //noinspection DuplicatedCode
      connectivity.find(_._1 == device) match {
        case Some((_, b)) => sink_reachable_from_device(b)
        case None => false
      }
    }

    @tailrec
    def update_map[K, D](m: Map[K, List[D]], updates: Iterable[(K, List[D])]): Map[K, List[D]] = {
      if (updates.isEmpty) m
      else {
        val (k, v) = updates.head
        update_map(m + (k -> (v ++ m(k))), updates.tail)
      }
    }

    // (set of devices that have no incoming connections, the complement)
    val (roots, links) = devices.partition(a => !connectivity.exists(_._2 == a))

    if (roots.isEmpty) {
      assert(links.isEmpty, s"Cycle detected in connectivity: $connectivity")
      commits
    } else {
      // for each root in the carry list, consider pushing into commits only if the root exists on a device with memory
      val pushable_roots = roots.filter(a => carries.contains(a) && devicesWithSinks.contains(a))
      val commit_updates = pushable_roots.map {
        root =>
          // for each carry terminal, consider pushing into commits by intersecting the sourceTrees currently in commits
          // with the sourceTree of the carry terminal
          val carry_terms = carries(root)
          val non_intersecting_terms = carry_terms.filter { tup =>
            commits(root).map { case (_, pterm) =>
              val intersect = pterm.intersect(tup._2)
              assert(intersect.length == pterm.length || intersect.isEmpty, "Partial overlap in source trees implies that the graph is not strongly unambiguous")
              intersect.isEmpty
            }.forall(identity)
          }
          (root, non_intersecting_terms.map(a => (extend_ele_via_protocol_node(a._1, make_xbar, assign), a._2)))
      }
      val new_commits = update_map(commits, commit_updates)
      // for each root in the carry list, consider moving the terms to the next device's carry list. This should
      // only be done if a memory device is reachable from the next device according to the connectivity list
      val carry_updates: List[(Int, List[(T, List[Int])])] = roots.filter(a =>
        carries.get(a).exists(b => b.nonEmpty) && sink_reachable_from_neighbor(a)).toList.map {
        root =>
          val carry_terms = carries(root)
          // reduce terms via tree
          val source_buffer = DeviceContext.withDevice(root) {
            extend_ele_via_protocol_node(xbar_tree_reduce_sources(carry_terms.map(_._1), 2, 1, make_xbar, make_buffer, assign)(p)(0), make_buffer, assign)
          }
          val next_device = connectivity.find(_._1 == root).get._2
          val dest_buffer = DeviceContext.withDevice(next_device) {
            extend_ele_via_protocol_node(extend_ele_via_protocol_node(source_buffer, make_buffer, assign), make_xbar, assign)
          }
          val carry_sources = carry_terms.flatMap(_._2)
          (next_device, List((dest_buffer, carry_sources)))
      }
      // devices may connect to the same neighbor, combine like terms

      val carries_combined = carry_updates.map(_._1).toSet.map { root: Int =>
        val sets = carry_updates.filter(_._1 == root)
        sets.tail.fold(sets.head) { case (a, b) => (a._1, a._2 ++ b._2) }
      }
      val new_carries = update_map(carries, carries_combined)
      // now we have the state of the network structure, just need to perform the topological search updates by
      // removing paths originating at the roots from the graph structure (`connectivity`)
      val new_connectivity = connectivity.filter { case (orig, _) => !roots.contains(orig) }
      val new_devices = devices.filter(!roots.contains(_))
      topological_xbarReduce_over_SUG(new_devices, new_connectivity, new_carries, new_commits, make_buffer, make_xbar, assign)
    }
  }

  private var tl_xbar_id = 0
  private var tl_buffer_id = 0

  def make_tl_xbar()(implicit p: Parameters): TLNode = LazyModuleWithFloorplan(new TLXbar(), {
    val id = tl_xbar_id
    tl_xbar_id = tl_xbar_id + 1
    f"zzanonymous_tlxbar_$id"
  }).node

  def make_tl_buffer()(implicit p: Parameters): TLNode = LazyModuleWithFloorplan(new TLBuffer(), {
    val id = tl_buffer_id
    tl_buffer_id = tl_buffer_id + 1
    f"zzanonymous_tlbuffer_$id"
  }).node

  def tl_assign[T <: TLNode](from: Seq[T], to: T)(implicit p: Parameters): Unit = {
    from.foreach(a => to := a)
  }

  def make_rocc_xbar()(implicit p: Parameters): RoccNode = {
    val q: RoccNode = LazyModuleWithFloorplan(new RoccCompositeXbar(), "RoccXbar").node
    q
  }

  private var rocc_buffer_id = 0
  def make_rocc_buffer()(implicit p: Parameters): RoccNode = LazyModuleWithFloorplan(new RoccBuffer(), {
    val id = rocc_buffer_id
    rocc_buffer_id = rocc_buffer_id + 1
    f"zzanonymous_roccbuffer_$id"
  }).node

  def rocc_assign[T <: RoccNode](from: Seq[T], to: T)(implicit p: Parameters): Unit = {
    from.foreach(a => to := a)
  }
}
