package beethoven.Protocol.tilelink.TLSlave

import beethoven.Floorplanning.LazyModuleWithSLRs.LazyModuleWithFloorplan
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, MixedAdapterNode, NexusNode, ValName}
import freechips.rocketchip.tilelink.{TLImp, TLMasterPortParameters, TLSlavePortParameters}
import freechips.rocketchip.util.BundleField
import chisel3._

class TLSlaveXbar()(implicit p: Parameters) extends LazyModule {
  val node = TLSlaveNexusNode(
    dFn = { mpps =>
      require(mpps.size == 1)
      mpps(0)
    }, uFn = { seq =>
      seq(0).copy(
        responseFields = BundleField.union(seq.flatMap(_.responseFields)),
        requestKeys = seq.flatMap(_.requestKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        managers = seq.flatMap { port =>
          require(port.beatBytes == seq(0).beatBytes,
            s"Xbar data widths don't match: ${port.managers.map(_.name)} has ${port.beatBytes}B vs ${seq(0).managers.map(_.name)} has ${seq(0).beatBytes}B")
          port.managers
        }
      )
    }
  )

  lazy val module = new LazyModuleImp(this) {
    val in = node.in.head._1.tl

    // the address ranges for an xbar, to make my life easier, should all be continuous, disjoint sections
    node.out.foreach { outport =>
      val addr_spaces = outport._2.slave.slaves.flatMap(_.address)
      val max_addr = addr_spaces.map(_.base).fold(BigInt(0)) { case (a, b) => a.max(b) }
      if (addr_spaces.size > 1) {
        addr_spaces.foreach { case AddressSet(base, _) =>
          require(base == max_addr || addr_spaces.exists(os => (os.base + (os.mask + 1)) == base))
        }
      }
    }

    node.out.foreach { oport =>
      val bundle = oport._1.tl
      val asets = oport._2.slave.slaves.flatMap(_.address)
      val min_addr = asets.map(_.base).min
      val max_addr = asets.map(as => as.base + as.mask).max
      bundle.valid := in.valid && (in.bits.address >= min_addr.U) && (in.bits.address <= max_addr.U)
      bundle.bits := in.bits
    }
  }
}
object TLSlaveXbar {
  private var tl_slave_xbar_idx = 0
  def apply()(implicit p: Parameters): TLSlaveNexusNode = LazyModuleWithFloorplan(new TLSlaveXbar(), {
    val id = tl_slave_xbar_idx
    tl_slave_xbar_idx += 1
    s"zztl_slave_xbar_$id"
  }).node
}
