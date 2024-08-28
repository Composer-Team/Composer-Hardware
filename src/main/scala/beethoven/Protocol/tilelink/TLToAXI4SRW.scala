// See LICENSE.SiFive for license details.

package beethoven.Protocol.tilelink
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import beethoven.platform
import freechips.rocketchip.tilelink._

class TLToAXI4SRW(implicit p: Parameters) extends LazyModule {
  /**
   * The problem with using a TL to AXI4 converter is that a write and read request in tile link share the same bus
   * whereas in AXI4, they are separate. 1To rememdy this, in Beethoven, we elaborate separate TL networks for writes
   * and reads. In practice, synthesizers should optimize away any unused channels. Then, we use this module to merge
   * these two separate networks into a single AXI4 bus.
   * This idiom is not achievable using existing Diplomacy constructs because the nodes are managers for the same
   * address range.
   */
  val defaultTransferSizes = TransferSizes(
    platform.extMem.master.beatBytes,
    platform.extMem.master.beatBytes * platform.prefetchSourceMultiplicity)

  val node = new MixedNexusNode(TLImp, AXI4Imp)(
    dFn = { tlpp =>
      // assert that there are at most 2 clients
      require(tlpp.size <= 2)
      // require that one client is a reader and the other is a writer
//      if (tlpp.size == 2) {
//        require(tlpp(0).allSupportGet ^ tlpp(1).allSupportGet)
//        require(tlpp(0).allSupportPutFull ^ tlpp(1).allSupportPutFull)
//      }

      AXI4MasterPortParameters(
        masters = Seq(AXI4MasterParameters(
          name = "TLToAXI4SRW",
          id = IdRange(0, tlpp.map(_.endSourceId).max),
          aligned = true,
          maxFlight = Some(1)
        )))
    }, uFn = { axis =>
      require(axis.size == 1)
      TLSlavePortParameters.v1(
        managers = Seq(TLSlaveParameters.v1(
          address = axis(0).slaves(0).address,
          supportsGet = defaultTransferSizes,
          supportsPutFull = defaultTransferSizes,
          supportsPutPartial = defaultTransferSizes,
        )),
        beatBytes = platform.extMem.master.beatBytes)
    }
  )

  lazy val module = new TLToAXI4SRWImpl(this)
}

