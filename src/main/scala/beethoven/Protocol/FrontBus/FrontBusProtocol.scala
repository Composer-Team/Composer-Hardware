package beethoven.Protocol.FrontBus

import beethoven.Protocol.RoCC.{RoccClientNode, RoccIdentityNode, RoccNode}
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import freechips.rocketchip.tilelink.{TLIdentityNode, TLNode}


abstract class FrontBusProtocol {
  /**
   * tlChainObj provides the diplomacy objects generated in `deriveTLSources` so that you, from a non-lazy context
   * can generate the physical IOs and tie them to the diplomacy object bundle IOs
   */
  def deriveTopIOs(tlChainObj: Any, withClock: Clock, withActiveHighReset: Reset)(implicit p: Parameters): Unit

  /**
   * This function is executed from the lazy context. Generate the following:
   * 1. the top-level protocol nodes of your choice. These will be `Any`-typed and passed to `deriveTopIOs` to be
   *    tied-off.
   * 2. A `TLIdentityNode` with a driver. In the simple case, it is directly driven by the aforementioned protocol
   *    nodes. The generated node (2) is used to drive the front-bus modules and receive commands to the accelerator
   *    system.
   * 3. Optionally, there may be DMA from the front-bus-associated modules, so those can be exposed here as well
   *    in the TileLink format.
   */
  def deriveTLSources(implicit p:Parameters) : (Any, RoccNode, Option[TLNode])
}
