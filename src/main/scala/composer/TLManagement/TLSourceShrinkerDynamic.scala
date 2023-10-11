// See LICENSE.SiFive for license details.

package composer.TLManagement

import Chisel._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class TLSourceShrinkerDynamic(maxInFlight: Int)(implicit p: Parameters) extends LazyModule
{
  require (maxInFlight > 0)
  private def noShrinkRequired(client: TLClientPortParameters) = maxInFlight >= client.endSourceId

  // The SourceShrinker completely destroys all FIFO property guarantees
  private val client = TLMasterParameters.v1(
    name     = "TLSourceShrinker2",
    sourceId = IdRange(0, maxInFlight))
  val node = new TLAdapterNode(
    clientFn  = { cp => if (noShrinkRequired(cp)) { cp } else {
      // We erase all client information since we crush the source Ids
      TLMasterPortParameters.v1(
        clients = Seq(client.v1copy(requestFifo = cp.clients.exists(_.requestFifo))),
        echoFields = cp.echoFields,
        requestFields = cp.requestFields,
        responseKeys = cp.responseKeys)
    }},
    managerFn = { mp => mp.v1copy(managers = mp.managers.map(m => m.v1copy(fifoId = if (maxInFlight==1) Some(0) else m.fifoId)))
    }) {
    override def circuitIdentity = edges.in.map(_.client).forall(noShrinkRequired)
  }

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    node.in.zip(node.out).foreach { case ((in, edgeIn), (out, edgeOut)) =>
      // Acquires cannot pass this adapter; it makes Probes impossible
      require (!edgeIn.client.anySupportProbe ||
        !edgeOut.manager.anySupportAcquireB)

      out.b.ready := Bool(true)
      out.c.valid := Bool(false)
      out.e.valid := Bool(false)
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)

      if (noShrinkRequired(edgeIn.client)) {
        out.a <> in.a
        in.d <> out.d
        println("No shrink required")
      } else {
        println("Shrinking because we need " + edgeIn.client.endSourceId + " clients")
        // map source ids from the side that wants shrinkage to the shrunken space id
        val sourceIn2OutMap = Reg(Vec(edgeIn.client.endSourceId, UInt(width = log2Up(maxInFlight).W)))
        val sourceOut2InMap = Reg(Vec(maxInFlight, UInt(width = log2Up(edgeIn.client.endSourceId).W)))
        val sourceInIdMapValid = Reg(Vec(edgeIn.client.endSourceId, Bool()))
        val allocated = Reg(Vec(maxInFlight, Bool()))
        when (reset.asBool) {
          allocated.foreach (_ := false.B)
          sourceInIdMapValid.foreach (_ := false.B)
        }
        val nextFree = PriorityEncoder(~allocated)
        val full = allocated.andR


        val a_first = !sourceInIdMapValid(in.a.bits.source)
        val beatsLeftPerAllocation = Reg(Vec(maxInFlight, UInt(log2Up((edgeOut.manager.maxTransfer / edgeOut.manager.beatBytes)+1).W)))
        println("Largest tx is " + edgeOut.manager.maxTransfer)

        val d_last = beatsLeftPerAllocation(out.d.bits.source) === UInt(1)

        val block = a_first && full
        in.a.ready := out.a.ready && !block
        out.a.valid := in.a.valid && !block
        out.a.bits := in.a.bits
        out.a.bits.source := Mux(a_first, nextFree, sourceIn2OutMap(in.a.bits.source))

        in.d <> out.d
        in.d.bits.source := sourceOut2InMap(out.d.bits.source)

        when (a_first && in.a.fire) {
          sourceIn2OutMap(in.a.bits.source) := nextFree
          sourceInIdMapValid(in.a.bits.source) := true.B
          sourceOut2InMap(nextFree) := in.a.bits.source
          allocated(nextFree) := true.B
          // has to be big enough for 16 - log2up(beatbytes)
          beatsLeftPerAllocation(nextFree) := 1.U << (in.a.bits.size - log2Up(edgeOut.manager.beatBytes).U)
          assert(in.a.bits.size >= log2Up(edgeOut.manager.beatBytes).U, "TLSourceShrinker2: Request too small")
        }

        when (in.d.fire) {
          beatsLeftPerAllocation(out.d.bits.source) := beatsLeftPerAllocation(out.d.bits.source) - 1.U
          assert(sourceInIdMapValid(sourceOut2InMap(out.d.bits.source)))
          assert(beatsLeftPerAllocation(out.d.bits.source) =/= 0.U)
        }

        when (in.d.fire && d_last) {
          // free the allocation
          val allocation = out.d.bits.source
          allocated(allocation) := false.B
          // mark the allocator as not having a valid in->out mapping
          val allocator = sourceOut2InMap(allocation)
          sourceInIdMapValid(allocator) := false.B
        }
      }
    }
  }
}

object TLSourceShrinkerDynamic
{
  def apply(maxInFlight: Int)(implicit p: Parameters): TLNode =
  {
    val shrinker = LazyModule(new TLSourceShrinkerDynamic(maxInFlight))
    shrinker.node
  }
}
