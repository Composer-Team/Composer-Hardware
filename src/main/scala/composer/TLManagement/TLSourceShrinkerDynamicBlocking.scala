// See LICENSE.SiFive for license details.

package composer.TLManagement

import Chisel._
import chipsalliance.rocketchip.config._
import composer.Generation.ComposerBuild
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class TLSourceShrinkerDynamicBlocking(maxInFlight: Int)(implicit p: Parameters) extends LazyModule
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
      require (!edgeIn.client.anySupportProbe || !edgeOut.manager.anySupportAcquireB)

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
        val sourceOut2InMap = Reg(Vec(maxInFlight, UInt(width = log2Up(edgeIn.client.endSourceId).W)))
        val allocated = Reg(Vec(maxInFlight, Bool()))
        val beatsLeftPerAllocation = Reg(Vec(maxInFlight, UInt(log2Up((edgeOut.manager.maxTransfer / edgeOut.manager.beatBytes) + 1).W)))
        val d_last = beatsLeftPerAllocation(out.d.bits.source) === UInt(1)
        val nextFree = PriorityEncoder(~allocated)
        val full = allocated.andR

        when (reset.asBool) {
          allocated.foreach (_ := false.B)
        }

        val a_in_valid = RegInit(false.B)
        val a_in = Reg(in.a.bits)
        out.a.valid := a_in_valid
        out.a.bits := a_in
        in.a.ready := ((a_in_valid && out.a.fire) || (!a_in_valid)) && !full

        when (in.a.fire) {
          a_in := in.a.bits
          a_in.source := nextFree
          a_in_valid := true.B
          sourceOut2InMap(nextFree) := in.a.bits.source
          allocated(nextFree) := true.B
          beatsLeftPerAllocation(nextFree) := 1.U << (in.a.bits.size - log2Up(edgeOut.manager.beatBytes).U)
          assert(in.a.bits.size >= log2Up(edgeOut.manager.beatBytes).U, "TLSourceShrinker2: Request too small")
        }
        when(out.a.fire) {
          a_in_valid := in.a.valid
        }

        val d_in = Reg(in.d.bits)
        val d_in_valid = RegInit(false.B)
        in.d.bits := d_in
        in.d.valid := d_in_valid
        out.d.ready := (d_in_valid && in.d.ready) || !d_in_valid
        when (in.d.fire) {
          d_in_valid := false.B
        }
        when (out.d.fire) {
          d_in := out.d.bits
          d_in_valid := true.B
          d_in.source := sourceOut2InMap(out.d.bits.source)
          beatsLeftPerAllocation(out.d.bits.source) := beatsLeftPerAllocation(out.d.bits.source) - 1.U
          assert(beatsLeftPerAllocation(out.d.bits.source) =/= 0.U)
          when(d_last) {
            allocated(out.d.bits.source) := false.B
          }
        }

      }
    }
  }
}

object TLSourceShrinkerDynamicBlocking
{
  def apply(maxInFlight: Int)(implicit p: Parameters): TLNode =
  {
    val shrinker = LazyModule(new TLSourceShrinkerDynamicBlocking(maxInFlight))
    shrinker.node
  }
}
