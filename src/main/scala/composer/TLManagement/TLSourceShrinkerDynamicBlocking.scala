// See LICENSE.SiFive for license details.

package composer.TLManagement

import Chisel._
import chipsalliance.rocketchip.config._
import composer.platform
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class TLSourceShrinkerDynamicBlocking(maxNIDs: Int)(implicit p: Parameters) extends LazyModule {
  require(maxNIDs > 0)

  private def noShrinkRequired(client: TLClientPortParameters) = maxNIDs >= client.endSourceId

  // The SourceShrinker completely destroys all FIFO property guarantees
  private val client = TLMasterParameters.v1(
    name = "TLSourceShrinker2",
    sourceId = IdRange(0, maxNIDs))
  val node = new TLAdapterNode(
    clientFn = { cp =>
      if (noShrinkRequired(cp)) {
        cp
      } else {
        // We erase all client information since we crush the source Ids
        TLMasterPortParameters.v1(
          clients = Seq(client.v1copy(requestFifo = cp.clients.exists(_.requestFifo))),
          echoFields = cp.echoFields,
          requestFields = cp.requestFields,
          responseKeys = cp.responseKeys)
      }
    },
    managerFn = { mp => mp.v1copy(managers = mp.managers.map(m => m.v1copy(fifoId = if (maxNIDs == 1) Some(0) else m.fifoId)))
    }) {
    override def circuitIdentity = edges.in.map(_.client).forall(noShrinkRequired)
  }

  lazy val module = new Impl

  class Impl extends LazyModuleImp(this) {
    node.in.zip(node.out).foreach { case ((in, edgeIn), (out, edgeOut)) =>
      // Acquires cannot pass this adapter; it makes Probes impossible
      require(!edgeIn.client.anySupportProbe || !edgeOut.manager.anySupportAcquireB)

      out.b.ready := Bool(true)
      out.c.valid := Bool(false)
      out.e.valid := Bool(false)
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)

      if (noShrinkRequired(edgeIn.client)) {
        out.a <> in.a
        in.d <> out.d
      } else {
        val sourceOut2InMap = Reg(Vec(maxNIDs, UInt(width = log2Up(edgeIn.client.endSourceId).W)))

        val allocated = Reg(Vec(maxNIDs, Bool()))
        val beatsLeftPerAllocation = Reg(Vec(maxNIDs,
          UInt(log2Up((edgeOut.manager.maxTransfer / edgeOut.manager.beatBytes) + 1).W)))
        val d_last = beatsLeftPerAllocation(out.d.bits.source) === UInt(1)
        val nextFree = PriorityEncoder(~allocated)
        val full = allocated.andR

        when(reset.asBool) {
          allocated.foreach(_ := false.B)
        }

        val a_in_valid = RegInit(false.B)
        val a_in = Reg(in.a.bits)
        out.a.valid := a_in_valid
        out.a.bits := a_in

        // need to count beats in the transaction because we might follow two transactions back to back with each other

        val handlingLongWriteTx = RegInit(false.B)
        val prevSourceMap = Reg(UInt(out.params.sourceBits.W))
        val prevSource = Reg(UInt(in.params.sourceBits.W))
        val singleBeatLgSz = log2Up(in.a.bits.data.getWidth/8)
        val isTxContinuation = handlingLongWriteTx && prevSource === in.a.bits.source
        val longBeatCount = Reg(UInt(log2Up(platform.prefetchSourceMultiplicity).W))

        in.a.ready := (((a_in_valid && out.a.fire) || (!a_in_valid)) && !full) || isTxContinuation

        when(in.a.fire) {
          a_in := in.a.bits
          a_in_valid := true.B
          prevSource := in.a.bits.source
          when (isTxContinuation) {
            a_in.source := prevSourceMap
            longBeatCount := longBeatCount + 1.U
            when (longBeatCount === (platform.prefetchSourceMultiplicity-1).U) {
              handlingLongWriteTx := false.B
            }
          }.otherwise {
            when(in.a.bits.opcode === 0.U && in.a.bits.size > singleBeatLgSz.U) {
              handlingLongWriteTx := true.B
              longBeatCount := 1.U
            }.otherwise {
              handlingLongWriteTx := false.B
            }
            allocated(nextFree) := true.B
            sourceOut2InMap(nextFree) := in.a.bits.source
            a_in.source := nextFree
            beatsLeftPerAllocation(nextFree) :=
              Mux(in.a.bits.opcode === 0.U,
                1.U, // if write then we only expect 1 write response
                1.U << (in.a.bits.size - log2Up(edgeOut.manager.beatBytes).U)) // if read, then many responses
            assert(in.a.bits.size >= log2Up(edgeOut.manager.beatBytes).U, "TLSourceShrinker2: Request too small")
          }
        }
        when(out.a.fire) {
          a_in_valid := in.a.valid
        }

        val d_in = Reg(in.d.bits)
        val d_in_valid = RegInit(false.B)
        in.d.bits := d_in
        in.d.valid := d_in_valid
        out.d.ready := (d_in_valid && in.d.ready) || !d_in_valid
        when(in.d.fire) {
          d_in_valid := false.B
        }
        when(out.d.fire) {
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

object TLSourceShrinkerDynamicBlocking {
  def apply(maxInFlight: Int, suggestedName: Option[String] = None)(implicit p: Parameters): TLNode = {
    val shrinker = LazyModule(new TLSourceShrinkerDynamicBlocking(maxInFlight))
    shrinker.suggestName(suggestedName)
    shrinker.node
  }
}
