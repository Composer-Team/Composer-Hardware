package composer.MemoryStreams


import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer._
import freechips.rocketchip.subsystem.ExtMem

/**
 * Bundle used to communicate memory requests between user and memory manager
 */
class ChannelTransactionBundle(implicit p: Parameters) extends Bundle {
  val addr = UInt(log2Up(p(ExtMem).get.nMemoryChannels * p(ExtMem).get.master.size).W)
  val len = UInt(log2Up(p(PlatformPhysicalMemoryBytes)).W)
}

/**
 * Parameters to configure a read-only cache between a memory manager and memory.
 * WARNING: large caches may fail to pass timing
 *
 * @param sizeBytes     total cache size in bytes
 * @param idxMask       optionally can provide a custom layout for choosing index bits. This may be useful for
 *                      preventing conflict misses in channels that are expected to be strided access
 * @param associativity cache associativity
 */
case class CCacheParams(sizeBytes: Int,
                        idxMask: Option[Long] = None,
                        associativity: Int = 1)

abstract class CScratchpadSpecialization

/**
 * The RocketChip interfaces we use deal in bytes, which may not fit well with the bit-granularity that you may
 * want from your scratchpad memory. Subword loader takes a subword length (in bits), which must evenly divide
 * the cache block size (64B usually) and unpack as many bit-granularity outputs as possible from the payload.
 * Ex. 1B subword, 3b outputs, let A, B, C, ... be outputs that will be written to the scratchpad and 'O' be bits that
 * are ignored.
 * AAABBBOO CCCDDDOO EEEFFFOO ... until the end of the cache line
 * 0        1        2
 *
 * Ex. 2B subword, 3b outputs
 * AAABBBCC CDDDEEEO FFFGGGHH HIIIJJJO ... until the end of the cache line
 * 0        1        2        3
 *
 * A larger subword is more efficient but may be harder to pack together in software.
 *
 * @param wordSizeBits   width of the software word that the datOutWidth fits into
 * @param datsPerSubword how many dats of width 'dataWidthBits' are there per word
 *
 */

case class PackedSubwordScratchpadParams(wordSizeBits: Int, datsPerSubword: Int) extends CScratchpadSpecialization

/**
 * Special case of PackedSubwordScratchpadParams for which there is no padding
 * Ex. 16b data type, 16b software word
 */
class FlatPackScratchpadParams extends CScratchpadSpecialization

/**
 * Base type for Composer Memory Channel configurations. Subtypes may include special behaviors and interfaces.
 * i.e., scratchpads and caches
 */
trait CChannelParams {
  val name: String
  val nChannels: Int
}

/**
 * Read Channel group
 *
 * @param name      The name of the channel
 * @param nChannels number of memory access channels of this type
 * @param maxInFlightTxs maximum number of AXI/TileLink memory transactions can be inflight per reader module at once
 */
case class CReadChannelParams(name: String, nChannels: Int, maxInFlightTxs: Int = 2) extends CChannelParams {
  require(maxInFlightTxs > 1, s"Max In Flight Transactions must be greater than 1. Got: $maxInFlightTxs")
}

/**
 * Write channel group
 *
 * @param name           The name of the channel
 * @param nChannels      number of memory access channels of this type
 * @param maxInFlightTxs maximum number of AXI/TileLink memory transactions can be inflight per writer module at once
 */
case class CWriteChannelParams(name: String, nChannels: Int, maxInFlightTxs: Int = 2) extends CChannelParams {
}

/**
 * All Scratchpad specializtion types should announce themselves in this object
 */
object CScratchpadSpecialization {
  def packedSubword(wordSizeBits: Int, datsPerSubword: Int): PackedSubwordScratchpadParams = {
    PackedSubwordScratchpadParams(wordSizeBits, datsPerSubword)
  }

  def flatPacked: FlatPackScratchpadParams = new FlatPackScratchpadParams
}

case class CCachedReadChannelParams(name: String, nChannels: Int, cacheParams: CCacheParams) extends CChannelParams {
}

case class CScratchpadParams(name: String,
                             supportWriteback: Boolean,
                             dataWidthBits: Number,
                             nDatas: Number,
                             latency: Number = 3,
                             nPorts: Int = 2,
                             supportMemRequest: Boolean = true,
                             specialization: CScratchpadSpecialization = CScratchpadSpecialization.flatPacked,
                             datasPerCacheLine: Int = 1)
  extends CChannelParams {
  override val nChannels: Int = 1
  private[composer] def make(implicit p: Parameters): MemoryScratchpad = {
    new MemoryScratchpad(this)
  }
}

case class CIntraCoreMemoryPortIn(name: String,
                                  nChannels: Int,
                                  dataWidthBits: Number,
                                  nDatas: Number,
                                  latency: Number = 2) extends CChannelParams {
  require(isPow2(dataWidthBits.intValue()) && dataWidthBits.intValue() >= 8, "the width of CIntraCoreMemory ports is" +
    "currently restricted to power-of-2 sizes. If you need this changed, please contact developer.")
}

case class CIntraCoreMemoryPortOut(name: String,
                                   toSystem: String,
                                   toMemoryPort: String) extends CChannelParams {
  // this has to be a bit special because this port is coupled to another existing port declared somewhere else
  override val nChannels: Int = -1
}