package beethoven

import beethoven.common.Address
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

/**
 * Bundle used to communicate memory requests between user and memory manager
 */
class ChannelTransactionBundle(implicit p: Parameters) extends Bundle {
  val addr = Address()
  val len = UInt(log2Up(platform.physicalMemoryBytes).W)
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

abstract class ScratchpadSpecialization

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

case class PackedSubwordScratchpadConfig(wordSizeBits: Int, datsPerSubword: Int) extends ScratchpadSpecialization

/**
 * Special case of PackedSubwordScratchpadConfig for which there is no padding
 * Ex. 16b data type, 16b software word
 */
class FlatPackScratchpadConfig extends ScratchpadSpecialization

/**
 * Base type for Beethoven Memory Channel configurations. Subtypes may include special behaviors and interfaces.
 * i.e., scratchpads and caches
 */
trait MemChannelConfig {
  val name: String
  val nChannels: Int
}

case class MemCachedReadChannelConfig(name: String, nChannels: Int, cacheParams: CCacheParams) extends MemChannelConfig {
}

object ScratchpadSpecialization {
  def packedSubword(wordSizeBits: Int, datsPerSubword: Int): PackedSubwordScratchpadConfig = {
    PackedSubwordScratchpadConfig(wordSizeBits, datsPerSubword)
  }

  def flatPacked: FlatPackScratchpadConfig = new FlatPackScratchpadConfig
}
