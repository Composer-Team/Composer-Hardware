package composer.MemoryStreams

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import composer.MemoryStreams.CChannelType.CChannelType
import composer._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.ExtMem

/**
  * Bundle used to communicate memory requests between user and memory manager
  */
class ChannelTransactionBundle(implicit p: Parameters) extends Bundle {
  val addr = UInt(log2Up(p(ExtMem).get.nMemoryChannels * p(ExtMem).get.master.size).W)
  val len = UInt(log2Up(p(MaximumTransactionLength)).W)
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


object CChannelType extends Enumeration {
  type CChannelType = Value
  val ReadChannel, WriteChannel = Value
  private[composer] val CacheChannel, Scratchpad = Value
}

/**
  * Base type for Composer Memory Channel configurations. Subtypes may include special behaviors and interfaces.
  * i.e., scratchpads and caches
  *
  * @param name      The name of the channel
  * @param nChannels number of memory access channels of this type
  * @param location  location of access
  */
abstract case class CChannelParams(name: String, nChannels: Int, channelType: CChannelType.CChannelType, location: String = "Mem")

/**
  * Read Channel group
  *
  * @param name      The name of the channel
  * @param nChannels number of memory access channels of this type
  * @param location  location of access
  */
class CReadChannelParams(name: String, nChannels: Int, val maxInFlightTxs: Int = 1, location: String = "Mem") extends CChannelParams(name, nChannels, CChannelType.ReadChannel, location)

object CReadChannelParams {
  def apply(name: String, nChannels: Int, maxInFlightTxs: Int = 1, location: String = "Mem"): CReadChannelParams =
    new CReadChannelParams(name, nChannels, maxInFlightTxs, location)
}

/**
  * Write channel group
  *
  * @param name           The name of the channel
  * @param nChannels      number of memory access channels of this type
  * @param maxInFlightTxs maximum number of AXI/TileLink memory transactions can be inflight per writer module at once
  * @param location       location of access
  */
class CWriteChannelParams(name: String, nChannels: Int, val maxInFlightTxs: Int = 2, location: String = "Mem") extends CChannelParams(name, nChannels, CChannelType.WriteChannel, location)

object CWriteChannelParams {
  def apply(name: String, nChannels: Int, maxInFlightTxs: Int = 2, location: String = "Mem"): CWriteChannelParams =
    new CWriteChannelParams(name, nChannels, maxInFlightTxs, location)
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

class CCachedReadChannelParams(name: String, nChannels: Int, val cacheParams: CCacheParams, location: String = "Mem")
  extends CChannelParams(name, nChannels, CChannelType.CacheChannel, location)

object CCachedReadChannelParams {
  def apply(name: String, nChannels: Int, cacheParams: CCacheParams, location: String = "Mem"): CChannelParams =
    new CCachedReadChannelParams(name, nChannels, cacheParams, location)
}

class CScratchpadChannelParams(name: String,
                               val supportWriteback: Boolean,
                               val dataWidthBits: Int,
                               val nDatas: Int,
                               val latency: Int = 2,
                               val supportReadLength: Int = 1 << 15,
                               val specialization: CScratchpadSpecialization = CScratchpadSpecialization.flatPacked)
  extends CChannelParams(name, nChannels = 1, channelType = CChannelType.Scratchpad) {
  private[composer] def make(implicit p: Parameters): CScratchpad = {
    new CScratchpad(supportWriteback, dataWidthBits, nDatas, latency, supportReadLength, specialization)
  }
}

object CScratchpadChannelParams {
  def apply(name: String,
            supportWriteback: Boolean,
            dataWidthBits: Int,
            nDatas: Int,
            latency: Int = 2,
            supportReadLength: Int = 1 << 15,
            specialization: CScratchpadSpecialization = CScratchpadSpecialization.flatPacked): CScratchpadChannelParams =
    new CScratchpadChannelParams(name, supportWriteback, dataWidthBits, nDatas, latency, supportReadLength, specialization)
}