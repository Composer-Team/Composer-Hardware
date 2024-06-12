package beethoven.Platforms

import chipsalliance.rocketchip.config._
import beethoven.Generation.BuildMode
import beethoven.Platforms.ASIC.MemoryCompiler
import beethoven.Platforms.PlatformType.PlatformType
import beethoven.Protocol.FrontBus.FrontBusProtocol
import freechips.rocketchip.subsystem.{MasterPortParams, MemoryPortParams}
import os.Path

object PlatformType extends Enumeration {
  val FPGA, ASIC = Value
  type PlatformType = Value
}

abstract class Platform {
  // platformType helps the generator choose dataflows and latencies that match the rough order of magnitude
  // expected for this platform type. In the future, we could use the clock rate parameter to perform much
  // more precise decision-making
  val platformType: PlatformType

  // Is the memory access from the FPGA discrete from the host memory
  val hasDiscreteMemory: Boolean

  /**
   * The front bus is the MMIO bus that the host uses to access accelerator cores. Most parameters are
   * self-explanatory except for some platforms (usually heavily resource constrained ones), the host
   * may access memory directly through Beethoven. This adds some latency but is not the primary concern
   * on such systems.
   */
  val frontBusBaseAddress: Long
  val frontBusAddressNBits: Int
  val frontBusAddressMask: Long
  val frontBusBeatBytes: Int
  val frontBusCanDriveMemory: Boolean
  val frontBusProtocol: FrontBusProtocol

  /**
   * These parameters describe the main memory access channel. Of note, the physical address space can
   * be smaller than the addressable space. This is the case, for instance, when certain parts of the
   * address space correspond to peripherals and don't actually correspond to main memory. The
   * `physicalMemoryBytes` parameter corresponds to the size of the physical memory space and
   * `memorySpaceSizeBytes` corresponds to the size of the whole addressable space.
   */
  val physicalMemoryBytes: Long
  val memorySpaceAddressBase: Long
  val memorySpaceSizeBytes: Long
  val memoryNChannels: Int
  val memoryControllerIDBits: Int
  val memoryControllerBeatBytes: Int

  /**
   * The rest of the parameters either correspond to parameters that are not typically used in generation
   * or are only useful or relevant for extreme fine-tuning. Clock rate can be important for ASIC memory
   * generation so we recommend setting a more precise value for ASIC platforms.
   */

  val clockRateMHz: Int = 100
  // when making a memory access, we instrument the access in one of two ways: as a long-burst or as a short burst
  // This parameter stimulates the length of the long burst. This is important to set on high-performance platforms
  // especially when there is strange addressing modes (usually stipulated by the memory controller) that affect
  // banking and expect a certain burst length to operate at full performance. Some platforms also may make certain
  // legal burst lengths illegal! This is the case on the Kria platform that, although burst length 64 is a valid
  // burst length in the AXI protocol spec, 16 is the maximum permissible burst supported by the controller.
  // Longer bursts are split into shorter AXI burst on Kria, which can lead to allocation problems and poor
  // performance under some circumstances. If this is a parameter of interest, it is usually find in the memory
  // controller documentation.
  val prefetchSourceMultiplicity: Int = 16
  val defaultReadTXConcurrency: Int = 4
  val defaultWriteTXConcurrency: Int = 4

  // maximum memory crossbar fanout degree. Larger xbars will be broken up into multiple layers.
  // This can be tuned for platforms where congestion is a primary concern
  val xbarMaxDegree = 2
  // maximum number of logical memory channels will be used to cross across SLRs
  val maxMemSLRcrossings = 1
  // maximum number of logical memory endpoints per system (read and write handled separately)
  val maxMemEndpointsPerSystem = 1

  val maxMemEndpointsPerCore = 1

  val interCoreMemReductionLatency = 1
  val intraCoreInterSLRMemReductionLatency = 2
  val crossSLRLatency = 2 // Only used in FPGAs

  val coreCommandLatency = 2

  def extMem: MemoryPortParams = MemoryPortParams(
    master=MasterPortParams(
      base=memorySpaceAddressBase,
      size=memorySpaceSizeBytes,
      beatBytes = memoryControllerBeatBytes,
      idBits = memoryControllerIDBits
    ), nMemoryChannels = memoryNChannels
  )
}

trait HasXilinxMem {
  val nURAMs: Int
  val nBRAMs: Int
}


trait MultiDiePlatform {
  /**
   * The dies are the physical units that the platform is divided into. Each die has a name and a set of
   * properties that describe the physical characteristics of the die. The order of the dies should
   * correspond to the physical connectivity: platformDies(0) is directly connected to platformDies(1).
   * We do not currently support more complex connectivities, but this could just as well be added.
   */
  val platformDies: Seq[DieName]
  /**
   * The placement affinity is the relative fraction of cores that should be placed on each die.
   * This is useful if you know that certain dies have existing IP that take up area on certain
   * dies, making resource contention more of a problem there. By default, we assign an equal
   * affinity to each die.
   */
  def placementAffinity: Seq[Int] = platformDies.map(_ => 1)
}

trait PlatformHasSeparateDMA {
  val DMAIDBits: Int
}

trait HasPostProccessorScript {
  def postProcessorMacro(c: Config, paths: Seq[Path]): Unit
}

trait HasMemoryCompiler {
  val memoryCompiler: MemoryCompiler
}

case object BuildModeKey extends Field[BuildMode]
case object PlatformKey extends Field[Platform]