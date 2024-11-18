package beethoven.Platforms

import beethoven.BuildMode
import chipsalliance.rocketchip.config._
import beethoven.Platforms.ASIC.TechLib
import beethoven.Platforms.ASIC.memoryCompiler.MemoryCompiler
import beethoven.Platforms.PlatformType.PlatformType
import beethoven.Protocol.FrontBus.FrontBusProtocol
import freechips.rocketchip.subsystem.{MasterPortParams, MemoryPortParams}
import os.Path

import scala.annotation.tailrec

object PlatformType extends Enumeration {
  val FPGA, ASIC = Value
  type PlatformType = Value
}

abstract class Platform {
  // platformType helps the generator choose dataflows and latencies that match the rough order of magnitude
  // expected for this platform type. In the future, we could use the clock rate parameter to perform much
  // more precise decision-making
  val platformType: PlatformType

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
  val frontBusProtocol: FrontBusProtocol

  /**
   * These parameters describe the main memory access channel. Of note, the physical address space can
   * be smaller than the addressable space. This is the case, for instance, when certain parts of the
   * address space correspond to peripherals and don't actually correspond to main memory. The
   * `physicalMemoryBytes` parameter corresponds to the size of the physical memory space and
   * `memorySpaceSizeBytes` corresponds to the size of the whole addressable space.
   */
  // Is the memory access from the FPGA discrete from the host memory
  val hasDiscreteMemory: Boolean
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
  // maximum number of logical memory endpoints per system (read and write handled separately)
  val maxMemEndpointsPerSystem = 1
  val maxMemEndpointsPerCore = 1
  val interCoreMemReductionLatency = 1
  val interCoreMemBusWidthBytes = 4

  /**
   * These default values should typically be fine.
   */
  val net_intraDeviceXbarLatencyPerLayer = 1
  val net_intraDeviceXbarTopLatency = 1
  val net_fpgaSLRBridgeLatency = 2

  val memEndpointsPerDevice = 1

  def extMem: MemoryPortParams = MemoryPortParams(
    master = MasterPortParams(
      base = memorySpaceAddressBase,
      size = memorySpaceSizeBytes,
      beatBytes = memoryControllerBeatBytes,
      idBits = memoryControllerIDBits
    ), nMemoryChannels = memoryNChannels
  )

  // multi-die stuff

  def placementAffinity: Map[Int, Double] = Map.from(physicalDevices.map { dev => (dev.identifier, 1.0 / physicalDevices.length) })

  val physicalDevices: List[DeviceConfig] = List(DeviceConfig(0, ""))
  val physicalInterfaces: List[PhysicalInterface] = List(PhysicalHostInterface(0), PhysicalMemoryInterface(0, 0))
  val physicalConnectivity: List[(Int, Int)] = List()

  private[beethoven] def platformCheck(): Unit = {
    assert(physicalInterfaces.exists(_.isInstanceOf[PhysicalHostInterface]),
      "Platform must have at least one host interface")
    assert(physicalInterfaces.exists(_.isInstanceOf[PhysicalMemoryInterface]),
      "Platform must have at least one memory interface")
    assert(physicalDevices.nonEmpty, "Platform must have at least one device")
  }

  def getConnectivityFromDeviceID(id: Int): List[Int] = {
    physicalConnectivity.filter(a => a._1 == id || a._2 == id).map {
      case (x, a) if x == id => a
      case (a, x) if x == id => a
    }
  }

}

abstract class PhysicalInterface {
  val locationDeviceID: Int
}

case class PhysicalMemoryInterface(locationDeviceID: Int, channelIdx: Int) extends PhysicalInterface

case class PhysicalHostInterface(locationDeviceID: Int) extends PhysicalInterface

case class DeviceRequirements(memory: Boolean,
                              sw_commands: Boolean,
                              hw_commands_src: Boolean,
                              ocmemory_src: Boolean,
                              ocmemory_sink: Boolean) {
  def ||(other: DeviceRequirements): DeviceRequirements = {
    DeviceRequirements(memory || other.memory,
      sw_commands || other.sw_commands,
      hw_commands_src || other.hw_commands_src,
      ocmemory_src || other.ocmemory_src,
      ocmemory_sink || other.ocmemory_sink)
  }
}

object DeviceRequirements {
  val empty = DeviceRequirements(false, false, false, false, false)
}

trait HasXilinxMem {
  val nURAMs: Map[Int, Int]
  val nBRAMs: Map[Int, Int]
}


trait PlatformHasSeparateDMA {
  val DMAIDBits: Int
}

trait HasPostProccessorScript {
  def postProcessorMacro(c: Parameters, paths: Seq[Path]): Unit
}

trait HasTechLib {
  val techLib: TechLib
}

trait HasMemoryCompiler {
  val memoryCompiler: MemoryCompiler
}

case object BuildModeKey extends Field[BuildMode]

case object PlatformKey extends Field[Platform]