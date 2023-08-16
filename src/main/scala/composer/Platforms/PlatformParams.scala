package composer.Platforms

import chipsalliance.rocketchip.config._
import composer.Generation.{BuildMode, ComposerBuild}
import composer.Platforms.ASIC.MemoryCompiler
import composer.Platforms.FrontBusProtocol.FrontBusProtocol
import composer.Platforms.PlatformType.PlatformType

object PlatformType extends Enumeration {
  val FPGA, ASIC = Value
  type PlatformType = Value
}

case object PlatformTypeKey extends Field[PlatformType]

object FrontBusProtocol extends Enumeration {
  val AHB, AXIL, AXI4 = Value
  type FrontBusProtocol = Value
}

/** **** MEMORY *****
 */
case object HasDiscreteMemory extends Field[Boolean]

case object HasDMA extends Field[Option[Int]]

case object HasDisjointMemoryControllers extends Field[Boolean]

// memory capacities
case object PlatformNBRAM extends Field[Int]

case object PlatformNURAM extends Field[Int]

// mmio
case object FrontBusBaseAddress extends Field[Long]

case object FrontBusAddressMask extends Field[Long]

case object FrontBusBeatBytes extends Field[Int]

case object FrontBusProtocolKey extends Field[FrontBusProtocol]

// default clock rates (MHz)(used in simulation) - can be overriden
case object DefaultClockRateKey extends Field[Int]


case object PlatformNumSLRs extends Field[Int]

case object PlatformPreferedSLRCmdRespRoutingPath extends Field[Option[Seq[String]]]

case object IsAWS extends Field[Boolean]

case object PostProcessorMacro extends Field[Config => Unit]

case object ASICMemoryCompilerKey extends Field[MemoryCompiler]

case object BuildModeKey extends Field[BuildMode]




