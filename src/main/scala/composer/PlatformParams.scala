package composer

import chipsalliance.rocketchip.config._
import composer.FrontBusProtocol.FrontBusProtocol
import composer.PlatformType.PlatformType
import freechips.rocketchip.subsystem._

object PlatformType extends Enumeration {
  val FPGA, ASIC = Value
  type PlatformType = Value
}

case object PlatformTypeKey extends Field[PlatformType]

object FrontBusProtocol extends Enumeration {
  val AHB, AXIL, AXI4 = Value
  type FrontBusProtocol = Value
}


/** **** MEMORY ***** */
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

// implementation specifics
case object PlatformSLRs extends Field[Option[Seq[SLRName]]]

case object PlatformNumSLRs extends Field[Int]

case object IsAWS extends Field[Boolean]

case object PostProcessorMacro extends Field[() => Unit]

case class SLRName(name: String,
                   frontBus: Boolean = false,
                   memoryBus: Boolean = false)

class WithKriaPlatform(nMemoryChannels: Int = 1) extends Config((_, _, _) => {
  case ExtMem => Some(MemoryPortParams(MasterPortParams(
    base = 0,
    size = 1L << 49,
    beatBytes = 16,
    idBits = 6
  ), nMemoryChannels))
  // 4GB total physical memory
  case PlatformPhysicalMemoryBytes => 4L << 30
  case FrontBusBaseAddress => 0x2000000000L
  case FrontBusAddressMask => 0xFFFFL
  case HasDMA => None
  // TODO this can be tuned
  case CXbarMaxDegree => 8
  case HasDiscreteMemory => false
  case FrontBusBeatBytes => 4
  case CoreCommandLatency => 0

  case PlatformTypeKey => PlatformType.FPGA
  case FrontBusProtocolKey => FrontBusProtocol.AXI4
  case PlatformNBRAM => 144
  case PlatformNURAM => 64
  case DefaultClockRateKey => 100
  case PlatformNumSLRs => 1
  case PlatformSLRs => None

  case IsAWS => false
  case PostProcessorMacro => () => ;
  case HasDisjointMemoryControllers => false
})

private[composer] class U200Base(nMemoryChannels: Int) extends Config((_, _, _) => {
  case PlatformNumSLRs => 3
  case ExtMem =>
    val q = Some(MemoryPortParams(MasterPortParams(
      base = 0,
      size = 0x400000000L,
      beatBytes = 64,
      idBits = 6
    ), nMemoryChannels))
    require(1 <= nMemoryChannels && nMemoryChannels <= 4)
    q
  // 16GB memory per DIMM
  case PlatformPhysicalMemoryBytes => (16L << 30) * nMemoryChannels
  case HasDMA => Some(6)
  // TODO this can be tuned
  case CXbarMaxDegree => 32
  case HasDiscreteMemory => true

  case PlatformTypeKey => PlatformType.FPGA
  case FrontBusProtocolKey => FrontBusProtocol.AXIL
  case FrontBusAddressMask => 0xFFFFL
  case FrontBusBaseAddress => 0L
  case FrontBusBeatBytes => 4
  case PlatformNURAM => 960
  case PlatformNBRAM => 2160


  case CoreCommandLatency => 4
  case HasDisjointMemoryControllers => true

})

private[composer] class U200_sole() extends Config((_, _, _) => {
  case PlatformSLRs => Some(Seq(SLRName("0", frontBus = true), SLRName("1", memoryBus = true), SLRName("2")))
  case DefaultClockRateKey => 300
  case IsAWS => false
  case PostProcessorMacro => () => ;
})

private[composer] class AWS_sole(simulation: Boolean) extends Config((_, _, _) => {
  // why did this ever become 128? It's 2X the bus width... That doesn't seem to make much sense...
  //  case CacheBlockBytes => 128
  case PlatformSLRs => Some(Seq(SLRName("pblock_CL_bot", frontBus = true), SLRName("pblock_CL_mid", memoryBus = true), SLRName("pblock_CL_top")))
  case DefaultClockRateKey => 125
  case IsAWS => true
  case PostProcessorMacro => () =>
    if (!simulation) {
      val cwd = ComposerBuild.composerVsimDir
      val cdir = ComposerBuild.composerBin
      val callable = os.proc(f"$cdir/aws-gen-build")
      callable.call(cwd = os.Path(cwd), stdin = os.Inherit, stdout = os.Inherit)
    }
})

class WithU200Platform extends Config(new U200Base(1) ++ new U200_sole)

class WithAWSPlatform(nMemoryChannels: Int, simulation: Boolean = true) extends Config(new U200Base(nMemoryChannels) ++ new AWS_sole(simulation))

object SLRConstants {
  final val DEFAULT_SLR = 0
}

class WithChipKitPlatform extends Config((_, _, _) => {
  case ExtMem => Some(MemoryPortParams(MasterPortParams(
    base = 0,
    size = 1L << 34,
    beatBytes = 16,
    idBits = 6
  ), 1))
  // 4GB total physical memory
  case PlatformPhysicalMemoryBytes => 1L << 34
  case FrontBusBaseAddress => 0x2000000000L
  case FrontBusAddressMask => 0xFFFFL
  case HasDMA => None
  case CXbarMaxDegree => 8
  case HasDiscreteMemory => false
  case FrontBusBeatBytes => 4
  case CoreCommandLatency => 0

  case PlatformTypeKey => PlatformType.ASIC
  case FrontBusProtocolKey => FrontBusProtocol.AHB
  case DefaultClockRateKey => 100

  case IsAWS => false
  case PostProcessorMacro => () => ;
  case HasDisjointMemoryControllers => false

})