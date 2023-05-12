package composer

import chipsalliance.rocketchip.config._
import freechips.rocketchip.subsystem._

// memory
case object MMIOBaseAddress extends Field[Long]
case object HasDiscreteMemory extends Field[Boolean]
case object HasDMA extends Field[Option[Int]]

// mmio
case object HasAXILExternalMMIO extends Field[Boolean]
case object AXILSlaveAddressMask extends Field[Long]
case object AXILSlaveBeatBytes extends Field[Int]

// implementation specifics
case object PlatformSLRs extends Field[Option[Seq[SLRName]]]
case object PlatformNumSLRs extends Field[Int]
case object IsAWS extends Field[Boolean]

case object PostProcessorMacro extends Field[() => Unit]

case class SLRName(name: String,
                   frontBus: Boolean = false,
                   memoryBus: Boolean = false)

class WithAWSPlatform(nMemoryChannels: Int) extends Config((_, _, _) => {
  // why did this ever become 128? It's 2X the bus width... That doesn't seem to make much sense...
  //  case CacheBlockBytes => 128
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
  case HasAXILExternalMMIO => true
  // TODO this can be tuned
  case CXbarMaxDegree => 32
  case HasDiscreteMemory => true
  case AXILSlaveAddressMask => 0xFFFFL
  case MMIOBaseAddress => 0L
  case AXILSlaveBeatBytes => 4
  case CoreCommandLatency => 4

  case PlatformNumSLRs => 3
  case PlatformSLRs => Some(Seq(SLRName("pblock_CL_bot", frontBus = true), SLRName("pblock_CL_mid", memoryBus = true), SLRName("pblock_CL_top")))
  case IsAWS => true
  case PostProcessorMacro => () =>
    val cwd = ComposerBuild.composerVsimDir
    val cdir = ComposerBuild.composerBin
    val callable = os.proc(f"$cdir/aws-gen-build")
    callable.call(cwd = os.Path(cwd))
})

class WithKriaPlatform extends Config((_, _, _) => {
  case ExtMem => Some(MemoryPortParams(MasterPortParams(
    base = 0,
    size = 1L << 49,
    beatBytes = 16,
    idBits = 6
  ), 1))
  // 4GB total physical memory
  case PlatformPhysicalMemoryBytes => 4L << 30
  case MMIOBaseAddress =>      0x2000000000L
  case AXILSlaveAddressMask => 0xFFFFL
  case HasDMA => None
  // TODO this can be tuned
  case CXbarMaxDegree => 8
  case HasAXILExternalMMIO => false // use full AXI4
  case HasDiscreteMemory => false
  case AXILSlaveBeatBytes => 4
  case CoreCommandLatency => 0

  case PlatformNumSLRs => 1
  case PlatformSLRs => None
  case IsAWS => false
  case PostProcessorMacro => () => ;
})

class WithU200Platform() extends Config ( (_, _, _) => {
  case ExtMem =>
    val q = Some(MemoryPortParams(MasterPortParams(
      base = 0,
      size = 0x400000000L,
      beatBytes = 64,
      idBits = 6
    ), 1))
    q
  // 16GB memory per DIMM
  case PlatformPhysicalMemoryBytes => 16L << 30
  case HasDMA => Some(6)
  case HasAXILExternalMMIO => true
  // TODO this can be tuned
  case CXbarMaxDegree => 32
  case HasDiscreteMemory => true
  case AXILSlaveAddressMask => 0xFFFFL
  case MMIOBaseAddress => 0L
  case AXILSlaveBeatBytes => 4
  case CoreCommandLatency => 4

  case PlatformNumSLRs => 3
  case PlatformSLRs => Some(Seq(SLRName("0", frontBus = true), SLRName("1", memoryBus = true), SLRName("2")))
  case IsAWS => false
  case PostProcessorMacro => () => ;
})

object SLRConstants {
  final val DEFAULT_SLR = 0
}