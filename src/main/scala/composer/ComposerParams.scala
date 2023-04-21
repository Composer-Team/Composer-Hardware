package composer

import chipsalliance.rocketchip.config._
import composer.MemoryStreams._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.PgLevels
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._


// Composer-system parameters
case object ComposerSystemsKey extends Field[List[ComposerSystemParams]]
case object SystemName2IdMapKey extends Field[Map[String, Int]]

// Architecture parameters
//case object MaxChannelTransactionLenKey extends Field[Int]
case object TLInterconnectWidthBytes extends Field[Int]
// if we support a dedicated DMA port, provide the number of ID bits
case object CXbarMaxDegree extends Field[Int]
case object RequireInternalCommandRouting extends Field[Boolean]
case object CmdRespBusWidthBytes extends Field[Int]
case object PlatformPhysicalMemoryBytes extends Field[Long]
case object MaxInFlightMemTxsPerSource extends Field[Int]
case object CoreCommandLatency extends Field[Int] // this might need to be high to expand beyond one slr

case class ComposerCoreParams(memoryChannelParams: List[CChannelParams] = List(),
                              core_id: Int = 0, // for internal use
                              system_id: Int = 0, // for internal use
                             )

case class ComposerConstructor(composerCoreParams: ComposerCoreParams, composerCoreWrapper: ComposerCoreWrapper)

case class ComposerSystemParams(nCores: Int,
                                name: String,
                                buildCore: (ComposerConstructor, Parameters) => ComposerCore,
                                /**
                                  * In elements, per write channel, scaled by the number of bytes
                                  */
                                coreParams: ComposerCoreParams = ComposerCoreParams(),
                                channelQueueDepth: Int = 32,
                                canReceiveSoftwareCommands: Boolean = true,
                                canIssueCoreCommands: Boolean = false
                               )

object ComposerConstraintHint extends Enumeration {
  val DistributeCoresAcrossSLRs = Value
  type ConstraintHints = Value
}

case object ConstraintHintsKey extends Field[List[ComposerConstraintHint.type]]

class WithComposer(constraintHints: List[ComposerConstraintHint.type] = List.empty) extends Config((site, _, _) => {
  case ComposerSystemsKey => Seq()
  case TLInterconnectWidthBytes => 16
//  case MaxChannelTransactionLenKey => 1 << 30
  // Tile parameters
  // Page table levels. We set it higher than rocket chip default because we need to support large virtual addresses
  // spaces for some targets (e.g. Kria). Increasing the # of page table levels supports this, but makes no difference
  // in elaboration
  case PgLevels => 5
  case XLen => 64 // Applies to all cores
  case CmdRespBusWidthBytes => 4
  case MaxHartIdBits => 1 // log2Up(site(TilesLocated(InSubsystem)).map(_.tileParams.hartId).max+1)
  // Interconnect parameters
  case SystemBusKey => SystemBusParams(
    beatBytes = site(XLen) / 8,
    blockBytes = site(CacheBlockBytes))
  case ControlBusKey => //noinspection DuplicatedCode
    PeripheryBusParams(
    beatBytes = site(XLen) / 8,
    blockBytes = site(CacheBlockBytes),
    errorDevice = Some(BuiltInErrorDeviceParams(
      errorParams = DevNullParams(List(AddressSet(0x3000, 0xfff)), maxAtomic = site(XLen) / 8, maxTransfer = 4096))))
  case PeripheryBusKey => PeripheryBusParams(
    beatBytes = site(XLen) / 8,
    blockBytes = site(CacheBlockBytes),
    dtsFrequency = Some(100000000)) // Default to 100 MHz pbus clock
  case MemoryBusKey => MemoryBusParams(
    beatBytes = site(XLen) / 8,
    blockBytes = site(CacheBlockBytes))
  case FrontBusKey => FrontBusParams(
    beatBytes = site(XLen) / 8,
    blockBytes = site(CacheBlockBytes))

  // implementation hints
  case ConstraintHintsKey => constraintHints

  // Additional device Parameters
  case BootROMLocated(InSubsystem) => Some(BootROMParams(contentFileName = "./bootrom/bootrom.img"))
  case SubsystemExternalResetVectorKey => false
  case DebugModuleKey => Some(DefaultDebugModuleParams(site(XLen)))
  case CLINTKey => Some(CLINTParams())
  case PLICKey => Some(PLICParams())
  // Copying WithJustOneBus
  case TLNetworkTopologyLocated(InSubsystem) => List(
    JustOneBusTopologyParams(sbus = site(SystemBusKey))
  )
  case MonitorsEnabled => false
  case TileKey => RocketTileParams()
  case MaxInFlightMemTxsPerSource => 8
})

object ComposerParams {
  val SystemIDLengthKey = 4
  val CoreIDLengthKey = 8
}