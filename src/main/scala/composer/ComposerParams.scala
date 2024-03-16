package composer

import chipsalliance.rocketchip.config._
import composer.ComposerConstraintHint.ComposerConstraintHint
import composer.Generation.BuildMode
import composer.MemoryStreams._
import composer.Platforms.{BuildModeKey, Platform, PlatformKey}
import composer.Systems._
import composer.common.{AccelCommand, AccelResponse, AccelRoccCommand}
import freechips.rocketchip.amba.axi4.{AXI4MasterParameters, AXI4MasterPortParameters}
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.PgLevels
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._

// Composer-system parameters
case object AcceleratorSystems extends Field[List[AcceleratorSystemConfig]]

case object SystemName2IdMapKey extends Field[Map[String, Int]]

case object DRAMBankBytes extends Field[Int]

case object ComposerQuiet extends Field[Boolean]

case object UseConfigAsOutputNameKey extends Field[Boolean]

// Architecture parameters
case object CmdRespBusWidthBytes extends Field[Int]


case object MaxInFlightMemTxsPerSource extends Field[Int]

// Platforms that require full bi-directional IO coherence must set this to true
//case class CoherenceConfiguration(memParams: MasterPortParams, maxMemorySegments: Int)

//case object HasCoherence extends Field[Option[CoherenceConfiguration]]

// this might need to be high to expand beyond one slr

trait ModuleConstructor {}

case class BlackboxBuilderCustom(coreCommand: AccelCommand, coreResponse: AccelResponse) extends ModuleConstructor

case class BlackboxBuilderRocc() extends ModuleConstructor

case class ModuleBuilder(constructor: (ComposerSystem, Parameters) => AcceleratorCore) extends ModuleConstructor

case class AcceleratorSystemConfig(
                                    nCores: Int,
                                    name: String,

                                    /** In elements, per write channel, scaled by the number of bytes
                                     */
                                    moduleConstructor: ModuleConstructor,
                                    memoryChannelParams: List[CChannelParams] = List(),
                                    canReceiveSoftwareCommands: Boolean = true,
                                    canIssueCoreCommandsTo: Seq[String] = Seq.empty,
                                    canSendDataTo: Seq[String] = Seq.empty
                                  )

object ComposerConstraintHint extends Enumeration {
  val DistributeCoresAcrossSLRs, MemoryConstrained = Value
  type ComposerConstraintHint = Value
}

case object ConstraintHintsKey extends Field[List[ComposerConstraintHint.type]]

class WithComposer(platform: Platform,
                   constraintHints: List[ComposerConstraintHint] = List.empty,
                   quiet: Boolean = false,
                   useConfigAsOutputName: Boolean = false) extends Config((site, _, _) => {
  case ComposerQuiet => quiet
  case PlatformKey => platform
  case AcceleratorSystems => Seq()
  case PgLevels => 5
  case XLen => 64 // Applies to all cores
  // PrefetchSourceMultiplicity must comply with the maximum number of beats
  // allowed by the underlying protocl. For AXI4, this is 256
  case CmdRespBusWidthBytes => 4
  case UseConfigAsOutputNameKey => useConfigAsOutputName
  case MaxHartIdBits => 1
  // Interconnect parameters
  case SystemBusKey =>
    SystemBusParams(
      beatBytes = site(XLen) / 8,
      blockBytes = site(CacheBlockBytes)
    )
  case ControlBusKey => //noinspection DuplicatedCode
    PeripheryBusParams(
      beatBytes = site(XLen) / 8,
      blockBytes = site(CacheBlockBytes),
      errorDevice = Some(
        BuiltInErrorDeviceParams(
          errorParams = DevNullParams(
            List(AddressSet(0x3000, 0xfff)),
            maxAtomic = site(XLen) / 8,
            maxTransfer = 4096
          )
        )
      )
    )
  case PeripheryBusKey =>
    PeripheryBusParams(
      beatBytes = site(XLen) / 8,
      blockBytes = site(CacheBlockBytes),
      dtsFrequency = Some(100000000)
    ) // Default to 100 MHz pbus clock
  case MemoryBusKey =>
    MemoryBusParams(
      beatBytes = site(XLen) / 8,
      blockBytes = site(CacheBlockBytes)
    )
  case FrontBusKey =>
    FrontBusParams(
      beatBytes = site(XLen) / 8,
      blockBytes = site(CacheBlockBytes)
    )

  // implementation hints
  case ConstraintHintsKey => constraintHints

  // Additional device Parameters
  case BootROMLocated(InSubsystem) =>
    Some(BootROMParams(contentFileName = "./bootrom/bootrom.img"))
  case SubsystemExternalResetVectorKey => false
  case DebugModuleKey => Some(DefaultDebugModuleParams(site(XLen)))
  case CLINTKey => Some(CLINTParams())
  case PLICKey => Some(PLICParams())
  // Copying WithJustOneBus
  case TLNetworkTopologyLocated(InSubsystem) =>
    List(
      JustOneBusTopologyParams(sbus = site(SystemBusKey))
    )
  case MonitorsEnabled => false
  case TileKey => RocketTileParams()
  case MaxInFlightMemTxsPerSource => 1
  case DRAMBankBytes => 4 * 1024
})

object ComposerParams {
  val SystemIDLengthKey = 4
  val CoreIDLengthKey = 10
}
