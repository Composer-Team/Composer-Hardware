package composer

import chipsalliance.rocketchip.config._
import composer.ComposerConstraintHint.ComposerConstraintHint
import composer.Generation.BuildMode
import composer.MemoryStreams._
import composer.Platforms.BuildModeKey
import composer.Systems.{AccelCoreWrapper, AcceleratorCore}
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
case object PrefetchSourceMultiplicity extends Field[Int]
case object UseConfigAsOutputNameKey extends Field[Boolean]

// Architecture parameters
//case object MaxChannelTransactionLenKey extends Field[Int]
case object TLInterconnectWidthBytes extends Field[Int]
// if we support a dedicated DMA port, provide the number of ID bits
case object CXbarMaxDegree extends Field[Int]
case object CmdRespBusWidthBytes extends Field[Int]
case object PlatformPhysicalMemoryBytes extends Field[Long]
case object MaxInFlightMemTxsPerSource extends Field[Int]

// Platforms that require full bi-directional IO coherence must set this to true
case class CoherenceConfiguration(memParams: MasterPortParams, maxMemorySegments: Int)
case object HasCoherence extends Field[Option[CoherenceConfiguration]]
case object CoreCommandLatency
    extends Field[Int] // this might need to be high to expand beyond one slr

case class ComposerCoreParams(
    memoryChannelParams: List[CChannelParams] = List(),
    core_id: Int = 0, // for internal use
    system_id: Int = 0 // for internal use
)

case class CoreConstructor(
    composerCoreParams: ComposerCoreParams,
    composerCoreWrapper: AccelCoreWrapper
)

case class AcceleratorSystemConfig(
                                    nCores: Int,
                                    name: String,
                                    buildCore: (CoreConstructor, Parameters) => AcceleratorCore,
                                    /** In elements, per write channel, scaled by the number of bytes
      */
                                    coreParams: ComposerCoreParams = ComposerCoreParams(),
                                    canReceiveSoftwareCommands: Boolean = true,
                                    canIssueCoreCommandsTo: Seq[String] = Seq.empty,
                                    canSendDataTo: Seq[String] = Seq.empty
)

object ComposerConstraintHint extends Enumeration {
  val DistributeCoresAcrossSLRs, MemoryConstrained = Value
  type ComposerConstraintHint = Value
}

case object ConstraintHintsKey extends Field[List[ComposerConstraintHint.type]]

class WithComposer(
    constraintHints: List[ComposerConstraintHint] = List.empty,
    quiet: Boolean = false,
    useConfigAsOutputName: Boolean = false) extends Config((site, _, _) => {
      case ComposerQuiet            => quiet
      case AcceleratorSystems       => Seq()
      case TLInterconnectWidthBytes => 16
      case PgLevels             => 5
      case XLen                 => 64 // Applies to all cores
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
      case DebugModuleKey                  => Some(DefaultDebugModuleParams(site(XLen)))
      case CLINTKey                        => Some(CLINTParams())
      case PLICKey                         => Some(PLICParams())
      // Copying WithJustOneBus
      case TLNetworkTopologyLocated(InSubsystem) =>
        List(
          JustOneBusTopologyParams(sbus = site(SystemBusKey))
        )
      case MonitorsEnabled            => false
      case TileKey                    => RocketTileParams()
      case MaxInFlightMemTxsPerSource => 4
      case DRAMBankBytes              => 4 * 1024
    })

object ComposerParams {
  val SystemIDLengthKey = 4
  val CoreIDLengthKey = 10
}
