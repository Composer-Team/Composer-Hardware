package beethoven

import beethoven.Platforms._
import beethoven.common._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.PgLevels
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._

// Beethoven-system parameters
case object AcceleratorSystems extends Field[List[AcceleratorSystemConfig]]

case object DRAMBankBytes extends Field[Int]

case object BQuiet extends Field[Boolean]

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

case class ModuleBuilder(constructor: Parameters => AcceleratorCore) extends ModuleConstructor


object BeethovenConstraintHint extends Enumeration {
  val DistributeCoresAcrossSLRs, MemoryConstrained = Value
  type BeethovenConstraintHint = Value
}

case object ConstraintHintsKey extends Field[List[BeethovenConstraintHint.type]]

class WithBeethoven(platform: Platform,
                    quiet: Boolean = false,
                    useConfigAsOutputName: Boolean = false) extends Config((site, _, _) => {
  case BQuiet => quiet
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
  case ConstraintHintsKey => List(BeethovenConstraintHint.DistributeCoresAcrossSLRs, BeethovenConstraintHint.MemoryConstrained)

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

object BeethovenParams {
  val SystemIDLengthKey = 4
  val CoreIDLengthKey = 10
}
