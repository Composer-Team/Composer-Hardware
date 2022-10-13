package composer

import freechips.rocketchip.config._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.PgLevels
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._

// can configure this to add whatever features of a channel we want. Location should probably be an enum...
// TODO UG: Channels should be named. The user shouldn't have to know the order of the channels and remember in both
//          the software and the hardware! There should be a similar "Key" system that gets put into the composer.yaml
//          and manifests in software that way or per haps we just generate the C++ enum and code stubs here...
//          This is a more design-focused task and I may end up doing it myself but you are more than welcome to propose
//          a solution - but it has to be good because this is an interface that is end-user-facing.

// TODO UG & Chris: Is the current address scheme for read/write channels good? May consider re-doing the whole thing
//                  to a more intuitive interface.
case class ComposerChannelParams(widthBytes: Int = 8,
                                 location: String = "Mem")

case class ComposerCoreParams(readChannelParams: Seq[ComposerChannelParams],
                              writeChannelParams: Seq[ComposerChannelParams],
                              customRead: Boolean = false,
                              core_id: Int = 0, // for internal use
                              system_id: Int = 0, // for internal use
                              nMemXacts: Int = 1 // not for production release
                             )

case class ComposerSystemParams(coreParams: ComposerCoreParams,
                                nCores: Int,
                                system_id: Int,
                                buildCore: (ComposerCoreParams, Parameters) => ComposerCore,

                                /**
                                  * In elements, per write channel, scaled by the number of bytes
                                  */
                                bufSize: Int = 1024,
                                bufMasked: Boolean = false,
                                doubleBuf: Boolean = false,
                                channelQueueDepth: Int = 32,
                               )

class WithAWSMem extends Config((site, here, up) => {
  case CacheBlockBytes => 128
  // TODO this is where we would change # of AWS Mem Channels!
  case ExtMem => Some(MemoryPortParams(MasterPortParams(
    base = 0,
    size = 0x400000000L,
    beatBytes = 64,
    idBits = 12
  ), 1))

  //------------------------------------------------------
  // need dummy parameters to trick rocketchip.
  // none of these are needed but they are
  // uninitialized by default and will cause
  // a compile error. ignore these
  case MonitorsEnabled => false
  case TileKey => RocketTileParams()
  case RocketCrossingKey => List(RocketCrossingParams(
    crossingType = SynchronousCrossing(),
    master = TileMasterPortParams(cork = Some(true))
  ))
  //  case ForceFanoutKey => ForceFanoutParams(false, false, false, false, false)
  //------------------------------------------------------

  // rocc data length
  case XLen => 64
  case NMemChan => 1
})

/*TODO: copy over template and change names to match the name of the scala file
 * Make sure CoreParams matches what is in the scala file
 * Seq in readElementsSize and readLocations should have the same number of entry
 * They correspond to the number of inputs
 * similar concept applies to write
 * opcode correspond to the base address number for sending the command
 * (BASE addr in $CL_DIR/software/runtime/include/rocc.h for this should be opcode <<< 3)
 */


class WithComposer extends Config((site, here, up) => {
  case ProducerBuffers => Map()
  case ConsumerBuffers => Map()
  case ComposerSystemsKey => Seq()
  case SystemIDLengthKey => 4
  case CoreIDLengthKey => 8
  // Tile parameters
  case PgLevels => if (site(XLen) == 64) 3 /* Sv39 */
  else 2 /* Sv32 */
  case XLen => 64 // Applies to all cores
  case MaxHartIdBits => 1 // log2Up(site(TilesLocated(InSubsystem)).map(_.tileParams.hartId).max+1)
  // Interconnect parameters
  case SystemBusKey => SystemBusParams(
    beatBytes = site(XLen) / 8,
    blockBytes = site(CacheBlockBytes))
  case ControlBusKey => PeripheryBusParams(
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
  // Additional device Parameters
  case BootROMLocated(InSubsystem) => Some(BootROMParams(contentFileName = "./bootrom/bootrom.img"))
  case SubsystemExternalResetVectorKey => false
  case DebugModuleKey => Some(DefaultDebugModuleParams(site(XLen)))
  case CLINTKey => Some(CLINTParams())
  case PLICKey => Some(PLICParams())
  // Copying WithJustOneBus - TODO consider if this is the best?
  case TLNetworkTopologyLocated(InSubsystem) => List(
    JustOneBusTopologyParams(sbus = site(SystemBusKey))
  )

})