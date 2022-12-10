package composer

import freechips.rocketchip.config._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.PgLevels
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._

case object MMIOBaseAddress extends Field[Option[Long]]

case object ProducerBuffers extends Field[Map[Seq[(Int, Int)], Seq[(Int, Int)]]]

case object ConsumerBuffers extends Field[Map[Seq[(Int, Int)], Seq[(Int, Int)]]]

case object ComposerSystemsKey extends Field[Seq[ComposerSystemParams]]

/* TODO UG: How many bits we are using to identify a system. Ensure that no system has the same ID and the largest ID
 *           can be represented with this many bits
 */
case object SystemIDLengthKey extends Field[Int]

/* TODO UG: How many bits are we using to identify a core. Ensure that no system has more cores than can be uniquely
 *           identified by this many bits.
 */
case object CoreIDLengthKey extends Field[Int]

case object ChannelSelectionBitsKey extends Field[Int]

case object MaxChannelTransactionLenKey extends Field[Int]

case object TLInterconnectWidthBytes extends Field[Int]

// if we support a dedicated DMA port, provide the number of ID bits
case object HasDMA extends Field[Option[Int]]

case object HasAXILExternalMMIO extends Field[Boolean]

abstract class ComposerChannelParams(val loc: String)

case class ComposerCachedReadChannelParams(nChannels: Int, sizeBytes: Int, id: Int,
                                           idxMask: Option[Long] = None,
                                           location: String = "Mem") extends ComposerChannelParams(location)
case class ComposerUncachedChannelParams(location: String = "Mem") extends ComposerChannelParams(location)

case class ComposerCoreParams(readChannelParams: Seq[ComposerChannelParams],
                              writeChannelParams: Seq[ComposerChannelParams],
                              customRead: Boolean = false,
                              core_id: Int = 0, // for internal use
                              system_id: Int = 0, // for internal use
                              nMemXacts: Int = 1 // not for production release
                             )

case class ComposerConstructor(composerCoreParams: ComposerCoreParams, composerCoreWrapper: ComposerCoreWrapper)

case class ComposerSystemParams(coreParams: ComposerCoreParams,
                                nCores: Int,
                                name: String,
                                buildCore: (ComposerConstructor, Parameters) => ComposerCore,

                                /**
                                  * In elements, per write channel, scaled by the number of bytes
                                  */
                                bufSize: Int = 1024,
                                bufMasked: Boolean = false,
                                doubleBuf: Boolean = false,
                                channelQueueDepth: Int = 32,
                               )


class WithAWSMem(nMemoryChannels: Int) extends Config((_, _, _) => {
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
  case HasDMA => None // Some(6)
  case HasAXILExternalMMIO => true
  case MMIOBaseAddress => None // MMIO is not real, it's just a PCIE bus transaction that pretends to be MMIO
})

class WithKriaMem extends Config((_, _, _) => {
  case ExtMem => Some(MemoryPortParams(MasterPortParams(
    base = 0,
    size = 1L << 49,
    beatBytes = 4,
    idBits = 6
  ), 1))
  // MMIO is real - part of the address space is reserved for MMIO communications
  case MMIOBaseAddress => Some(0xB0000000L)
  case HasDMA => None
  case HasAXILExternalMMIO => false // use full AXI4
})


// TODO work DMA into Trait
// TODO work Kria Memory (4GB) into Trait
// TODO MMIO between CPU & Composer very doable

/*TODO: copy over template and change names to match the name of the scala file
 * Make sure CoreParams matches what is in the scala file
 * Seq in readElementsSize and readLocations should have the same number of entry
 * They correspond to the number of inputs
 * similar concept applies to write
 * opcode correspond to the base address number for sending the command
 * (BASE addr in $CL_DIR/software/runtime/include/rocc.h for this should be opcode <<< 3)
 */


class WithComposer extends Config((site, _, _) => {
  case ProducerBuffers => Map()
  case ConsumerBuffers => Map()
  case ComposerSystemsKey => Seq()
  case SystemIDLengthKey => 4
  case CoreIDLengthKey => 8
  case TLInterconnectWidthBytes => 16
  case ChannelSelectionBitsKey => 3
  case MaxChannelTransactionLenKey => 1 << 30
  // Tile parameters
  // Page table levels. We set it higher than rocket chip default because we need to support large virtual addresses
  // spaces for some targets (e.g. Kria). Increasing the # of page table levels supports this, but makes no difference
  // in elaboration
  case PgLevels => 5
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
  // Copying WithJustOneBus
  case TLNetworkTopologyLocated(InSubsystem) => List(
    JustOneBusTopologyParams(sbus = site(SystemBusKey))
  )
  // NOT NECESSARY FOR COMPILER BUT IS NECESSARY FOR VERILATOR
  case MonitorsEnabled => false
  // Necessary for compile
  case TileKey => RocketTileParams()
  // unneeded in newer versions of RocketChip
  //  case RocketCrossingKey => List(RocketCrossingParams(
  //    crossingType = SynchronousCrossing(),
  //    master = TileMasterPortParams(cork = Some(true))
  //  ))
  //  case ForceFanoutKey => ForceFanoutParams(false, false, false, false, false)
  //------------------------------------------------------
})