package composer

import composer.MemoryStreams._
import freechips.rocketchip.config._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.PgLevels
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._

case object MMIOBaseAddress extends Field[Long]
case object HasDiscreteMemory extends Field[Boolean]
case object ComposerSystemsKey extends Field[List[ComposerSystemParams]]
case object SystemIDLengthKey extends Field[Int]
case object CoreIDLengthKey extends Field[Int]
//case object MaxChannelTransactionLenKey extends Field[Int]
case object TLInterconnectWidthBytes extends Field[Int]
// if we support a dedicated DMA port, provide the number of ID bits
case object HasDMA extends Field[Option[Int]]
case object HasAXILExternalMMIO extends Field[Boolean]
case object CXbarMaxDegree extends Field[Int]
case object MaximumTransactionLength extends Field[Int]
case object SystemName2IdMapKey extends Field[Map[String, Int]]
case object RequireInternalCommandRouting extends Field[Boolean]

case class ComposerCoreParams(memoryChannelParams: List[CChannelParams] = List(),
                              core_id: Int = 0, // for internal use
                              system_id: Int = 0, // for internal use
                              nMemXacts: Int = 1 // not for production release
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
  case HasDMA => Some(6)
  case HasAXILExternalMMIO => true
  case MMIOBaseAddress => 0x0L // MMIO is not real, it's just a PCIE bus transaction that pretends to be MMIO
  // TODO this can be tuned
  case CXbarMaxDegree => 16
  case HasDiscreteMemory => true
})

class WithKriaMem extends Config((_, _, _) => {
  case ExtMem => Some(MemoryPortParams(MasterPortParams(
    base = 0,
    size = 1L << 49,
    beatBytes = 4,
    idBits = 6
  ), 1))
  // MMIO is real - part of the address space is reserved for MMIO communications
  case MMIOBaseAddress => (1 << 12).toLong // put on the 2nd page. Skip first 4KB page to preserve behavior of *(nullptr)
  case HasDMA => None
  // TODO this can be tuned
  case CXbarMaxDegree => 8
  case HasAXILExternalMMIO => false // use full AXI4
  case HasDiscreteMemory => false
})

class WithNoMem extends WithAWSMem(1)


// TODO work DMA into Trait
// TODO work Kria Memory (4GB) into Trait

class WithComposer(maximumTxLengthBytes: Int = 1 << 10, systemIDbits: Int = 4, coreIdBits: Int = 8) extends Config((site, _, _) => {
  case ComposerSystemsKey => Seq()
  case SystemIDLengthKey => systemIDbits
  case CoreIDLengthKey => coreIdBits
  case TLInterconnectWidthBytes => 16
//  case MaxChannelTransactionLenKey => 1 << 30
  // Tile parameters
  // Page table levels. We set it higher than rocket chip default because we need to support large virtual addresses
  // spaces for some targets (e.g. Kria). Increasing the # of page table levels supports this, but makes no difference
  // in elaboration
  case PgLevels => 5
  case XLen => 64 // Applies to all cores
  case MaximumTransactionLength =>
    require(maximumTxLengthBytes <= (1 << 14), "Maximum transaction length supported by AXI is 2^14 B. ")
    maximumTxLengthBytes
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