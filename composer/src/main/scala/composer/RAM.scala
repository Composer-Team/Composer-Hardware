package composer
//
//import chisel3._
//import chisel3.util._
//import composer.RAMUtil.{makeSinglyPortedMem, mask}
//import freechips.rocketchip.diplomacy._
//import freechips.rocketchip.tilelink._
//import freechips.rocketchip.util._
//import freechips.rocketchip.config.Parameters
//
//import scala.annotation.tailrec
//
//class RAMProperties(val address: AddressSet,
//                    val cacheable: Boolean = true,
//                    val exeutable: Boolean = true,
//                    val beatBytes: Int = 32,
//                    val devName: Option[String] = None)
//
//abstract class RAM(implicit p: Parameters, prop: RAMProperties) extends LazyModule {
//  val device = prop.devName map (new SimpleDevice(_, Seq("sifive,sram0"))) getOrElse new MemoryDevice
//  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(
//    managers = Seq(TLSlaveParameters.v1(
//      address = List(prop.address),
//      resources = device.reg("mem"),
//      regionType = if (prop.cacheable) RegionType.UNCACHED else RegionType.IDEMPOTENT,
//      executable = prop.exeutable,
//      supportsGet = TransferSizes(1, prop.beatBytes),
//      supportsPutPartial = TransferSizes(1, prop.beatBytes),
//      supportsPutFull = TransferSizes(1, prop.beatBytes),
//      fifoId = Some(0)
//    )),
//    beatBytes = prop.beatBytes,
//    minLatency = 1
//  )))
//
//  val resources = device.reg("mem")
//}
//
//object RAMUtil {
//  @tailrec
//  def bigBits(x: BigInt, tail: List[Boolean] = Nil): List[Boolean] =
//    if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)
//
//  def mask(implicit prop: RAMProperties): List[Boolean] = bigBits(prop.address.mask >> log2Ceil(prop.beatBytes))
//
//  // Use single-ported memory with byte-write enable
//  def makeSinglyPortedMem(size: Int)(implicit prop: RAMProperties): SyncReadMem[Vec[UInt]] = {
//    // We require the address range to include an entire beat (for the write mask)
//    require((prop.address.mask & (prop.beatBytes - 1)) == prop.beatBytes - 1)
//    //val mem = SeqMem(size, Vec(beatBytes, Bits(width = 8)))
//    val mem = SyncReadMem(size, Vec(prop.beatBytes / 4, UInt(32.W)))
//    prop.devName.foreach(n => mem.suggestName(n.split("-").last))
//    mem
//  }
//}
//
//
////noinspection DuplicatedCode
//class MaskTLRAM(implicit prop: RAMProperties, p: Parameters) extends RAM {
//
//  lazy val module = new LazyModuleImp(this) {
//    val (in, edge) = node.in(0)
//
//    val addrBits = (mask zip edge.addr_hi(in.a.bits).asBools).filter(_._1).map(_._2)
//    val a_legal = prop.address.contains(in.a.bits.address)
//    val memAddress = Cat(addrBits.reverse)
//    val mem = makeSinglyPortedMem(1 << addrBits.size)
//
//    val s_idle :: s_read :: s_write :: Nil = Enum(3)
//
//    //val d_full = RegInit(Bool(false))
//    val d_read = Reg(Bool())
//    val d_size = Reg(UInt())
//    val d_source = Reg(UInt())
//    val d_data = Wire(UInt())
//    val d_legal = Reg(Bool())
//
//    val state = RegInit(s_idle)
//
//    // Flow control
//    when(in.d.fire) {
//      state := s_idle
//    }
//    in.d.valid := state === s_read || state === s_write
//    in.a.ready := state === s_idle
//
//    in.d.bits := edge.AccessAck(d_source, d_size, !d_legal)
//    // avoid data-bus Mux
//    in.d.bits.data := d_data
//    in.d.bits.opcode := Mux(d_read, TLMessages.AccessAckData, TLMessages.AccessAck)
//
//    val read = in.a.bits.opcode === TLMessages.Get
//    val rdata = Wire(Vec(prop.beatBytes / 4, UInt(32.W)))
//    val wmask = RegNext(FillInterleaved(8, in.a.bits.mask))
//    val wdata = RegNext(in.a.bits.data)
//    d_data := rdata.asTypeOf(UInt((prop.beatBytes * 8).W))
//    when(in.a.fire) {
//      d_read := read
//      d_size := in.a.bits.size
//      d_source := in.a.bits.source
//      d_legal := a_legal
//    }
//
//    // exactly this pattern is required to get a RWM memory
//    when(in.a.fire && !read && a_legal) {
//      state := s_write
//      //mem.write(memAddress, wdata)
//    }
//
//    when(state === s_write && RegNext(in.a.fire)) {
//
//      val wmaskdata = wmask & wdata
//      val olddata = rdata.asTypeOf(UInt((prop.beatBytes * 8).W)) & (~wmask).asUInt
//      mem.write(RegNext(memAddress), (wmaskdata | olddata).asTypeOf(Vec(prop.beatBytes / 4, UInt(32.W))))
//      //state := s_idle
//    }
//
//    when(in.a.fire && read) {
//      state := s_read
//    }
//
//    val ren = in.a.fire // && read
//    rdata := mem.readAndHold(memAddress, ren)
//
//    // Tie off unused channels
//    in.b.valid := false.B
//    in.c.ready := true.B
//    in.e.ready := true.B
//  }
//}
//
////noinspection DuplicatedCode
//class SimpleTLRAM(implicit prop: RAMProperties, p: Parameters) extends RAM {
//  lazy val module = new LazyModuleImp(this) {
//    val (in, edge) = node.in(0)
//
//    val addrBits = (mask zip edge.addr_hi(in.a.bits).asBools).filter(_._1).map(_._2)
//    val a_legal = prop.address.contains(in.a.bits.address)
//    val memAddress = Cat(addrBits.reverse)
//    val mem = makeSinglyPortedMem(1 << addrBits.size)
//    val d_full = RegInit(false.B)
//    val d_read = Reg(Bool())
//    val d_size = Reg(UInt())
//    val d_source = Reg(UInt())
//    val d_data = Wire(UInt())
//    val d_legal = Reg(Bool())
//
//    // Flow control
//    when(in.d.fire) {
//      d_full := false.B
//    }
//    when(in.a.fire) {
//      d_full := true.B
//    }
//    in.d.valid := d_full
//    in.a.ready := in.d.ready || !d_full
//
//    in.d.bits := edge.AccessAck(d_source, d_size, !d_legal)
//    // avoid data-bus Mux
//    in.d.bits.data := d_data
//    in.d.bits.opcode := Mux(d_read, TLMessages.AccessAckData, TLMessages.AccessAck)
//
//    val read = in.a.bits.opcode === TLMessages.Get
//    val rdata = Wire(Vec(prop.beatBytes / 4, UInt(32.W)))
//    val wdata = in.a.bits.data
//    d_data := rdata.asTypeOf(UInt((prop.beatBytes * 8).W))
//    when(in.a.fire) {
//      d_read := read
//      d_size := in.a.bits.size
//      d_source := in.a.bits.source
//      d_legal := a_legal
//    }
//
//    // exactly this pattern is required to get a RWM memory
//    when(in.a.fire && !read && a_legal) {
//      mem.write(memAddress, wdata.asTypeOf(Vec(prop.beatBytes / 4, UInt(32.W))))
//    }
//    val ren = in.a.fire && read
//    rdata := mem.readAndHold(memAddress, ren)
//
//    // Tie off unused channels
//    in.b.valid := false.B
//    in.c.ready := true.B
//    in.e.ready := true.B
//  }
//}
//
////noinspection DuplicatedCode
//// SimpleTLRAM that automatically changes addresses
//// between the first and second half of the
//// address space to simulate a double buffer
//class DoubleTLRAM(implicit prop: RAMProperties, p: Parameters) extends RAM {
//  lazy val module = new DoubleTLRAMModule(this)
//}
//
//class DoubleTLRAMModule(outer: DoubleTLRAM) (implicit prop: RAMProperties, p: Parameters) extends LazyModuleImp(outer) {
//  val io = IO(new Bundle {
//    val swap = Flipped(Decoupled(Bool()))
//  })
//  val (in, edge) = outer.node.in(0)
//
//  // double buffering logic
//  io.swap.ready := true.B
//  val swap = RegInit(false.B)
//  when(io.swap.fire) {
//    swap := ~swap
//  }
//
//  val addrBits = (mask zip edge.addr_hi(in.a.bits).asBools).filter(_._1).map(_._2)
//  val a_legal = prop.address.contains(in.a.bits.address)
//  val origAddress = Cat(addrBits.reverse)
//
//  val readAddress = Mux(swap, origAddress, origAddress + (1 << (addrBits.size - 1)).U)
//  val writeAddress = Mux(swap, origAddress + (1 << (addrBits.size - 1)).U, origAddress)
//  val mem = makeSinglyPortedMem(1 << addrBits.size)
//
//  val d_full = RegInit(false.B)
//  val d_read = Reg(Bool())
//  val d_size = Reg(UInt())
//  val d_source = Reg(UInt())
//  val d_data = Wire(UInt())
//  val d_legal = Reg(Bool())
//
//  // Flow control
//  when(in.d.fire) {
//    d_full := false.B
//  }
//  when(in.a.fire) {
//    d_full := false.B
//  }
//  in.d.valid := d_full
//  in.a.ready := in.d.ready || !d_full
//
//  in.d.bits := edge.AccessAck(d_source, d_size, !d_legal)
//  // avoid data-bus Mux
//  in.d.bits.data := d_data
//  in.d.bits.opcode := Mux(d_read, TLMessages.AccessAckData, TLMessages.AccessAck)
//
//  val read = in.a.bits.opcode === TLMessages.Get
//  val rdata = Wire(Vec(prop.beatBytes / 4, UInt(32.W)))
//  val wdata = in.a.bits.data
//  d_data := rdata.asTypeOf(UInt((prop.beatBytes * 8).W))
//  when(in.a.fire) {
//    d_read := read
//    d_size := in.a.bits.size
//    d_source := in.a.bits.source
//    d_legal := a_legal
//  }
//
//  // exactly this pattern is required to get a RWM memory
//  when(in.a.fire && !read && a_legal) {
//    mem.write(writeAddress, wdata.asTypeOf(Vec(prop.beatBytes / 4, UInt(32.W))))
//  }
//  val ren = in.a.fire && read
//  rdata := mem.readAndHold(readAddress, ren)
//
//  // Tie off unused channels
//  in.b.valid := false.B
//  in.c.ready := true.B
//  in.e.ready := true.B
//}
