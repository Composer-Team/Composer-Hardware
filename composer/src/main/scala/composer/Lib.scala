package composer

import chisel3._
import chisel3.util._
import composer.CppGenerationUtils._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Permissions(readable: Boolean, writeable: Boolean)
object ReadOnly extends Permissions(true, false)
object WriteOnly extends Permissions(false, true)
object ReadWrite extends Permissions(true, true)

abstract class MCRMapEntry {
  def name: String
  def permissions: Permissions
}

case class DecoupledSinkEntry(node: DecoupledIO[UInt], name: String) extends MCRMapEntry {
  val permissions = WriteOnly
}
case class DecoupledSourceEntry(node: DecoupledIO[UInt], name: String) extends MCRMapEntry {
  val permissions = ReadOnly
}
case class RegisterEntry(node: Data, name: String, permissions: Permissions) extends MCRMapEntry


class MCRFileMap() {
  private val name2addr = mutable.LinkedHashMap[String, Int]()
  private val regList = ArrayBuffer[MCRMapEntry]()

  def allocate(entry: MCRMapEntry): Int = {
    Predef.assert(!name2addr.contains(entry.name), "name already allocated")
    val address = name2addr.size
    name2addr += (entry.name -> address)
    regList.append(entry)
    address
  }

  def lookupAddress(name: String): Option[Int] = name2addr.get(name)

  def numRegs: Int = regList.size

  def bindRegs(mcrIO: MCRIO): Unit = regList.zipWithIndex foreach {
    case (e: DecoupledSinkEntry, addr) => mcrIO.bindDecoupledSink(e, addr)
    case (e: DecoupledSourceEntry, addr) => mcrIO.bindDecoupledSource(e, addr)
    case (e: RegisterEntry, addr) => mcrIO.bindReg(e, addr)
  }

  def genHeader(prefix: String, base: BigInt, sb: StringBuilder): Unit = {
    name2addr.toList foreach { case (regName, idx) =>
      val fullName = s"${prefix}_$regName"
      val address = base + idx
      sb append s"#define $fullName $address\n"
    }
  }
  // A variation of above which dumps the register map as a series of arrays
  def genArrayHeader(prefix: String, base: BigInt, sb: StringBuilder): Unit = {
    def emitArrays(regs: Seq[(MCRMapEntry, BigInt)], prefix: String): Unit = {
      sb.append(genConstStatic(s"${prefix}_num_registers", UInt32(regs.size)))
      sb.append(genArray(s"${prefix}_names", regs.map{ x => CStrLit(x._1.name)}))
      sb.append(genArray(s"${prefix}_addrs", regs.map{ x => UInt32(x._2)}))
    }

    val regAddrs = regList map (reg => reg -> (base + lookupAddress(reg.name).get))
    val readRegs = regAddrs filter (_._1.permissions.readable)
    val writeRegs = regAddrs filter (_._1.permissions.writeable)
    emitArrays(readRegs, prefix + "_R")
    emitArrays(writeRegs, prefix + "_W")
  }

  // Returns a copy of the current register map
  def getRegMap = name2addr.toMap

  def printCRs(): Unit = {
    regList.zipWithIndex foreach { case (entry, i) => println(s"Name: ${entry.name}, Addr: $i") }
  }
}

class MCRIO(numCRs: Int)(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val read = Vec(numCRs, Flipped(Decoupled(UInt(32.W))))
  val write = Vec(numCRs, Decoupled(UInt(32.W)))
  val wstrb = Output(UInt(4.W))

  def bindReg(reg: RegisterEntry, addr: Int): Unit = {
    if (reg.permissions.writeable) {
      when(write(addr).valid){
        reg.node := write(addr).bits
      }
    } else {
      assert(write(addr).valid != true.B, s"Register ${reg.name} is read only")
    }

    if (reg.permissions.readable) {
      read(addr).bits := reg.node
    } else {
      assert(read(addr).ready === false.B, "Register ${reg.name} is write only")
    }

    read(addr).valid := true.B
    write(addr).ready := true.B
  }

  def bindDecoupledSink(channel: DecoupledSinkEntry, addr: Int): Unit = {
    channel.node <> write(addr)
    assert(read(addr).ready === false.B, "Can only write to this decoupled sink")
  }

  def bindDecoupledSource(channel: DecoupledSourceEntry, addr: Int): Unit = {
    read(addr) <> channel.node
    assert(write(addr).valid =/= true.B, "Can only read from this decoupled source")
  }

}

class MCRFile(numRegs: Int)(implicit p: Parameters) extends LazyModule {

  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address = List(AddressSet(0,1023)),
      regionType = RegionType.UNCACHED,
      supportsWrite = TransferSizes(1, 4),
      supportsRead = TransferSizes(1, 4)
    )),
    beatBytes = 4
  )))

  lazy val module = new MCRFileModule(this, numRegs)
}

class MCRFileModule(outer: MCRFile, numRegs: Int)(implicit p: Parameters) extends LazyModuleImp(outer) {

  val io = IO(new Bundle {
    val mcr = new MCRIO(numRegs)
  })

  val nastiWStrobeBits = 4

  val (in, _) = outer.node.in(0)

  //TODO: Just use a damn state machine.
  val rValid = RegInit(false.B)
  val arFired = RegInit(false.B)
  val awFired = RegInit(false.B)
  val wFired = RegInit(false.B)
  val wCommited = RegInit(false.B)
  val bId = Reg(UInt(1.W))
  val rId = Reg(UInt(1.W))
  val rData = Reg(UInt(32.W))
  val wData = Reg(UInt(32.W))
  val wAddr = Reg(UInt(log2Up(numRegs).W))
  val rAddr = Reg(UInt(log2Up(numRegs).W))
  val wStrb = Reg(UInt(4.W))

  when(in.aw.fire){
    awFired := true.B
    wAddr := in.aw.bits.addr >> log2Up(nastiWStrobeBits)
    bId := in.aw.bits.id
    assert(in.aw.bits.len === 0.U)
  }

  when(in.w.fire){
    wFired := true.B
    wData := in.w.bits.data
    wStrb := in.w.bits.strb
  }

  when(in.ar.fire) {
    arFired := true.B
    rAddr := (in.ar.bits.addr >> log2Up(nastiWStrobeBits))(log2Up(numRegs)-1,0)
    rId := in.ar.bits.id
    assert(in.ar.bits.len === 0.U, "MCRFile only support single beat reads")
  }

  when(in.r.fire) {
    arFired := false.B
  }

  when(in.b.fire) {
    awFired := false.B
    wFired := false.B
    wCommited := false.B
  }

  when(io.mcr.write(wAddr).fire){
    wCommited := true.B
  }

  io.mcr.write foreach { w => w.valid := false.B; w.bits := wData }
  io.mcr.read foreach { _.ready := false.B }
  io.mcr.write(wAddr).valid := awFired && wFired && (~wCommited).asBool
  io.mcr.read(rAddr).ready := arFired && in.r.ready

  in.r.bits.id := rId
  in.r.bits.data := io.mcr.read(rAddr).bits
  in.r.bits.last := true.B
  in.r.bits.resp := 0.U
  //in.r.bits.user := 0.U
  in.r.valid := arFired && io.mcr.read(rAddr).valid

  in.b.bits.id := bId
  in.b.bits.resp := 0.U
  //in.b.bits.user := 0.U
  in.b.valid := awFired && wFired && wCommited

  in.ar.ready := ~arFired
  in.aw.ready := ~awFired
  in.w.ready := ~wFired
}
