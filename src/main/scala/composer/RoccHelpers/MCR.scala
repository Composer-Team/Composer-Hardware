package composer.RoccHelpers

import chisel3._
import chisel3.util._
import composer._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import java.io.FileWriter
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
  // DO NOT put the MMIOs in the first page. For unified memory systems this will result in null pointer dereferences
  // not segfaulting
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

  def printCRs(outStream: Option[FileWriter] = None)(implicit p: Parameters): Unit = {
    regList.zipWithIndex foreach { case (entry, i) =>
      val addr = i << log2Up(p(AXILSlaveBeatBytes))
      require(i < 1024)
      outStream match {
        case a: Some[FileWriter] => a.get.write(s"#define ${entry.name.toUpperCase()} ($addr)\n")
        case None => println(s"Name: ${entry.name}, ID: $i, Addr: $addr")
      }
    }
  }
}

class MCRIO(numCRs: Int)(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val read = Vec(numCRs, Flipped(Decoupled(UInt((p(CmdRespBusWidthBytes) * 8).W))))
  val write = Vec(numCRs, Decoupled(UInt((p(CmdRespBusWidthBytes) * 8).W)))
  val wstrb = Output(UInt(p(CmdRespBusWidthBytes).W))

  def bindReg(reg: RegisterEntry, addr: Int): Unit = {
    if (reg.permissions.writeable) {
      when(write(addr).valid) {
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
  require((p(MMIOBaseAddress) & 0x3FFL) == 0)
  val node = TLManagerNode(portParams = Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = List(AddressSet(p(MMIOBaseAddress), p(AXILSlaveAddressMask))),
      supportsGet = TransferSizes(1, p(AXILSlaveBeatBytes)),
      supportsPutFull = TransferSizes(1, p(AXILSlaveBeatBytes)),
      supportsPutPartial = TransferSizes(1, p(AXILSlaveBeatBytes))
    )),
    beatBytes = p(CmdRespBusWidthBytes)
  )))
  lazy val module = new MCRFileModule(this, numRegs)
}

class MCRFileModule(outer: MCRFile, numRegs: Int)(implicit p: Parameters) extends LazyModuleImp(outer) {

  val io = IO(new Bundle {
    val mcr = new MCRIO(numRegs)
  })

  val logNumRegs = log2Up(numRegs)
  val (in, _) = outer.node.in(0)

  val s_idle :: s_read :: s_read_response :: s_write :: s_write_response :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val address = Reg(UInt(logNumRegs.W))
  val writeData = Reg(UInt(in.params.dataBits.W))
  val readData = Reg(UInt(in.params.dataBits.W))
  val source = Reg(UInt(in.params.sourceBits.W))
  val strobe = Reg(UInt((in.params.dataBits/8).W))

  in.a.ready := state === s_idle
  in.d.bits := DontCare
  in.d.bits.data := readData
  in.d.bits.source := source
  in.d.valid := false.B

  // initialize read/write value wires
  io.mcr.read.foreach { rChannel =>
    rChannel.ready := false.B
  }
  io.mcr.write.foreach { wChannel =>
    wChannel.bits := DontCare
    wChannel.valid := false.B
  }

  // For bus widths > 64, we expect multiple transactions to
  switch(state) {
    is(s_idle) {
      writeData := in.a.bits.data
      val asbb = log2Up(p(AXILSlaveBeatBytes))
      address := in.a.bits.address(asbb + log2Up(numRegs) - 1, asbb)
      state := Mux(in.a.bits.opcode === TLMessages.PutFullData, s_write, s_read)
      source := in.a.bits.source
      strobe := in.a.bits.mask
    }
    is(s_read) {
      io.mcr.read(address).ready := true.B
      readData := io.mcr.read(address).bits
      when(io.mcr.read(address).fire) {
        state := s_read_response
      }
    }
    is(s_read_response) {
      in.d.valid := true.B
      in.d.bits.opcode := TLMessages.AccessAckData
      // other fields already set above
      when(in.d.fire) {
        state := s_idle
      }
    }
    is(s_write) {
      val move_on = io.mcr.write(address).ready || (strobe === 0.U)
      io.mcr.write(address).valid := strobe =/= 0.U
      io.mcr.write(address).bits := writeData
      when(move_on) {
        state := s_write_response
      }
    }
    is(s_write_response) {
      in.d.valid := true.B
      in.d.bits.opcode := TLMessages.AccessAck
      when(in.d.fire) {
        state := s_idle
      }
    }
  }
}
