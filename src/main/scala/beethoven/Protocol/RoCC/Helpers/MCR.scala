package beethoven.Protocol.RoCC.Helpers

import beethoven.Parameters.CmdRespBusWidthBytes
import chisel3._
import chisel3.util._
import beethoven._
import freechips.rocketchip.amba.axi4._
import chipsalliance.rocketchip.config._
import beethoven.Platforms._
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

  def getCRdef(implicit p: Parameters): Seq[(String, String)] = {
    (regList.zipWithIndex map { case (entry, i) =>
      val addr = i << log2Up(p(PlatformKey).frontBusBeatBytes)
      require(i < 1024)
      (entry.name.toUpperCase, f"0x${addr.toHexString}")
    }).toSeq
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

trait MCRFile {
  def getMCRIO: MCRIO
}

class MCRFileTL(numRegs: Int)(implicit p: Parameters) extends LazyModule with MCRFile {
  require((p(PlatformKey).frontBusBaseAddress & 0x3FFL) == 0)
//  println(p(FrontBusAddressMask))
  val node = TLManagerNode(portParams = Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(platform.frontBusBaseAddress, platform.frontBusAddressMask)),
      supportsPutFull = TransferSizes(1, platform.frontBusBeatBytes),
      supportsPutPartial = TransferSizes(1, platform.frontBusBeatBytes),
      supportsGet = TransferSizes(1, platform.frontBusBeatBytes)
    )),
    beatBytes = platform.frontBusBeatBytes)
  ))

  lazy val module = new MCRFileModuleTL(this, numRegs)

  override def getMCRIO: MCRIO = module.io.mcr
}

class MCRFileAXI(numRegs: Int)(implicit p: Parameters) extends LazyModule with MCRFile {
  require((platform.frontBusBaseAddress& 0x3FFL) == 0)
    val node = AXI4SlaveNode(portParams = Seq(AXI4SlavePortParameters(slaves = Seq(
      AXI4SlaveParameters(
        address = Seq(AddressSet(platform.frontBusBaseAddress, platform.frontBusAddressMask)),
        supportsRead = TransferSizes(platform.frontBusBeatBytes),
        supportsWrite = TransferSizes(platform.frontBusBeatBytes)
      )),
    beatBytes = platform.frontBusBeatBytes)))
  lazy val module = new MCRFileModuleAXI(this, numRegs)

  override def getMCRIO: MCRIO = module.io.mcr
}


class MCRFileModuleAXI(outer: MCRFileAXI, numRegs: Int)(implicit p: Parameters) extends LazyModuleImp(outer) {

  val io = IO(new Bundle {
    val mcr = new MCRIO(numRegs)
  })

  val logNumRegs = log2Up(numRegs)
  val (in, edge) = outer.node.in(0)

  val s_idle :: s_read :: s_read_send :: s_write :: s_write_data :: s_write_response :: Nil = Enum(6)
  val state = RegInit(s_idle)

  val address = Reg(UInt(logNumRegs.W))
  val writeData = Reg(UInt(32.W))
  val readData = Reg(UInt(32.W))
  val opLen = Reg(UInt(8.W))
  val opvalid = Reg(Bool())

  in.ar.ready := false.B
  in.aw.ready := false.B
  in.r.valid := false.B
  in.r.bits.data := readData
  in.r.bits.id := 0.U
  in.r.bits.last := false.B
  in.w.ready := false.B
  in.b.valid := false.B
  in.b.bits := DontCare

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
      in.aw.ready := true.B
      in.ar.ready := !in.aw.valid

      when(in.aw.fire) {
        address := in.aw.bits.addr >> log2Up(platform.frontBusBeatBytes)
        state := s_write_data
        opLen := in.aw.bits.len
        opvalid := true.B
        assert(in.aw.bits.len === 0.U)
      }
      when(in.ar.fire) {
        address := in.ar.bits.addr >> log2Up(platform.frontBusBeatBytes)
        state := s_read
        opLen := in.ar.bits.len
        opvalid := true.B
        assert(in.ar.bits.len === 0.U)
      }
    }
    is(s_write_data) {
      in.w.ready := true.B
      when(in.w.fire) {
        writeData := in.w.bits.data
        state := s_write
      }
    }
    is(s_write) {
      io.mcr.write(address).bits := writeData
      val go = io.mcr.write(address).ready || !opvalid
      io.mcr.write(address).valid := opvalid
      when(go) {
        opLen := opLen - 1.U
        when(opLen === 0.U) {
          state := s_write_response
        }.otherwise {
          state := s_write_data
        }
      }
    }
    is(s_write_response) {
      in.b.valid := true.B
      in.b.bits.resp := 0.U
      when(in.b.fire) {
        state := s_idle
      }
    }
    is(s_read) {
      io.mcr.read(address).ready := opvalid
      readData := io.mcr.read(address).bits
      when(io.mcr.read(address).fire || !opvalid) {
        opvalid := false.B
        state := s_read_send
      }
    }
    is(s_read_send) {
      in.r.valid := true.B
      in.r.bits.resp := 0.U
      in.r.bits.data := readData
      in.r.bits.last := opLen === 0.U
      when(in.r.fire) {
        when(opLen === 0.U) {
          state := s_idle
        }.otherwise {
          state := s_read
        }
        opLen := opLen - 1.U
      }
    }
  }
}

class MCRFileModuleTL(outer: MCRFileTL, numRegs: Int)(implicit p: Parameters) extends LazyModuleImp(outer) {

  val io = IO(new Bundle {
    val mcr = new MCRIO(numRegs)
  })

  val logNumRegs = log2Up(numRegs)
  val (in, edge) = outer.node.in(0)

  val s_idle :: s_read :: s_read_send :: s_write  :: s_write_response :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val address = Reg(UInt(logNumRegs.W))
  val writeData = Reg(UInt(32.W))
  val readData = Reg(UInt(32.W))
  val id = Reg(UInt(in.params.sourceBits.W))
  val opLen = Reg(UInt(8.W))
  val param = Reg(UInt(in.params.sizeBits.W))


  // initialize read/write value wires
  io.mcr.read.foreach { rChannel =>
    rChannel.ready := false.B
  }
  io.mcr.write.foreach { wChannel =>
    wChannel.bits := DontCare
    wChannel.valid := false.B
  }

  in.a.ready := false.B
  in.d.valid := false.B
  in.d.bits := DontCare

  // For bus widths > 64, we expect multiple transactions to

  switch(state) {
    is(s_idle) {
      in.a.ready := true.B
      when (in.a.fire) {
        id := in.a.bits.source
        param := in.a.bits.size
        val start = log2Up(platform.frontBusBeatBytes)
        val end = start + log2Up(numRegs) - 1
        address := in.a.bits.address(end, start)
        when (in.a.bits.opcode === TLMessages.PutFullData || in.a.bits.opcode === TLMessages.PutPartialData) {
          state := s_write
          writeData := in.a.bits.data(31, 0)
        }
        when (in.a.bits.opcode === TLMessages.Get) {
          state := s_read
        }
      }
    }
    is(s_write) {
      io.mcr.write(address).bits := writeData
      io.mcr.write(address).valid := true.B
      state := s_write_response
    }
    is(s_write_response) {
      in.d.valid := true.B
      in.d.bits := edge.AccessAck(id, param)
      when (in.d.fire) {
        state := s_idle
      }
    }
    is(s_read) {
      io.mcr.read(address).ready := true.B
      readData := io.mcr.read(address).bits
      when(io.mcr.read(address).fire) {
        state := s_read_send
      }
    }
    is(s_read_send) {
      in.d.valid := true.B
      in.d.bits := edge.AccessAck(id, param, readData)
      when (in.d.fire) {
        state := s_idle
      }
    }
  }
}

