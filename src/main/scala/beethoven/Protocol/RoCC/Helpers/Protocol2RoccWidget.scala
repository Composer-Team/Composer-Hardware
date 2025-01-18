package beethoven.Protocol.RoCC.Helpers

import chisel3._
import chisel3.util._
import beethoven.{CmdRespBusWidthBytes, _}
import freechips.rocketchip.amba.axi4._
import chipsalliance.rocketchip.config._
import beethoven.Platforms._
import beethoven.common.{Misc, splitIntoChunks}
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

class MCRFileMap {
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

class Protocol2RoccWidget(numRegs: Int)(implicit p: Parameters) extends LazyModule with MCRFile {
  require((platform.frontBusBaseAddress & 0x3FFL) == 0)
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


class MCRFileModuleAXI(outer: Protocol2RoccWidget, numRegs: Int)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val dataWidth = platform.frontBusBeatBytes * 8
  require(dataWidth >= 32 && isPow2(dataWidth),
    """RoCC commands were originally broken down into 5 by 32b payloads. The first payload is 32b for the RISC-V instr.
      |and the 128b remaining were data payloads. 32b is a pretty common lower bound on the supported size for AXI
      |controllers (lower would be unusual because you need so many wires for the address and other metadata that
      |anything less is paying a high cost for control with very little data transfer. Anyway, we support powers of
      |two higher than 32. Lower than 32 is possible but requies modifying the state machines a bit.
      |""".stripMargin)
  val io = IO(new Bundle {
    val mcr = new MCRIO(numRegs)
  })

  val logNumRegs = log2Up(numRegs)
  val (in, edge) = outer.node.in(0)

  val address = Reg(UInt(logNumRegs.W))
  val writeData = Reg(UInt(dataWidth.W))
  val opLen = Reg(UInt(8.W))

  in.ar.ready := false.B
  in.aw.ready := false.B
  in.r.valid := false.B
  in.r.bits.data := DontCare
  in.r.bits.id := 0.U
  in.r.bits.last := false.B
  in.w.ready := false.B
  in.b.valid := false.B
  in.b.bits := DontCare

  // initialize read/write value wires
  io.mcr.read.foreach { rChannel =>
    rChannel.ready := false.B
  }
  /**
   * Implementation details:
   * We are assuming single beats on the bus for a few reasons
   * 1. AXI-Lite implementations don't support multi-beat (from my memory)
   * 2. Platforms that support the full AXI-protocol on their master bus to the accelerator are unlikely to
   * emit multi-beat transactions for Beethoven's current implementation of the software control.
   * If I remember right, this implementation is something like:
   *
   * volatile uint32_t *ptr;
   * *ptr = payload;
   *
   * The `volatile` keyword prevents a whole bunch of optimization that may result in "more efficient"
   * instrumentation. Worry about this problem when it arises I say.
   * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   * The machines RELY on Beethoven-specific instrumentation.
   * Specifically, that bits of a single RoCC command are all contiguous on the bus - no interleaving rocc commands.
   * Let's say you have a 32b bus and we're piecing together the pieces R1-R5 into a command
   * Then you get
   * R1... R2... R3... so on
   *
   * If you have a 64b bus, then you actually get two of these at a time
   * R1+2... R3+4..... R5+R6....
   * but R6 doesn't exist!
   * This means we either have to keep track of which part of the command we're moving at each transaction
   * so we can throw away R6, or even if we have a wide bus, we only use the bottom 32b. Doing the former
   * requires rewriting a lot of the software integration to be more flexible to the bus size. Someone else
   * can do this, so we're going with the latter solution.
   */

  // WRITE
  val write_machine = {
    val s_idle :: s_write :: s_drain :: s_response :: Nil = Enum(4)
    val state = RegInit(s_idle)
    val addr = Reg(UInt(logNumRegs.W))
    val len = Reg(in.ar.bits.len.cloneType)
    val id = Reg(in.ar.bits.id.cloneType)
    when(in.w.fire) {
      len := len - 1.U
    }
    when(state === s_idle) {
      in.aw.ready := true.B
      addr := in.aw.bits.addr(logNumRegs + 2 - 1, 2)
      len := in.aw.bits.len
      when(in.aw.fire) {
        state := s_write
      }
    }.elsewhen(state === s_write) {
      in.w.ready := true.B
      io.mcr.write(addr).valid := in.w.valid
      io.mcr.write(addr).bits := in.w.bits.data(31, 0)
      when(in.w.fire) {
        when(len === 0.U) {
          state := s_response
        }.otherwise {
          state := s_drain
        }
      }
    }.elsewhen(state === s_drain) {
      in.w.ready := true.B
      when(in.w.fire && len === 0.U) {
        state := s_response
      }
    }.otherwise {
      in.b.valid := true.B
      in.b.bits.resp := 0.U
      in.b.bits.id := id
      when(in.b.fire) {
        state := s_idle
      }
    }
  }

  val read_machine = {
    val s_idle :: s_read :: s_drain :: Nil = Enum(3)
    val state = RegInit(s_idle)

    val addr = Reg(UInt(logNumRegs.W))
    val len = Reg(in.ar.bits.len.cloneType)
    val id = Reg(in.ar.bits.id.cloneType)
    in.r.bits.id := id
    in.r.bits.last := len === 0.U
    in.r.bits.resp := 0.U
    // pad out the higher-order bits with 0 if necessary
    in.r.bits.data := (if (dataWidth == 32) io.mcr.read(addr).bits else {
      Cat(0.U((dataWidth-32).W), io.mcr.read(addr).bits)
    })

    when(in.r.fire) {
      len := len - 1.U
    }
    when(state === s_idle) {
      in.ar.ready := true.B
      addr := in.ar.bits.addr(logNumRegs + 2 - 1, 2)
      len := in.ar.bits.len
      id := in.ar.bits.id
      when(in.ar.fire) {
        state := s_read
      }
    }.elsewhen(state === s_read) {
      in.r.valid := true.B
      io.mcr.read(addr).ready := in.r.ready
      when(in.r.fire) {
        when(len === 0.U) {
          state := s_idle
        }.otherwise {
          state := s_drain
        }
      }
    }.otherwise { // state === s_drain
      in.r.valid := true.B
      in.r.bits.data := BigInt(f"ABCD" * (dataWidth/16), radix=16).U
      when(len === 0.U && in.r.fire) {
        state := s_idle
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

  val s_idle :: s_read :: s_read_send :: s_write :: s_write_response :: Nil = Enum(5)
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
      when(in.a.fire) {
        id := in.a.bits.source
        param := in.a.bits.size
        val start = log2Up(platform.frontBusBeatBytes)
        val end = start + log2Up(numRegs) - 1
        address := in.a.bits.address(end, start)
        when(in.a.bits.opcode === TLMessages.PutFullData || in.a.bits.opcode === TLMessages.PutPartialData) {
          state := s_write
          writeData := in.a.bits.data(31, 0)
        }
        when(in.a.bits.opcode === TLMessages.Get) {
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
      when(in.d.fire) {
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
      when(in.d.fire) {
        state := s_idle
      }
    }
  }
}

