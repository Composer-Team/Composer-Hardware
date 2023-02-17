package composer.RoccHelpers

import chisel3._
import chisel3.util._
import composer.{AXILSlaveAddressMask, MMIOBaseAddress}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
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
      val addr = i << 2
      require(i < 1024)
      outStream match {
        case a: Some[FileWriter] => a.get.write(s"#define ${entry.name.toUpperCase()} ($addr)\n")
        case None => println(s"Name: ${entry.name}, ID: $i, Addr: $addr")
      }
    }
  }
}

class MCRIO(numCRs: Int)(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val read = Vec(numCRs, Flipped(Decoupled(UInt(32.W))))
  val write = Vec(numCRs, Decoupled(UInt(32.W)))
  val wstrb = Output(UInt(4.W))

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
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      // 40b address
      address = List(AddressSet(0, p(AXILSlaveAddressMask))),
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

  val logNumRegs = log2Up(numRegs)
  val lnr_mo = logNumRegs - 1

  val (in, _) = outer.node.in(0)

  val sWriteIdle :: sGetWrite :: sDoWrite :: sWriteResp :: sWriteErrorWait :: sWriteErrorWb :: Nil = Enum(6)
  val sReadIdle :: sGetReadData :: sPublishRead :: sReadError :: Nil = Enum(4)

  val writeState = RegInit(sWriteIdle)
  val writeAddr = Reg(UInt(12.W))
  val writeLen = Reg(UInt(in.aw.bits.len.getWidth.W))
  val writeId = Reg(UInt(in.aw.bits.id.getWidth.W))
  val wStrb = Reg(UInt(io.mcr.wstrb.getWidth.W))
  val wData = Reg(UInt(in.w.bits.data.getWidth.W))

  val readState = RegInit(sReadIdle)
  val readAddr = Reg(UInt(12.W))
  val readLen = Reg(UInt(in.aw.bits.len.getWidth.W))
  val readID = Reg(UInt(in.ar.bits.id.getWidth.W))
  val readData = Reg(UInt(in.r.bits.data.getWidth.W))

  in.ar.ready := readState === sReadIdle
  in.aw.ready := writeState === sWriteIdle
  in.w.ready := writeState === sGetWrite || writeState === sWriteErrorWait

  // initialize read/write value wires
  io.mcr.read.foreach { rChannel =>
    rChannel.ready := false.B
  }
  io.mcr.write.foreach { wChannel =>
    wChannel.bits := DontCare
    wChannel.valid := false.B
  }

  io.mcr.write(writeAddr).bits := wData
  io.mcr.wstrb := wStrb
  io.mcr.write(writeAddr).valid := writeState === sDoWrite

  in.r.valid := readState === sPublishRead
  in.r.bits.data := io.mcr.read(readAddr).bits
  in.r.bits.resp := 0.U
  in.r.bits.last := readLen === 0.U
  in.r.bits.id := readID

  in.b.valid := writeState === sWriteResp
  in.b.bits.resp := 0.U

  switch(writeState) {
    is(sWriteIdle) {
      when(in.aw.valid) {
        writeAddr := (in.aw.bits.addr >> 2)(lnr_mo, 0)
        writeState := sGetWrite
        writeLen := in.aw.bits.len
        // this only actually asserts during simulation. During a real execution, we return errors for each beat instead
        assert(in.aw.bits.len === 0.U, "Currently only support single word writes")
        when(in.aw.bits.len =/= 0.U) {
          writeState := sWriteErrorWait
        }
        writeId := in.aw.bits.id
      }
    }
    is(sGetWrite) {
      when(in.w.fire) {
        wStrb := in.w.bits.strb
        wData := in.w.bits.data
        writeState := sDoWrite
      }
    }
    is(sDoWrite) {
      io.mcr.write(writeAddr).valid := true.B
      when(io.mcr.write(writeAddr).fire) {
        when(writeLen === 0.U) {
          writeState := sWriteResp
        } .otherwise {
          writeLen := writeLen - 1.U
          // don't increment the address because we're writing to the same register
        }
      }
    }
    is(sWriteResp) {
      in.b.bits.resp := 0.U // OKAY signal
      in.b.bits.id := writeId
      when(in.b.fire) {
        writeState := sWriteIdle
      }
    }
    is(sWriteErrorWait) {
      when (in.w.fire) {
        when (writeLen === 0.U) {
          writeState := sWriteErrorWb
        }.otherwise {
          writeLen := writeLen - 1.U
        }
      }
    }
    is(sWriteErrorWb) {
      in.b.bits.resp := 2.U // slave error
      in.b.valid := true.B
      when (in.b.fire) {
        writeState := sWriteIdle
      }
    }
  }

  switch(readState) {
    is(sReadIdle) {
      when(in.ar.valid) {
        readAddr := (in.ar.bits.addr >> 2)(lnr_mo, 0)
        readState := sGetReadData
        readLen := in.ar.bits.len
        readID := in.ar.bits.id
        assert(in.ar.bits.len === 0.U, "Currently only support single word reads")
        when (in.ar.bits.len =/= 0.U) {
          readState := sReadError
        }
      }
    }
    is (sGetReadData){
      io.mcr.read(readAddr).ready := true.B
      when(io.mcr.read(readAddr).fire) {
        readData := io.mcr.read(readAddr).bits
        readState := sPublishRead
      }
    }
    is(sPublishRead) {
      in.r.valid := true.B
      when(in.r.fire) {
        when (readLen === 0.U) {
          readState := sReadIdle
        }.otherwise {
          readLen := readLen - 1.U
        }
      }
    }
    is (sReadError) {
      in.r.bits.data := DontCare
      in.r.bits.resp := 2.U // slave error
      in.r.valid := true.B
      when(in.r.fire) {
        when (readLen === 0.U) {
          readState := sReadIdle
        }.otherwise {
          readLen := readLen - 1.U
        }
      }
    }
  }
}
