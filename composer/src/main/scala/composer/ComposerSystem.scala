package composer

import chipsalliance.rocketchip.config._
import chisel3.util._
import chisel3._
import composer.MemoryStreams._
import composer.RoccConstants.{FUNC_ADDR, FUNC_START}
import composer.common._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._

import scala.annotation.tailrec

class ComposerSystem(val systemParams: ComposerSystemParams, val system_id: Int)(implicit p: Parameters) extends LazyModule {
  val nCores = systemParams.nCores
  val coreParams = systemParams.coreParams

  val cores = List.tabulate(nCores) { idx: Int =>
    LazyModule(new ComposerCoreWrapper(systemParams, idx, system_id))
  }

  // we want to avoid high-degree xbars. Recursively make multi-stage xbar network
  val memory_nodes = {
    val core_mems = cores.flatMap(_.mem_nodes).map { mn =>
      val tli = TLIdentityNode()
      tli := mn
      tli
    }

    def recursivelyReduceXBar(grp: Seq[TLIdentityNode]): Seq[TLIdentityNode] = {
      if (grp.length <= p(CChannelXBarWidth)) grp
      else (grp.grouped(p(CChannelXBarWidth)) map { subgroup =>
        val sub_xbar = TLXbar()
        subgroup foreach (sub_xbar := _)
        val out_node = TLIdentityNode()
        out_node := TLBuffer() := sub_xbar
        out_node
      }).toSeq
    }

    recursivelyReduceXBar(core_mems)
  }

val blockBytes = p(CacheBlockBytes)

lazy val module = new ComposerSystemImp(this)
}

case class CChannelIdentifier(system_name: String, core_idx: Int, channel_name: String, channel_subidx: Int, io_idx: Int)

class ComposerSystemImp(val outer: ComposerSystem) extends LazyModuleImp(outer) {
  val io = IO(new ComposerSystemIO())
  val arbiter = Module(new RRArbiter(new ComposerRoccResponse(), outer.systemParams.nCores))
  val cores = outer.cores.map(_.module)

  arbiter.io.in <> cores.map(_.io.resp)

  resp.valid := arbiter.io.out.valid
  resp.bits.rd := arbiter.io.out.bits.rd
  resp.bits.core_id := arbiter.io.chosen // .io.out.bits.core_id
  resp.bits.data := arbiter.io.out.bits.data
  resp.bits.system_id := outer.system_id.U
  arbiter.io.out.ready := resp.ready

  lazy val cmd = Queue(io.cmd)

  lazy val funct = cmd.bits.inst.funct

  lazy val coreSelect = cmd.bits.core_id

  val addressBits = outer.memory_nodes map { m =>
    m.out(0)._1.params.addressBits
  } match {
    case Seq() => 0
    case l: Seq[Int] => l.max
    case _ => 0
  }

  lazy val resp = Wire(Decoupled(new ComposerRoccResponse())) //Queue(io.resp)

  // can't this be done much easier with a mux?
  lazy val coreReady = cores.zipWithIndex.map { case (core, i) =>
    core.io.req.ready || coreSelect =/= i.U
  }.reduce(_ && _)

  // connect cores to resp channel
  io.resp <> Queue(resp)

  cmd.ready := funct =/= FUNC_START.U || coreReady

  io.busy := cores.map(_.io.busy).reduce(_ || _)

  val lenBits = log2Up(p(MaxChannelTransactionLenKey))

  val channelSelect = Cat(cmd.bits.inst.rs2(2, 0), cmd.bits.inst.rs1)

  cores.zipWithIndex.foreach { case (core, i) =>
    val coreStart = cmd.fire && funct === FUNC_START.U && coreSelect === i.U
    core.io.req.valid := coreStart
    core.io.req.bits := cmd.bits
  }

  // scope to separate out read channel stuff
  val cmdFireLatch = RegNext(cmd.fire)
  val cmdBitsLatch = RegNext(cmd.bits)
  val functLatch = cmdBitsLatch.inst.funct
  val coreSelectLatch = cmdBitsLatch.core_id

  val addr_func_live = cmd.bits.inst.funct === FUNC_ADDR.U && cmd.fire

  if (cores(0).read_ios.nonEmpty) {
    cores(0).read_ios(0)._2.valid := false.B
    cores(0).write_ios(0)._2.valid := true.B
  }
  val txLenFromCmd = cmd.bits.payload1(lenBits-1, 0)

  @tailrec
  private def assign_channel_addresses(coreId: Int, channelList: List[((String, Int), DecoupledIO[ChannelTransactionBundle])],
                                       assignment: Int = 0,
                                      // core name, core_id, channel name, channel idx, io_idx
                                       io_map: List[CChannelIdentifier] = List()): List[CChannelIdentifier] = {
    channelList match {
      case (channel_identifier, txio) :: rst =>
        val tx_len = Reg(UInt(log2Up(p(MaxChannelTransactionLenKey)).W))
        val tx_addr_start = Reg(UInt(addressBits.W))
        when(addr_func_live && coreSelect === coreId.U && channelSelect === assignment.U) {
          tx_len := txLenFromCmd
          tx_addr_start := cmd.bits.payload2(addressBits - 1, 0)
        }
        txio.valid := cmdFireLatch && functLatch === FUNC_START.U && coreSelectLatch === coreId.U
        txio.bits.addr := tx_addr_start
        txio.bits.len := tx_len
        assign_channel_addresses(coreId, rst, assignment + 1,
          CChannelIdentifier(
            system_name = outer.systemParams.name,
            core_idx = coreId,
            channel_name = channel_identifier._1,
            channel_subidx = channel_identifier._2,
            io_idx = assignment) :: io_map)
      case _ => io_map
    }
  }

  val core_io_mappings = cores.zipWithIndex.map { case (co, id) =>
    assign_channel_addresses(id, co.read_ios ++ co.write_ios)
  }
}

