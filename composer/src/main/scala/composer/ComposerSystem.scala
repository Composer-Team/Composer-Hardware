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

  val memory_nodes = {
    val core_mems = cores.flatMap(_.mem_nodes)
    if (core_mems.length > p(CChannelXBarWidth)) {
      core_mems.grouped(p(CChannelXBarWidth)) map { node_set =>
        val memory_subset_xbar = LazyModule(new TLXbar())
        val xbar_node = memory_subset_xbar.node
        node_set foreach (xbar_node := _)
        xbar_node
      }
    } else core_mems
  }

  val blockBytes = p(CacheBlockBytes)

  lazy val module = new ComposerSystemImp(this)
}

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

  val nChannelBits = p(ChannelSelectionBitsKey)
  val lenBits = log2Up(p(MaxChannelTransactionLenKey))

  val channelSelect = cmd.bits.payload1(nChannelBits - 1, 1)
  val channelRead = cmd.bits.payload1(0)

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

  // connect readChannels and writeChannels appropriately
  // TODO don't do this for sparse readers/writers
  if (cores(0).read_ios.nonEmpty) {
    cores(0).read_ios(0)._2.valid := false.B
    cores(0).write_ios(0)._2.valid := true.B
  }
  val txLenFromCmd = cmd.bits.payload1(nChannelBits + lenBits - 1, nChannelBits)

  val read_ios = cores.zipWithIndex.flatMap(q => q._1.read_ios.map((q._2, _)))
  val write_ios = cores.zipWithIndex.flatMap(q => q._1.write_ios.map((q._2, _)))

  @tailrec
  private def assign_channel_addresses(coreId: Int, channelList: List[(String, DecoupledIO[ChannelTransactionBundle])], condition: Bool, assignment: Int = 0): Unit = {
    channelList match {
      case (_, txio) :: rst =>
        val tx_len = Reg(UInt(log2Up(p(MaxChannelTransactionLenKey)).W))
        val tx_addr_start = Reg(UInt(addressBits.W))
        when(addr_func_live && coreSelect === coreId.U && channelSelect === assignment.U && condition) {
          tx_len := txLenFromCmd
          tx_addr_start := cmd.bits.payload2(addressBits - 1, 0)
        }
        txio.valid := cmdFireLatch && functLatch === FUNC_START.U && coreSelectLatch === coreId.U
        txio.bits.addr := tx_addr_start
        txio.bits.len := tx_len
        assign_channel_addresses(coreId, rst, condition, assignment + 1)
      case _ => ;
    }
  }

  cores.zipWithIndex.foreach { case (co, id) =>
    assign_channel_addresses(id, co.read_ios, channelRead)
    assign_channel_addresses(id, co.write_ios, !channelRead)
  }

}

