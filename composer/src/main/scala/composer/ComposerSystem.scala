package composer

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3.util._
import chisel3._
import composer.MemoryStreams.{ChannelTransactionBundle, SequentialWriter}
import composer.RoccConstants.{FUNC_ADDR, FUNC_START}
import composer.common.Util.BoolSeqHelper
import composer.common._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
case object ComposerSystemWrapperKey extends Field[ComposerCoreWrapper]

class ComposerSystem(val systemParams: ComposerSystemParams, val system_id: Int)(implicit p: Parameters) extends LazyModule {
  val nCores = systemParams.nCores
  val queueDepth = systemParams.channelQueueDepth
  val coreParams = systemParams.coreParams

  val readLoc = coreParams.readChannelParams.map(_.loc)
  val writeLoc = coreParams.writeChannelParams.map(_.loc)

  val cores = Seq.tabulate(systemParams.nCores) { idx: Int =>
    LazyModule(new ComposerCoreWrapper(systemParams, idx, system_id))
  }

//  val all_mems = readLoc ++ writeLoc
  val reader_nodes = cores.flatMap(_.unCachedReaders) ++ cores.flatMap(_.CacheNodes.map(_._1.mem_out))
  val writer_nodes = cores.flatMap(_.writers)

//  val hasMem = (all_mems foldLeft[Boolean] false) ({ case (b: Boolean, s: String) => b || (s == "Mem") })
  val hasMem = reader_nodes.nonEmpty || writer_nodes.nonEmpty
  val mem = if (hasMem) {
    Seq.fill(nCores) {
      TLIdentityNode()
    }
  } else {
    Seq()
  }


  if (reader_nodes.nonEmpty || writer_nodes.nonEmpty) {
    mem zip cores foreach {
      case (m_out, core) =>
        val mem_xbar = TLXbar()
        core.unCachedReaders ++ core.writers ++ core.CacheNodes.map (_._1.mem_out) foreach (mem_xbar := _)
        m_out := mem_xbar
    }
  }


  val blockBytes = p(CacheBlockBytes)

  val maxBufSize = 1024 * 4 * 4
  // TODO UG: use parameters for this
  val maxCores = 32
  val maxWriteCount = 4
  val maxReadCount = 5
  val maxMemAddr = p(ExtMem).get.master.size
  val bufStride = 1 // used for sharing a buffer amongst several cores
  val bufSize = /*systemParams.bufSize  * nBytes*/ maxBufSize * bufStride
//  val producerBuffers = coreParams.writeChannelParams.indices.map { i =>
//    val wloc = writeLoc(i)
//    if (wloc contains "Local") {
//      None
//      val bufSize = /*systemParams.bufSize  * nBytes*/ maxBufSize * bufStride
//      Some((0 until nCores by bufStride).map { x =>
//        //noinspection DuplicatedCode
//        val bufAddr = (system_id * maxWriteCount * maxCores * maxBufSize) + (i * maxCores * maxBufSize)+ (x * maxBufSize) + maxMemAddr
//        implicit val prop = new RAMProperties(AddressSet(bufAddr, bufSize - 1))
//        if (systemParams.bufMasked) {
//          LazyModule(new MaskTLRAM()).node
//        } else {
//          LazyModule(new SimpleTLRAM()).node
//        }
//      })
//    } else {
//      None
//    }
//  }

//  val consumerBuffersModules = coreParams.readChannelParams.indices.map { i =>
//    val rloc = readLoc(i)
//    if (rloc contains "Local") {
//      // This address thing sucks, can collide addresses between the two
//      Some((0 until nCores by bufStride).map { x =>
//        //noinspection DuplicatedCode
//        val bufAddr = (system_id * maxReadCount * maxCores * maxBufSize) + (i * maxCores * maxBufSize) + (x * maxBufSize) + maxMemAddr
//        implicit val prop = new RAMProperties(AddressSet(bufAddr, bufSize - 1))
//        if (systemParams.bufMasked) {
//          LazyModule(new MaskTLRAM())
//          //LazyModule(new TLRAM(AddressSet(bufAddr, bufSize - 1), beatBytes = 32)).node
//        } else if (systemParams.doubleBuf) {
//          LazyModule(new DoubleTLRAM())
//        } else {
//          LazyModule(new SimpleTLRAM())
//        }
//      })
//      None
//    } else {
//      None
//    }
//  }
//  val consumerBuffers = consumerBuffersModules.map { bufs =>
//    if (bufs.isDefined) {
//      if (systemParams.bufMasked) {
//        Some(bufs.get.map(_.asInstanceOf[MaskTLRAM].node))
//      } else if (systemParams.doubleBuf) {
//        Some(bufs.get.map(_.asInstanceOf[DoubleTLRAM].node))
//      } else {
//        Some(bufs.get.map(_.asInstanceOf[SimpleTLRAM].node))
//      }
//      None
//    } else {
//      None
//    }
//  }

  // potentially split writer node
  // (mem, localBuf, remoteBuf)
//  val writeSplit = writeLoc.zipWithIndex.map { case (wloc, i) =>
//    writers.map { wlist =>
//      // wlist(i) is one writer
//      if ((wloc contains "Local") && (wloc contains "Mem")) {
//        val split = LazyModule(new TLXbar)
//        split.node := wlist(i)
//        (Some(split.node), Some(split.node), None)
//      } else if ((wloc contains "Remote") && (wloc contains "Mem")) {
//        val split = LazyModule(new TLXbar)
//        split.node := wlist(i)
//        (Some(split.node), None, Some(split.node))
//      } else if (wloc contains "Mem") {
//        (Some(wlist(i)), None, None)
//      } else if (wloc contains "Local") {
//        (None, Some(wlist(i)), None)
//      } else if (wloc contains "Remote") {
//        (None, None, Some(wlist(i)))
//      } else {
//        (None, None, None)
//      }
//    }
//  }

  // potentially split reader node
  // (mem, localBuf, remoteBuf)
//  val readSplit = readLoc.zipWithIndex.map { case (rloc, i) =>
//    readers.map { rlist =>
//      // rlist(i) is one reader
//      if ((rloc contains "Remote") && (rloc contains "Mem")) {
//        val split = LazyModule(new TLXbar)
//        split.node := rlist(i)
//        (Some(split.node), None, Some(split.node))
//      } else if ((rloc contains "Local") && (rloc contains "Mem")) {
//        val split = LazyModule(new TLXbar)
//        split.node := rlist(i)
//        (Some(split.node), Some(split.node), None)
//      } else if (rloc contains "Mem") {
//        (Some(rlist(i)), None, None)
//      } else if (rloc contains "Remote") {
//        (None, None, Some(rlist(i)))
//      } else if (rloc contains "Local") {
//        (None, Some(rlist(i)), None)
//      } else {
//        (None, None, None)
//      }
//    }
//  }

  // connect Mem channels up to mem
//  mem.zipWithIndex.foreach { case (m, coreid) =>
//    val arb = LazyModule(new TLXbar)
//    Seq((readLoc, readSplit), (writeLoc, writeSplit)) foreach { case (locs, splits) =>
//      locs zip splits foreach { case (loc, split) =>
//        if (loc contains "Mem") { arb.node := split(coreid)._1.get }
//      }
//    }
//    m := TLBuffer() := arb.node
//  }

  // connect producers+consumers to local buffer
  // localMem is a Seq of identity nodes correspnoding to a single write channel
  // this attaches to an xbar which splits into 8 per-core buf per channel
  // then a second xbar connects a single producer and single consumer

  // first when the buffer is at the producer

//  val localWrite = writeLoc.zipWithIndex.map { case (wloc, i) =>
//    if (wloc contains "Local") {
//      val corenode = TLIdentityNode()
//      val corearb = LazyModule(new TLXbar)
//      (0 until nCores by bufStride).map { coreid =>
//        val loc = TLIdentityNode()
//        val arb = LazyModule(new TLXbar)
//        (0 until bufStride).foreach { offset =>
//          arb.node := TLBuffer() := writeSplit(i)(coreid + offset)._2.get
//        }
//        arb.node := TLBuffer() := loc
//        producerBuffers(i).get(coreid / bufStride) := TLFragmenter(32, 128) := TLBuffer() := arb.node
//        loc
//      }.foreach { loc_node =>
//        loc_node := corearb.node
//      }
//      corearb.node := corenode
//      Some(corenode)
//    } else {
//      None
//    }
//  }

  // then when the buffer is at the consumer
//  val localRead = readLoc.zipWithIndex.map { case (rloc, i) =>
//    if (rloc contains "Local") {
//      val corenode = TLIdentityNode()
//      val corearb = LazyModule(new TLXbar)
//      (0 until nCores by bufStride).map { coreid =>
//        val loc = TLIdentityNode()
//        val arb = LazyModule(new TLXbar)
//        (0 until bufStride).foreach { offset =>
//          arb.node := TLBuffer() := readSplit(i)(coreid + offset)._2.get
//        }
//        arb.node := TLBuffer() := loc
//        consumerBuffers(i).get(coreid / bufStride) := TLFragmenter(32, 128) := TLBuffer() := arb.node
//        loc
//      }.foreach { loc_node =>
//        loc_node := corearb.node
//      }
//      corearb.node := corenode
//      Some(corenode)
//    } else {
//      None
//    }
//  }

  // create external ports for reading from remote types
  // will be connected to the localWrite of another syste
  // in Accelerator.scala
//  val remoteRead = readLoc.zipWithIndex.map { case (rloc, i) =>
//    if (rloc contains "Remote") {
//      val corenode = TLIdentityNode()
//      val corearb = LazyModule(new TLXbar)
//      readSplit(i).foreach { rnode =>
//        corearb.node := rnode._3.get
//      }
//      corenode := corearb.node
//      Some(corenode)
//    } else {
//      None
//    }
//  }

//  val remoteWrite = writeLoc.zipWithIndex.map { case (wloc, i) =>
//    if (wloc contains "Remote") {
//      val corenode = TLIdentityNode()
//      val corearb = LazyModule(new TLXbar)
//      writers.indices.foreach { coreid =>
//        corearb.node := writeSplit(i)(coreid)._3.get
//      }
//      corenode := corearb.node
//      Some(corenode)
//    } else {
//      None
//    }
//  }

  lazy val module = new ComposerSystemImp(this)
}

class ComposerSystemImp(val outer: ComposerSystem) extends LazyModuleImp(outer) {
  val io = IO(new ComposerSystemIO())
  val arbiter = Module(new RRArbiter(new ComposerRoccResponse(), outer.systemParams.nCores))
  val cores = outer.cores.map(_.module)

  arbiter.io.in <> cores.map(_.io.resp)
  resp.valid := arbiter.io.out.valid
  resp.bits.rd := arbiter.io.out.bits.rd
  resp.bits.core_id := arbiter.io.out.bits.core_id
  resp.bits.data := arbiter.io.out.bits.data
  resp.bits.system_id := outer.system_id.U
  arbiter.io.out.ready := resp.ready

  lazy val cmd = Queue(io.cmd)

  lazy val funct = cmd.bits.inst.funct

  lazy val coreSelect = cmd.bits.core_id

  val addressBits = outer.mem map { m =>
    val edge = m.edges.in(0)
    log2Up(edge.manager.maxAddress)
  } match {
    case Seq() => 0
    case l: Seq[Int] => l.max
    case _ => 0
  }
  //    ++ outer.localWrite.filter(_.isDefined).map(_.get)
  //    ++ outer.remoteWrite.filter(_.isDefined).map(_.get)
  //    ++ outer.localRead.filter(_.isDefined).map(_.get)
  //    ++ outer.remoteRead.filter(_.isDefined).map(_.get)

  lazy val numReadChannels = outer.readLoc.length
  lazy val numWriteChannels = outer.writeLoc.length

  lazy val resp = Wire(Decoupled(new ComposerRoccResponse())) //Queue(io.resp)

  // can't this be done much easier with a mux?
  lazy val coreReady = cores.zipWithIndex.map { case (core, i) =>
    core.io.req.ready || coreSelect =/= i.U
  }.reduce(_ && _)

  // connect cores to mem
  io.resp <> Queue(resp)

//  if (outer.systemParams.doubleBuf) {
//    outer.consumerBuffersModules.foreach { bufs =>
//      if (bufs.isDefined) {
//        bufs.get.zipWithIndex.foreach { case (buf, i) =>
//          buf.asInstanceOf[DoubleTLRAM].module.io.swap.valid := false.B
//          when(cores(i).io.req.fire) {
//            buf.asInstanceOf[DoubleTLRAM].module.io.swap.valid := true.B
//          }
//        }
//      }
//    }
//  }

  cmd.ready := funct =/= FUNC_START.U || coreReady

  io.busy := cores.map(_.io.busy).any

  val nChannelBits = p(ChannelSelectionBitsKey)
  val lenBits = log2Up(p(MaxChannelTransactionLenKey))

  val channelSelect = cmd.bits.rs1(nChannelBits-1, 1)
  val channelRead = cmd.bits.rs1(0)

  cores.zipWithIndex.foreach { case (core, i) =>
    // TO-DO: delay until all readers/writers are ready? use DecoupledHelper
    val coreStart = cmd.fire && funct === FUNC_START.U && coreSelect === i.U //&& coreRWReady(i)
    core.io.req.valid := coreStart
    core.io.req.bits := cmd.bits
    //rest are up to individual system
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
  val txLenFromCmd = cmd.bits.rs1(nChannelBits + lenBits - 1, nChannelBits)
  cores.zipWithIndex.foreach { case (core: ComposerCore, i) =>
    def pairWithAddrStore(id: Int, interface: DecoupledIO[ChannelTransactionBundle], condition: Bool): Unit = {
      val tx_len = Reg(UInt(log2Up(p(MaxChannelTransactionLenKey)).W))
      val tx_addr_start = Reg(UInt(addressBits.W))
      when(addr_func_live && coreSelect === i.U && channelSelect === id.U && condition) {
        tx_len := txLenFromCmd
        tx_addr_start := cmd.bits.rs2(addressBits - 1, 0)
      }
      interface.valid := cmdFireLatch && functLatch === FUNC_START.U && coreSelectLatch === i.U
      interface.bits.addr := tx_addr_start
      interface.bits.len := tx_len
    }
    core.read_ios.foreach(a => pairWithAddrStore(a._1, a._2, channelRead))
    core.write_ios.foreach(a => pairWithAddrStore(a._1, a._2, !channelRead))
  }
}

