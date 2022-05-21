package composer

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._
import composer.RoccConstants.{FUNC_ADDR, FUNC_START}
import composer.common.Util.BoolSeqHelper
import composer.common._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.{TLBuffer, TLFragmenter, TLIdentityNode, TLXbar}

class ComposerSystem(val systemParams: ComposerSystemParams)(implicit p: Parameters) extends LazyModule {
  val nCores = systemParams.nCores
  val queueDepth = systemParams.channelQueueDepth
  val modularInterface = systemParams.coreParams
  val system_id = systemParams.system_id

  val readLoc = modularInterface.readChannelParams.map(_.location)
  val writeLoc = modularInterface.writeChannelParams.map(_.location)

  // replace this code with "getorelse"

  val all_mems = readLoc ++ writeLoc
  val hasMem = (all_mems foldLeft[Boolean]false)({case (b: Boolean, s: String) => b || (s == "Mem")})
  val mem = if (hasMem) {
    Seq.fill(nCores) {
      TLIdentityNode()
    }
  } else {
    Seq()
  }

  // TODO Other reader types
  val readers = Seq.fill(nCores) {
    modularInterface.readChannelParams.map { rch =>
      LazyModule(new ColumnReadChannel(rch.width)(p))
    }
  }
  val writers = Seq.fill(nCores) {
    modularInterface.writeChannelParams.map { wch =>
      LazyModule(new FixedSequentialWriteChannel(wch.width, modularInterface.nMemXacts)(p))
    }
  }

  val maxBufSize = 1024 * 4 * 4
  val maxCores = 32
  val maxWriteCount = 4
  val maxReadCount = 5
  val maxMemAddr = 0x400000000L
  val bufStride = 1 // used for sharing a buffer amongst several cores
  val bufSize = /*systemParams.bufSize  * nBytes*/ maxBufSize * bufStride
  val producerBuffers = modularInterface.writeChannelParams.indices.map { i =>
    val wloc = writeLoc(i)
    if (wloc contains "Local") {
      val bufSize = /*systemParams.bufSize  * nBytes*/ maxBufSize * bufStride
      Some((0 until nCores by bufStride).map { x =>
        //noinspection DuplicatedCode
        val bufAddr = (system_id * maxWriteCount * maxCores * maxBufSize) + (i * maxCores * maxBufSize) + (x * maxBufSize) + maxMemAddr
        implicit val prop = new RAMProperties(AddressSet(bufAddr, bufSize - 1))
        if (systemParams.bufMasked) {
          LazyModule(new MaskTLRAM()).node
        } else {
          LazyModule(new SimpleTLRAM()).node
        }
      })
    } else {
      None
    }
  }

  val consumerBuffersModules = modularInterface.readChannelParams.indices.map { i =>
    val rloc = readLoc(i)
    if (rloc contains "Local") {
      //TODO: This address thing sucks, can collide addresses between the two
      Some((0 until nCores by bufStride).map { x =>
        //noinspection DuplicatedCode
        val bufAddr = (system_id * maxReadCount * maxCores * maxBufSize) + (i * maxCores * maxBufSize) + (x * maxBufSize) + maxMemAddr
        implicit val prop = new RAMProperties(AddressSet(bufAddr, bufSize - 1))
        if (systemParams.bufMasked) {
          LazyModule(new MaskTLRAM())
          //LazyModule(new TLRAM(AddressSet(bufAddr, bufSize - 1), beatBytes = 32)).node
        } else if (systemParams.doubleBuf) {
          LazyModule(new DoubleTLRAM())
        } else {
          LazyModule(new SimpleTLRAM())
        }
      })
    } else {
      None
    }
  }
  val consumerBuffers = consumerBuffersModules.map { bufs =>
    if (bufs.isDefined) {
      if (systemParams.bufMasked) {
        Some(bufs.get.map(_.asInstanceOf[MaskTLRAM].node))
      } else if (systemParams.doubleBuf) {
        Some(bufs.get.map(_.asInstanceOf[DoubleTLRAM].node))
      } else {
        Some(bufs.get.map(_.asInstanceOf[SimpleTLRAM].node))
      }
    } else {
      None
    }
  }

  // potentially split writer node
  // (mem, localBuf, remoteBuf)
  val writeSplit = writeLoc.zipWithIndex.map { case (wloc, i) =>
    writers.map { wlist =>
      // wlist(i) is one writer
      if ((wloc contains "Local") && (wloc contains "Mem")) {
        val split = LazyModule(new TLXbar)
        split.node := wlist(i).node
        (Some(split.node), Some(split.node), None)
      } else if ((wloc contains "Remote") && (wloc contains "Mem")) {
        val split = LazyModule(new TLXbar)
        split.node := wlist(i).node
        (Some(split.node), None, Some(split.node))
      } else if (wloc contains "Mem") {
        (Some(wlist(i).node), None, None)
      } else if (wloc contains "Local") {
        (None, Some(wlist(i).node), None)
      } else if (wloc contains "Remote") {
        (None, None, Some(wlist(i).node))
      } else {
        (None, None, None)
      }
    }
  }

  // potentially split reader node
  // (mem, localBuf, remoteBuf)
  val readSplit = readLoc.zipWithIndex.map { case (rloc, i) =>
    readers.map { rlist =>
      // rlist(i) is one reader
      if ((rloc contains "Remote") && (rloc contains "Mem")) {
        val split = LazyModule(new TLXbar)
        split.node := rlist(i).node
        (Some(split.node), None, Some(split.node))
      } else if ((rloc contains "Local") && (rloc contains "Mem")) {
        val split = LazyModule(new TLXbar)
        split.node := rlist(i).node
        (Some(split.node), Some(split.node), None)
      } else if (rloc contains "Mem") {
        (Some(rlist(i).node), None, None)
      } else if (rloc contains "Remote") {
        (None, None, Some(rlist(i).node))
      } else if (rloc contains "Local") {
        (None, Some(rlist(i).node), None)
      } else {
        (None, None, None)
      }
    }
  }

  // connect Mem channels up to mem
  mem.zipWithIndex.foreach { case (m, coreid) =>
    val arb = LazyModule(new TLXbar)
    Seq((readLoc, readSplit), (writeLoc, writeSplit)) foreach { case (locs, splits) =>
      locs zip splits foreach { case (loc, split) =>
        if (loc contains "Mem") arb.node := split(coreid)._1.get
      }
    }
    m := TLBuffer() := arb.node
  }

  // connect producers+consumers to local buffer
  // localMem is a Seq of identity nodes correspnoding to a single write channel
  // this attaches to an xbar which splits into 8 per-core buf per channel
  // then a second xbar connects a single producer and single consumer

  // first when the buffer is at the producer
  val localWrite = writeLoc.zipWithIndex.map { case (wloc, i) =>
    if (wloc contains "Local") {
      val corenode = TLIdentityNode()
      val corearb = LazyModule(new TLXbar)
      (0 until nCores by bufStride).map { coreid =>
        val loc = TLIdentityNode()
        val arb = LazyModule(new TLXbar)
        (0 until bufStride).foreach { offset =>
          arb.node := TLBuffer() := writeSplit(i)(coreid + offset)._2.get
        }
        arb.node := TLBuffer() := loc
        producerBuffers(i).get(coreid / bufStride) := TLFragmenter(32, 128) := TLBuffer() := arb.node
        loc
      }.foreach { loc_node =>
        loc_node := corearb.node
      }
      corearb.node := corenode
      Some(corenode)
    } else {
      None
    }
  }

  // then when the buffer is at the consumer
  val localRead = readLoc.zipWithIndex.map { case (rloc, i) =>
    if (rloc contains "Local") {
      val corenode = TLIdentityNode()
      val corearb = LazyModule(new TLXbar)
      (0 until nCores by bufStride).map { coreid =>
        val loc = TLIdentityNode()
        val arb = LazyModule(new TLXbar)
        (0 until bufStride).foreach { offset =>
          arb.node := TLBuffer() := readSplit(i)(coreid + offset)._2.get
        }
        arb.node := TLBuffer() := loc
        consumerBuffers(i).get(coreid / bufStride) := TLFragmenter(32, 128) := TLBuffer() := arb.node
        loc
      }.foreach { loc_node =>
        loc_node := corearb.node
      }
      corearb.node := corenode
      Some(corenode)
    } else {
      None
    }
  }

  // create external ports for reading from remote types
  // will be connected to the localWrite of another syste
  // in Accelerator.scala
  val remoteRead = readLoc.zipWithIndex.map { case (rloc, i) =>
    if (rloc contains "Remote") {
      val corenode = TLIdentityNode()
      val corearb = LazyModule(new TLXbar)
      readSplit(i).foreach { rnode =>
        corearb.node := rnode._3.get
      }
      corenode := corearb.node
      Some(corenode)
    } else {
      None
    }
  }
  val remoteWrite = writeLoc.zipWithIndex.map { case (wloc, i) =>
    if (wloc contains "Remote") {
      val corenode = TLIdentityNode()
      val corearb = LazyModule(new TLXbar)
      writers.indices.foreach { coreid =>
        corearb.node := writeSplit(i)(coreid)._3.get
      }
      corenode := corearb.node
      Some(corenode)
    } else {
      None
    }
  }

  lazy val module = new ComposerSystemImp(this)
}

class ComposerSystemImp(val outer: ComposerSystem) extends LazyModuleImp(outer) {
  val io = IO(new ComposerSystemIO())
  val cores = Seq.tabulate(outer.systemParams.nCores){idx: Int =>
    val cparam = outer.systemParams.coreParams.copy(core_id = idx, system_id = outer.systemParams.system_id)
    outer.systemParams.buildCore(cparam, p)
  }

  cores.zipWithIndex.foreach { case (_, i) =>
    readLengths(i).foreach(_ := RegNext(cmd.bits.rs2))
    writeLengths(i).foreach(_ := RegNext(cmd.bits.rs2))
  }

  val arbiter = Module(new RRArbiter(new ComposerRoccResponse(), outer.systemParams.nCores))
  arbiter.io.in <> cores.map(_.io.resp)
  resp.valid := arbiter.io.out.valid
  resp.bits.rd := arbiter.io.out.bits.rd
  resp.bits.core_id := arbiter.io.out.bits.core_id
  resp.bits.data := arbiter.io.out.bits.data
  resp.bits.system_id := outer.systemParams.system_id.U
  arbiter.io.out.ready := resp.ready

  lazy val cmd = Queue(io.cmd)

  lazy val funct = cmd.bits.inst.funct

  lazy val coreSelect = cmd.bits.core_id

  // val addressBits = (outer.mem ++ outer.producerBuffers.filter(_.isDefined).map(_.get).flatten.map(_.node) ++
  //   outer.remoteMem.filter(_.isDefined).map(_.get)).map { case m =>
  val addressBits = (outer.mem
    ++ outer.localWrite.filter(_.isDefined).map(_.get)
    ++ outer.remoteWrite.filter(_.isDefined).map(_.get)
    ++ outer.localRead.filter(_.isDefined).map(_.get)
    ++ outer.remoteRead.filter(_.isDefined).map(_.get)
    ).map { m =>
    val edge = m.edges.in(0)
    log2Up(edge.manager.maxAddress)
  } match {
    case Seq() => 0
    case l: Seq[Int] => l.max
    case _ => 0
  }

  lazy val numReadChannels = outer.readLoc.length
  lazy val numWriteChannels = outer.writeLoc.length
  lazy val numChannels = numReadChannels + numWriteChannels
  lazy val addrFile = Module(new SettingsFile(numChannels * cores.length, addressBits))

  lazy val resp = Wire(Decoupled(new ComposerRoccResponse())) //Queue(io.resp)

  //TODO: figure out hwo to deal with type param for arbiter
  //val arbiter = Module(new RRArbiter(new DKAggregatorCoreResponse(addressBits), nCores))
  //arbiter.io.in <> cores.map(_.io.resp)
  //resp.valid := arbiter.io.out.valid
  //resp.bits.rd := arbiter.io.out.bits.rd
  //resp.bits.data := Cat(arbiter.io.out.bits.id, arbiter.io.out.bits.len.asTypeOf(UInt(32.W)))
  //arbiter.io.out.ready := resp.ready

  def coreRWReady(c: Int): Bool = {
    RegNext(outer.readers(c).map(_.module.io.req.ready).fold(true.B) (_ && _) &&
      outer.writers(c).map(_.module.io.req.ready).fold(true.B)(_ && _))
  }

  //TODO: Have to assign length manually for now
  lazy val readLengths: Seq[Seq[UInt]] = outer.readers.map(_.map(_.module.io.req.bits.len))
  lazy val writeLengths: Seq[Seq[UInt]] = outer.writers.map(_.map(_.module.io.req.bits.len))


  lazy val coreReady = cores.zipWithIndex.map { case (core, i) =>
    (core.io.req.ready && coreRWReady(i)) || (coreSelect =/= i.U)
  }.reduce(_ && _)

  // connect cores to mem
  io.resp <> Queue(resp)

  if (outer.systemParams.doubleBuf) {
    outer.consumerBuffersModules.foreach { bufs =>
      if (bufs.isDefined) {
        bufs.get.zipWithIndex.foreach { case (buf, i) =>
          buf.asInstanceOf[DoubleTLRAM].module.io.swap.valid := false.B
          when(cores(i).io.req.fire) {
            buf.asInstanceOf[DoubleTLRAM].module.io.swap.valid := true.B
          }
        }
      }
    }
  }

  cores.zipWithIndex.foreach { case (core, i) =>
    // TO-DO: delay until all readers/writers are ready? use DecoupledHelper
    val coreStart = cmd.fire && funct === FUNC_START.U && coreSelect === i.U //&& coreRWReady(i)
    core.io.req.valid := coreStart
    core.io.req.bits := io.cmd.bits

    //rest are up to individual system
  }

  cmd.ready := funct =/= FUNC_START.U || coreReady

  io.busy := cores.map(_.io.busy).any

  addrFile.io.write.en := cmd.bits.inst.funct === FUNC_ADDR.U && cmd.fire
  addrFile.io.write.addr := cmd.bits.rs1 + (numChannels.U * coreSelect)
  addrFile.io.write.data := cmd.bits.rs2

  // connect readChannels and writeChannels appropriately
  cores.zipWithIndex.foreach { case (core, i) =>
    val cmdFireLatch = RegNext(cmd.fire)
    val cmdBitsLatch = RegNext(cmd.bits)
    val functLatch = cmdBitsLatch.inst.funct
    val coreSelectLatch = cmdBitsLatch.core_id

    core.readChannels.zipWithIndex.foreach { case (c, j) =>
      val r = outer.readers(i)(j)
      //        c.req <> r.module.io.req
      r.module.io.req.valid := cmdFireLatch && functLatch === FUNC_START.U && coreSelectLatch === i.U
      r.module.io.req.bits.addr := addrFile.io.values(i * numChannels + j)

      c <> SFQueue(outer.queueDepth, outer.modularInterface.readChannelParams(j).width)(r.module.io.channel)
    }

    core.writeChannels.zipWithIndex.foreach { case (c, j) =>
      val w = outer.writers(i)(j)
      //        c.req <> w.module.io.req
      w.module.io.req.valid := cmdFireLatch && functLatch === FUNC_START.U && coreSelectLatch === i.U
      w.module.io.req.bits.addr := addrFile.io.values(i * numChannels + numReadChannels + j)

      w.module.io.channel <> SFQueue(outer.queueDepth, outer.modularInterface.writeChannelParams(j).width)(c)
    }
  }

  // TO-DO: Generate value for r.module.io.req
  cores.zipWithIndex.foreach { case (_, i) =>
    readLengths(i).foreach(_ := RegNext(cmd.bits.rs2))
    writeLengths(i).foreach(_ := RegNext(cmd.bits.rs2))
  }
}

