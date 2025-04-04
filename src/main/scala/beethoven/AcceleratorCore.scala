package beethoven

import beethoven.MemoryStreams._
import BeethovenParams.CoreIDLengthKey
import IntraCoreMemoryPortInConfig._
import beethoven.AcceleratorCore.{commandExpectsResponse, systemOpCodeMap}
import beethoven.Protocol.RoCC._
import beethoven.Protocol.tilelink.MultiBeatCommandEmitter
import beethoven.Systems.{AcceleratorSystem, BeethovenCommandBundler, getCommMemAddress, getCommMemSpaceBits}
import beethoven.common._
import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

import scala.language.implicitConversions

class CustomIO[T1 <: AccelCommand, T2 <: AccelResponse](bundleIn: T1, bundleOut: T2, respExists: Boolean) extends Bundle {
  private[beethoven] val _req: DecoupledIO[T1] = DecoupledIO(bundleIn)
  private[beethoven] val _resp: Option[DecoupledIO[T2]] = if (respExists) Some(Flipped(DecoupledIO(bundleOut))) else None

  def req: DecoupledIO[T1] = _req

  def resp: DecoupledIO[T2] = if (respExists) _resp.get else
    throw new Exception("Tried to access the response bundle for a command that doesn't exist.\n" +
      s"Check the definition of your IO for this command/response port (${bundleIn.commandName})")
}

private[beethoven] object CustomCommandUsage extends Enumeration {
  //noinspection ScalaUnusedSymbol
  type custom_usage = Value
  val unused, default, custom = Value
}

class DataChannelIO(dWidth: Int) extends Bundle {
  val data = Decoupled(UInt(dWidth.W))
  val in_progress = Output(Bool())
}

object AcceleratorCore {
  private var systemOpCodeMap = List[(String, String, Int)]()

  private var commandExpectsResponse: List[(String, String, Boolean)] = List.empty

  implicit def addressToUInt(addr: Address): UInt = addr.address
}

object OuterKey extends Field[AcceleratorSystem]

class AcceleratorCore(implicit p: Parameters) extends Module {
  val outer: AcceleratorSystem = p(OuterKey)
  val io_declaration = IO(Flipped(new RoccExchange))
  io_declaration.resp.valid := false.B
  io_declaration.resp.bits := DontCare
  io_declaration.req.ready := false.B
  private[beethoven] var using_custom = CustomCommandUsage.unused

  def getIntraCoreMemOut(name: String): Seq[MemWritePort] = {
    val (params, match_params) = try {
      val q = outer.intraCoreMemMasters.find(_._1.name == name).get
      val r = p(AcceleratorSystems).find(_.name == q._1.toSystem).get.memoryChannelConfig.find(_.name == q._1.toMemoryPort).get.asInstanceOf[IntraCoreMemoryPortInConfig]
      (q, r)
    } catch {
      case e: Exception =>
        System.err.println("You may be trying to access a intra core mem port by the wrong name. Check your config.")
        throw e
    }
    val ports = intra_mem_outs(params._1)

    ports.zipWithIndex.map { case (port: TLBundle, channelIdx) =>
      val w = Wire(new MemWritePort(getCommMemSpaceBits(), port.params.dataBits,
        canSelectCore = has_core_select(match_params.communicationDegree),
        canSelectChannel = has_channel_select(match_params.communicationDegree)
      ))
      w.suggestName(s"intraCoreWritePort_for$name" + "_ch" + channelIdx)
      port.a.valid := w.valid
      w.ready := port.a.ready
      port.a.bits.source := 0.U
      port.a.bits.address := getCommMemAddress(params._1.toSystem, w.bits.core.getOrElse(0), params._1.toMemoryPort, w.bits.channel.getOrElse(0), w.bits.addr, CLog2Up(port.params.dataBits / 8))
      port.a.bits.size := CLog2Up(port.params.dataBits / 8).U
      port.a.bits.data := w.bits.data
      port.a.bits.mask := BigInt("1" * (match_params.dataWidthBits / 8), 2).U
      port.a.bits.param := DontCare
      port.a.bits.opcode := TLMessages.PutFullData
      port.a.bits.corrupt := false.B
      port.d.ready := true.B
      w
    }
  }

  def getIntraCoreMemIns(name: String): Seq[Seq[ScratchpadDataPort]] = {
    if (!intra_mem_ins.exists(_._1 == name))
      throw new Exception(s"Attempting to access intraCoreMem \"$name\" which we can't find in the config.")
    intra_mem_ins(name)
  }

  def getIntraCoreMemIn(name: String, channelIdx: Int): Seq[ScratchpadDataPort] = {
    getIntraCoreMemIns(name)(channelIdx)
  }

  case class ReaderModuleChannel(requestChannel: DecoupledIO[ChannelTransactionBundle], dataChannel: DataChannelIO)

  case class WriterModuleChannel(requestChannel: DecoupledIO[ChannelTransactionBundle], dataChannel: WriterDataChannelIO)

  case class ScratchpadModuleChannel(requestChannel: ScratchpadMemReqPort, dataChannels: Seq[ScratchpadDataPort])

  def getReaderModule(name: String, idx: Int = 0): ReaderModuleChannel = {
    val a = getReaderModules(name, Some(idx))
    ReaderModuleChannel(a._1(0), a._2(0))
  }

  val read_ios = Map.from(outer.memParams.filter(_.isInstanceOf[ReadChannelConfig]).map {
    case mp: ReadChannelConfig =>
      (mp.name,
        (mp, (0 until mp.nChannels).map { channelIdx =>
          val io_pair = (IO(Decoupled(new ChannelTransactionBundle)), IO(Flipped(new DataChannelIO(mp.dataBytes * 8))))
          io_pair._1.suggestName(s"readRequest_${mp.name}_channel${channelIdx}")
          io_pair._2.suggestName(s"readData_${mp.name}_channel${channelIdx}")
          io_pair
        }))
  })
  val write_ios = Map.from(outer.memParams.filter(_.isInstanceOf[WriteChannelConfig]).map {
    case mp: WriteChannelConfig =>
      (mp.name,
        (mp, (0 until mp.nChannels).map { channelIdx =>
          val io_pair = (IO(Decoupled(new ChannelTransactionBundle)), IO(Flipped(new WriterDataChannelIO(mp.dataBytes * 8))))
          io_pair._1.suggestName(s"writeRequest_${mp.name}_channel${channelIdx}")
          io_pair._2.suggestName(s"writeData_${mp.name}_channel${channelIdx}")
          io_pair
        }))
  })
  val sp_ios = Map.from(outer.memParams.filter(_.isInstanceOf[ScratchpadConfig]).map {
    case mp: ScratchpadConfig =>
      (mp.name,
        (mp, ( {
          val io = IO(Flipped(new ScratchpadMemReqPort(mp.nDatas.intValue())))
          io.suggestName(s"scratchpadRequest_${mp.name}")
          io
        },
          (0 until mp.nPorts).map { ch: Int =>
            val io = IO(Flipped(new ScratchpadDataPort(log2Up(mp.nDatas.intValue()), mp.dataWidthBits.intValue())))
            io.suggestName(s"scratchpadData_${mp.name}_channel${ch}")
            io
          })))
  })

  val intra_mem_ins = Map.from(outer.memParams.filter(_.isInstanceOf[IntraCoreMemoryPortInConfig]).map {
    case mp: IntraCoreMemoryPortInConfig => (mp.name, (0 until mp.nChannels).map { ch: Int =>
      (0 until mp.portsPerChannel) map { port_idx =>
        val io = IO(Flipped(new ScratchpadDataPort(log2Up(mp.nDatas.intValue()), mp.dataWidthBits.intValue())))
        io.suggestName(s"intra_mem_in_${mp.name}_channel${ch}_port${port_idx}")
        io
      }
    })
  })

  val intra_mem_outs = Map.from(outer.memParams.filter(_.isInstanceOf[IntraCoreMemoryPortOutConfig]).map {
    case mp: IntraCoreMemoryPortOutConfig =>
      (mp, {
        val matching = p(AcceleratorSystems).find(_.name == mp.toSystem).get.memoryChannelConfig.find(_.name == mp.toMemoryPort).get.asInstanceOf[IntraCoreMemoryPortInConfig]
        (0 until mp.nChannels).map { channel_idx: Int =>
          val io = IO(new TLBundle(outer.intraCoreMemMasters(0)._2(0).out(0)._1.params.copy(dataBits = matching.dataWidthBits)))
          io.suggestName(f"intra_mem_out_to${mp.toSystem}_${mp.toMemoryPort}_ch${channel_idx}")
          io
        }
      })
  })


  /**
   * Declare reader module implementations associated with a certain channel name.
   * Data channel will read out a vector of UInts of dimension (vlen, dataBytes*8 bits)
   *
   * @return List of transaction information bundles (address and length in bytes) and then a data channel. For
   *         sparse readers, we give back both interfaces and for non-sparse, addresses are provided through separate address
   *         commands in software.
   */
  def getReaderModules(name: String, idx: Option[Int] = None):
  (List[DecoupledIO[ChannelTransactionBundle]], List[DataChannelIO]) = {
    idx match {
      case None =>
        val q = read_ios(name)
        (q._2.map(_._1).toList, q._2.map(_._2).toList)
      case Some(id) =>
        val q = read_ios(name)
        (List(q._2(id)._1), List(q._2(id)._2))
    }
  }

  def getWriterModule(name: String,
                      idx: Int = 0): WriterModuleChannel = {
    val a = getWriterModules(name, Some(idx))
    WriterModuleChannel(a._1(0), a._2(0))
  }

  def getScratchpad(name: String): ScratchpadModuleChannel = {
    val a = sp_ios(name)._2
    a._1.writeback.valid := false.B
    a._1.writeback.bits := DontCare
    ScratchpadModuleChannel(a._1, a._2)
  }

  def getWriterModules(name: String,
                       idx: Option[Int] = None): (List[DecoupledIO[ChannelTransactionBundle]], List[WriterDataChannelIO]) = {
    idx match {
      case None =>
        val q = write_ios(name)
        (q._2.map(_._1).toList, q._2.map(_._2).toList)
      case Some(id) =>
        val q = write_ios(name)
        (List(q._2(id)._1), List(q._2(id)._2))
    }
  }

  def BeethovenIO[T1 <: AccelCommand](bundleIn: T1): CustomIO[T1, AccelRoccUserResponse] = {
    BeethovenIO[T1, AccelRoccUserResponse](bundleIn, InvalidAccelResponse())
  }

  def getSystemID(name: String): UInt = p(AcceleratorSystems).indexWhere(_.name == name).U

  private var nCommands = 0

  def BeethovenIO[T1 <: AccelCommand, T2 <: AccelResponse](bundleIn: T1, bundleOut: T2): CustomIO[T1, T2] = {
    if (using_custom == CustomCommandUsage.default) {
      throw new Exception("Cannot use custom io after using the default io")
    }
    using_custom = CustomCommandUsage.custom
    val opCode = if (!systemOpCodeMap.exists(a => a._1 == outer.systemParams.name && a._2 == bundleIn.commandName)) {
      systemOpCodeMap = (outer.systemParams.name, bundleIn.commandName, nCommands) :: systemOpCodeMap
      nCommands
    } else {
      systemOpCodeMap.filter(a => a._1 == outer.systemParams.name && a._2 == bundleIn.commandName)(0)._3
    }

    if (commandExpectsResponse.exists(a => outer.systemParams.name == a._1 && bundleIn.commandName == a._2)) {
    } else {
      commandExpectsResponse = (outer.systemParams.name, bundleIn.commandName, !bundleOut.isInstanceOf[InvalidAccelResponse]) :: commandExpectsResponse
    }


    val beethovenCustomCommandManager = Module(new BeethovenCommandBundler[T1, T2](
      bundleIn, bundleOut, outer, !bundleOut.isInstanceOf[InvalidAccelResponse], opCode))
    beethovenCustomCommandManager.cio.req.bits := DontCare
    beethovenCustomCommandManager.cio.req.valid := false.B
    beethovenCustomCommandManager.cio.resp.ready := false.B
    if (!bundleOut.isInstanceOf[InvalidAccelResponse]) {
      beethovenCustomCommandManager.io.resp.valid := false.B
      beethovenCustomCommandManager.io.resp.bits.rd := DontCare
    }

    beethovenCustomCommandManager.suggestName(outer.systemParams.name + "CustomCommand")

    when(io_declaration.req.bits.inst.funct === opCode.U) {
      beethovenCustomCommandManager.cio.req <> io_declaration.req
    }

    when(beethovenCustomCommandManager.io_am_active) {
      io_declaration.resp.valid := beethovenCustomCommandManager.cio.resp.valid
      io_declaration.resp.bits := beethovenCustomCommandManager.cio.resp.bits
      beethovenCustomCommandManager.cio.resp.ready := io_declaration.resp.ready
    }

    nCommands = nCommands + 1
    beethovenCustomCommandManager.io
  }

  def RoccBeethovenIO(): RoccExchange = {
    if (using_custom == CustomCommandUsage.custom) {
      throw new Exception("Cannot use io after generating a custom io")
    }
    using_custom = CustomCommandUsage.default
    io_declaration
  }

  val beethoven_rocc_exchanges = outer.systemParams.canIssueCoreCommandsTo.map { target =>
    (target, {
      val io = IO(new RoccExchange())
      io.suggestName(f"externalCommandInterface_to${target}")
      io
    })
  }

  class IntraCoreIO[Tcmd <: AccelCommand, Tresp <: AccelResponse](genCmd: Tcmd, genResp: Tresp) extends Bundle {
    val req = Decoupled(new Bundle {
      val payload: Tcmd = genCmd.cloneType
      val target_core_idx: UInt = UInt(CoreIDLengthKey.W)
    })
    val resp = Flipped(Decoupled(genResp.cloneType))
  }

  def getIntraSysIO[T <: AccelCommand, R <: AccelResponse](targetSys: String, cmdName: String,
                                                           cmdGen: T, respGen: R): IntraCoreIO[T, R] = {
    val target_exchange = beethoven_rocc_exchanges.find(_._1 == targetSys).get
    val sys_io = Wire(Output(new IntraCoreIO[T, R](cmdGen, respGen)))
    val opcode = systemOpCodeMap.filter(a => a._1 == targetSys && a._2 == cmdName) match {
      case Nil => throw new Exception(s"Could not find opcode for $cmdName in $targetSys")
      case _ => systemOpCodeMap.filter(a => a._1 == targetSys && a._2 == cmdName).head._3
    }
    val expectResponse = commandExpectsResponse.filter(a => a._1 == targetSys && a._2 == cmdName) match {
      case Nil => throw new Exception(s"Could not find response expectation for $cmdName in $targetSys")
      case _ => commandExpectsResponse.filter(a => a._1 == targetSys && a._2 == cmdName).head._3
    }
    val emitter = Module(new MultiBeatCommandEmitter[T](cmdGen, expectResponse, opcode))
    emitter.out <> target_exchange._2.req
    emitter.in.valid := sys_io.req.valid
    emitter.in.bits <> sys_io.req.bits.payload
    emitter.in.bits.__core_id := sys_io.req.bits.target_core_idx
    emitter.in.bits.__system_id := getSystemID(targetSys)
    sys_io.resp.valid := target_exchange._2.resp.valid
    sys_io.resp.bits.getDataField := target_exchange._2.resp.bits.getDataField
    target_exchange._2.resp.ready := sys_io.resp.ready

    sys_io
  }
}

class AcceleratorBlackBoxCore(blackboxBuilder: ModuleConstructor)(implicit p: Parameters, systemParams: AcceleratorSystemConfig)
  extends AcceleratorCore {
  override val desiredName = systemParams.name + "Wrapper"


  val aio = blackboxBuilder match {
    case bbc: BlackboxBuilderCustom[_, _] =>
      BeethovenIO(bbc.coreCommand, bbc.coreResponse)
  }

  val readerParams = systemParams.memoryChannelConfig.filter(_.isInstanceOf[ReadChannelConfig]).map(_.asInstanceOf[ReadChannelConfig])
  val writerParams = systemParams.memoryChannelConfig.filter(_.isInstanceOf[WriteChannelConfig]).map(_.asInstanceOf[WriteChannelConfig])
  val spParams = systemParams.memoryChannelConfig.filter(_.isInstanceOf[ScratchpadConfig]).map(_.asInstanceOf[ScratchpadConfig])

  val rrio = readerParams.map(pr => getReaderModules(pr.name))
  val writerIOs = writerParams.map(pr => getWriterModules(pr.name))
  val spIOs = spParams.map(pr => getScratchpad(pr.name))

  class bb extends BlackBox {
    override val desiredName = systemParams.name + "Wrapper"
    val io = IO(new Bundle {

      val clock = Input(Clock())
      val reset = Input(Reset())

      val cmd = Flipped(Decoupled(aio.req.bits.cloneType))
      val resp = Decoupled(aio.resp.bits.cloneType)

      val read_req = MixedVec(rrio.map(rr => MixedVec(rr._1.map(rrr => Decoupled(rrr.bits.cloneType)))))
      val read_data = MixedVec(rrio.map(rr => MixedVec(rr._2.map(rrd => Flipped(Decoupled(rrd.data.bits.cloneType))))))
      val read_inProgress = MixedVec(rrio.map(rr => MixedVec(rr._2.map(_ => Input(Bool())))))
      val write_req = MixedVec(writerIOs.map(wr => MixedVec(wr._1.map(wrr => Decoupled(wrr.bits.cloneType)))))
      val write_data = MixedVec(writerIOs.map(wr => MixedVec(wr._2.map(wrd => Decoupled(wrd.data.bits.cloneType)))))
      val write_isFlushed = MixedVec(writerIOs.map(wr => MixedVec(wr._2.map(_ => Input(Bool())))))
      val sp_req = MixedVec(spIOs.map(wr => Decoupled(wr.requestChannel.cloneType)))
      val sp_data_req = MixedVec(spIOs.map(wr => MixedVec(wr.dataChannels.map(spd => Decoupled(spd.req.bits.cloneType)))))
      val sp_data_resp = MixedVec(spIOs.map(sp => MixedVec(sp.dataChannels.map(spr => Flipped(Valid(spr.res.bits.cloneType))))))
      //      sp_data.zip(spParams).foreach { case (a, b) => a.suggestName(b.name + "_data") }

    })
  }

  val impl = Module(new bb)
  val OUTPUT = true
  val INPUT = false

  class VerilogPort(nm: String, val dir: Boolean, val dim: Seq[Int], val sources: Seq[String]) {
    val name = nm.strip().stripSuffix("_")

    override def toString: String = f"PORT[nm:'$nm', out:$dir, srcs: $sources]"
  }

  object VerilogPort {
    def apply(str: String, bool: Boolean, value: Seq[Int], value1: Seq[String]): VerilogPort = {
      new VerilogPort(str, bool, value, value1)
    }
  }

  def getRecursiveNames(a: Data, other: Seq[(Data, String)] = Seq()): Seq[(Data, String)] = {
    a match {
      case b: Bundle =>
        var acc = other
        b.elements.foreach { case (_, data) =>
          acc = getRecursiveNames(data, acc)
        }
        acc
      case v: Vec[_] =>
        var acc = other
        v.zipWithIndex.foreach { case (data, _) =>
          acc = getRecursiveNames(data, acc)
        }
        acc
      case _ => other :+ (a, a.instanceName)
    }
  }

  def fixBase(a: String): String = {
    val q = a.replace(".", "_")
    if (q.contains("[")) {
      val idx = q.indexOf("[")
      q.substring(0, q.indexOf("[")) + "_" + q.substring(idx + 1, q.indexOf("]"))
    } else {
      q
    }
  }

  def fix2Real(a: String): String = {
    a.replace(".", "_").replace("bits_", "")
  }

  // for reads, there are a few dimensions. The first index is the read channel name itself, the second index is
  // the channel number, and the third index (if applicable) is the vector index

  def getStructureAsPorts(a: Data,
                          primaryDirection: Boolean,
                          structureDepth: Int = 0,
                          yieldSubfieldOnlyWithPrefix: Option[String] = None): Iterable[VerilogPort] = {
    def getRName(s: String): String = {
      yieldSubfieldOnlyWithPrefix match {
        case None => fix2Real(s)
        case Some(t) => t + "_" + s.split("\\.").takeRight(structureDepth).mkString("_")
      }
    }

    a match {
      case _: EmptyAccelResponse =>
        Seq()
      case v: Vec[_] =>
        v.zipWithIndex.flatMap { case (data, _) =>
          getStructureAsPorts(data, primaryDirection, structureDepth + 1, yieldSubfieldOnlyWithPrefix)
        }
      case de: DecoupledIO[_] =>
        val v_iname = de.valid.instanceName
        val r_iname = de.ready.instanceName
        Seq(
          VerilogPort(getRName(v_iname), primaryDirection, Seq(1), Seq(fixBase(v_iname))),
          VerilogPort(getRName(r_iname), !primaryDirection, Seq(1), Seq(fixBase(r_iname)))) ++
          getStructureAsPorts(de.bits, primaryDirection, structureDepth + 1, yieldSubfieldOnlyWithPrefix)
      case b: Bundle =>
        b.elements.flatMap { case (_, data) =>
          getStructureAsPorts(data, primaryDirection, structureDepth + 1, yieldSubfieldOnlyWithPrefix)
        }
      case b =>
        Seq(VerilogPort(getRName(b.instanceName), primaryDirection, Seq(b.getWidth),
          Seq(fixBase(b.instanceName))))

    }
  }

  def getRWChannelAsPorts[T <: Data](a: MixedVec[MixedVec[T]], ps: List[MemChannelConfig], prefix: String, primaryDirection: Boolean): Iterable[VerilogPort] = {
    // first dimension corresponds directly to Channel Param names
    a.zip(ps).flatMap { case (mv: MixedVec[T], param: MemChannelConfig) =>
      val portName = param.name + "_"
      // second dimension corresponds to channel number

      val channelLen = param.nChannels

      mv.zipWithIndex.flatMap { case (v: T, idx: Int) =>
        val channelName = channelLen match {
          case 1 => ""
          case _ => "_channel" + idx
        }
        val base = portName + prefix + channelName
        // if the field is a Decoupled, we need to take apart that field
        v match {
          case d: DecoupledIO[_] =>
            val valid = d.valid.instanceName
            val ready = d.ready.instanceName
            Seq(
              VerilogPort(base + "_valid", primaryDirection, Seq(1), Seq(fixBase(valid))),
              VerilogPort(base + "_ready", !primaryDirection, Seq(1), Seq(fixBase(ready)))) ++
              (d.bits match {
                case vec: Vec[_] =>
                  val fieldNames = getRecursiveNames(vec).map(_._2)
                  val sources = fieldNames.map(fixBase)
                  Seq(VerilogPort(base, primaryDirection, Seq(1, vec(0).getWidth), sources))
                case _ =>
                  getStructureAsPorts(d.bits, primaryDirection, yieldSubfieldOnlyWithPrefix = Some(base))
              })
          case _ =>
            getStructureAsPorts(v, primaryDirection)
        }
      }
    }
  }

  impl.io.clock := clock
  impl.io.reset := reset

  val cmd_fields = getStructureAsPorts(impl.io.cmd, INPUT)
  val resp_fields = getStructureAsPorts(impl.io.resp, OUTPUT)
  val rr_fields = getRWChannelAsPorts(impl.io.read_req, readerParams, "req", OUTPUT)
  val rd_fields = getRWChannelAsPorts(impl.io.read_data, readerParams, "data", INPUT)
  val wr_fields = getRWChannelAsPorts(impl.io.write_req, writerParams, "req", OUTPUT)
  val wd_fields = getRWChannelAsPorts(impl.io.write_data, writerParams, "data", OUTPUT)
  val spr_fields = getRWChannelAsPorts(impl.io.sp_data_req, spParams, "req", OUTPUT)
  val spd_fields = getRWChannelAsPorts(impl.io.sp_data_resp, spParams, "resp", INPUT)

  impl.io.read_req.zip(rrio).foreach { case (a, b) => a.zip(b._1).foreach { case (c, d) => c <> d } }
  impl.io.read_data.zip(rrio).foreach { case (a, b) => a.zip(b._2).foreach { case (c, d) => c <> d.data } }
  impl.io.read_inProgress.zip(rrio).foreach { case (a, b) => a.zip(b._2).foreach { case (c, d) => c <> d.in_progress } }
  impl.io.write_req.zip(writerIOs).foreach { case (a, b) => a.zip(b._1).foreach { case (c, d) => c <> d } }
  impl.io.write_data.zip(writerIOs).foreach { case (a, b) => a.zip(b._2).foreach { case (c, d) => c <> d.data } }
  impl.io.write_isFlushed.zip(writerIOs).foreach { case (a, b) => a.zip(b._2).foreach { case (c, d) => c <> d.isFlushed } }
  impl.io.sp_data_req.zip(spIOs).foreach { case (a, b) => a.zip(b.dataChannels.map(_.req)).foreach { case (c, d) => c <> d } }
  impl.io.sp_data_resp.zip(spIOs).foreach { case (a, b) => a.zip(b.dataChannels.map(_.res)).foreach { case (c, d) => c <> d } }

  impl.io.sp_req.zip(spIOs).foreach { case (a, b) => a <> b.requestChannel }
  impl.io.cmd <> aio.req
  impl.io.resp <> aio.resp

  def getVerilogPorts(m: Iterable[VerilogPort]): String = {
    def len_of_io(L: Seq[Int]): Int = if (L.head == 1) 0 else 4 + Math.log10(L.head).ceil.toInt
    val longest_io = m.map(a => len_of_io(a.dim)).max
    m.map { a =>
      val dim = a.dim.map { b =>
        if (b == 1) "" else s"[${b - 1}:0]"
      }.reverse
      val dir = if (a.dir == OUTPUT) "output" else "input "
      val dim_pad = dim.head + " " * (longest_io - dim.head.length)
      s"  $dir ${dim_pad} ${a.name}${(if (dim.tail.isEmpty) "" else " ") + dim.tail.mkString("")}"
    }.mkString(",\n")
  }

  def getVerilogPortsOfSources(m: Iterable[VerilogPort]): String = {
    m.flatMap { a =>
      val dir = if (a.dir == OUTPUT) "output" else "input"
      val finalDim = a.dim.last
      val dstr = if (finalDim == 1) "\t" else s"[${finalDim - 1}:0]"
      a.sources.map { src =>
        f"  $dir $dstr ${fixBase(src)}"
      }
    }.mkString(",\n")
  }

  def getVerilogModulePortInstantiation(m: Iterable[VerilogPort]): (String, String) = {
    def safeMkString(b: String, d: String, additive: String): String = if (b.isEmpty) d else {
      if (d.isEmpty) b else b + additive + d
    }

    m.map { a =>
      if (a.sources.length == 1) {
        // then just wire the port directly
        (s"  .${a.name}(${a.sources(0)})", "")
      } else {
        // otherwise, we need to declare an array wire, wire up the array, and use that wire
        // as the IO
        val wireName = a.name + "_wire"
        val init = s"  .${a.name}($wireName)"
        val wireDeclaration = f"  wire [${a.dim.head - 1}:0] $wireName${a.dim.tail.map(d => "[" + (d - 1) + ":0]").mkString("")};"
        val wireInit = a.sources.zipWithIndex.map { case (src, idx) =>
          s"  assign $wireName[$idx] = $src;"
        }.mkString("\n")
        (init, wireDeclaration + "\n" + wireInit)
      }
    }.foldLeft(("", "")) { case ((a, b), (c, d)) => (safeMkString(a, c, ",\n"), safeMkString(b, d, "\n")) }
  }

  // filter out secret fields
  val allIOs = (cmd_fields ++ resp_fields ++ rr_fields ++ rd_fields ++ wr_fields ++ wd_fields
    ++ spr_fields ++ spd_fields).filter(!_.name.contains("__"))

  val userBB =
    f"""
       |module ${systemParams.name} (
       |  input clock,
       |  input reset,
       |
       |${getVerilogPorts(allIOs)}
       |);
       |
       |endmodule
       |""".stripMargin

  val (portInit, wireDec) = getVerilogModulePortInstantiation(allIOs)
  val bbWrapper =
    f"""
       |module ${this.desiredName} (
       |  input clock,
       |  input reset,
       |${getVerilogPortsOfSources(allIOs)}
       |  );
       |
       |$wireDec
       |
       |${systemParams.name} ${systemParams.name}_inst (
       |  .clock(clock),
       |  .reset(reset),
       |$portInit
       |  );
       |
       |endmodule
       |
       |""".stripMargin

  // Link in Wrapper using BeethovenBuild,
  // write source to file first
  val wrapperFName = BeethovenBuild.hw_build_dir / s"${systemParams.name}_chiselLink.v"
  os.write.over(wrapperFName, bbWrapper)

  val bbFName = BeethovenBuild.hw_build_dir / s"${systemParams.name}.v"
  os.write.over(bbFName, userBB)
}