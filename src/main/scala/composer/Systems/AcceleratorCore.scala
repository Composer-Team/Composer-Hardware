package composer.Systems

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util._
import composer._
import composer.ComposerParams.{CoreIDLengthKey, SystemIDLengthKey}
import composer.Generation.ComposerBuild
import composer.MemoryStreams.{ScratchpadDataPort, ScratchpadMemReqPort, _}
import composer.RoccHelpers._
import composer.TLManagement.{ComposerIntraCoreIOModule, TLClientModule, TLClientModuleIO}
import composer.common._
import composer.Generation.Tune.Tunable
import composer.Systems.AcceleratorCore.Address
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleField

import java.io.File
import scala.collection.immutable.Seq
import scala.language.implicitConversions

class CustomIO[T1 <: Bundle, T2 <: Bundle](bundleIn: T1, bundleOut: T2, val functionID: Int) extends Bundle {
  val req: DecoupledIO[T1] = DecoupledIO(bundleIn)
  val resp: DecoupledIO[T2] = Flipped(DecoupledIO(bundleOut))
}

private[composer] object CustomCommandUsage extends Enumeration {
  //noinspection ScalaUnusedSymbol
  type custom_usage = Value
  val unused, default, custom = Value
}

class CustomIOWithRouting[T1 <: Bundle, T2 <: Bundle](bundleIn: T1, bundleOut: T2) extends Bundle {
  val req: DecoupledIOWithCRouting[T1] = DecoupledIOWithCRouting(bundleIn)
  val resp: DecoupledIO[T2] = Flipped(DecoupledIO(bundleOut))
}

class ComposerCoreIO(implicit p: Parameters) extends CustomIO[AccelRoccCommand, AccelRoccUserResponse](new AccelRoccCommand, new AccelRoccUserResponse, -1)

class DataChannelIO(dWidth: Int) extends Bundle {
  val data = Decoupled(UInt(dWidth.W))
  val in_progress = Output(Bool())
}

object AcceleratorCore {
  class Address(addrBits: Int) extends Bundle {
    val address = UInt(addrBits.W)

    def :=(other: Data): Unit = {
      address := other
    }

    implicit def toUInt: UInt = address
  }
}

class AcceleratorCore(val outer: ComposerSystem)(implicit p: Parameters) extends Module {
  val composer_response_ios_ = Map.from(outer.systemParams.canIssueCoreCommandsTo.map { target =>
    (target, {
      val io = IO(Flipped(Decoupled(new AccelRoccResponse())))
      io.suggestName(s"ComposerIntraCoreResponsePort_$target")
      io
    })
  })

  val composer_command_ios_ = Map.from(outer.systemParams.canIssueCoreCommandsTo.map { name =>
    (name, IO(Decoupled(new TLClientModuleIO())))
  })

  val io_declaration = IO(Flipped(new ComposerCoreIO()))
  io_declaration.resp.valid := false.B
  io_declaration.resp.bits := DontCare
  io_declaration.req.ready := false.B
  val io_source = IO(Input(UInt(log2Up(outer.acc.sysNCmdSourceLookup(outer.systemParams.name)).W)))
  private[composer] var using_custom = CustomCommandUsage.unused

  //  def getCoreID: Int = composerConstructor.composerCoreWrapper.core_id

  def getIntraCoreMemOuts(name: String): CCoreChannelMultiAccessBundle[MemWritePort] = {
    val params = try {
      outer.intraCoreMemMasters.filter(_._1.name == name)(0)
    } catch {
      case e: Exception =>
        System.err.println("You may be trying to access a intra core mem port by the wrong name. Check your config.")
        throw e
    }

    val q = params._2.indices map (getIntraCoreMemOut(name, _))
    CCoreChannelMultiAccessBundle(q)
  }

  def getIntraCoreMemOut(name: String, core_idx: Int = 0): Seq[MemWritePort] = {
    val params = try {
      outer.intraCoreMemMasters.filter(_._1.name == name)(0)
    } catch {
      case e: Exception =>
        System.err.println("You may be trying to access a intra core mem port by the wrong name. Check your config.")
        throw e
    }

    params._2(core_idx).zipWithIndex.map { case (master_singleton, channelIdx) =>
      val master = master_singleton.out(0)
      val port = master._1
      val edge = master._2
      val w = Wire(new MemWritePort(log2Up(params._3.nDatas.intValue()), params._3.dataWidthBits.intValue()))
      w.suggestName(s"intraCoreWritePort_for$name" + "_" + channelIdx)
      port.a.valid := w.valid
      w.ready := port.a.ready
      port.a.bits := edge.Put(
        fromSource = 0.U,
        toAddress = if (params._3.dataWidthBits.intValue() == 8) w.bits.addr else Cat(w.bits.addr, 0.U(log2Up(params._3.dataWidthBits.intValue() / 8).W)),
        lgSize = CLog2Up(params._3.dataWidthBits.intValue() / 8).U,
        data = w.bits.data
      )._2
      port.d.ready := true.B
      w
    }
  }

  def getIntraCoreMemIns(name: String)(implicit valName: ValName): Seq[ScratchpadDataPort] = {
    val params = try {
      outer.intraCoreMemSlaveNodes.filter(_._1 == name)(0)
    } catch {
      case e: Exception =>
        System.err.println(s"You may be trying to access a intra core mem port by the wrong name($name). Check your config.")
        throw e
    }
    implicit val nameHint = Some(valName.name)
    VecInit((0 until params._3.nChannels) map (getIntraCoreMemIn(name, _)))
  }

  def getIntraCoreMemIn(name: String, idx: Int): ScratchpadDataPort = {
    val params = outer.intraCoreMemSlaveNodes.filter(_._1 == name)
    if (params.isEmpty) throw new Exception(s"Attempting to access intraCoreMem \"$name\" which we can't find in the config.")
    val ic_scratchpad = params(0)
    ic_scratchpad._4(idx).module.IOs(0)
  }

  case class ReaderModuleChannel(requestChannel: DecoupledIO[ChannelTransactionBundle], dataChannel: DataChannelIO)

  case class WriterModuleChannel(requestChannel: DecoupledIO[ChannelTransactionBundle], dataChannel: WriterDataChannelIO)

  case class ScratchpadModuleChannel(requestChannel: ScratchpadMemReqPort, dataChannels: Seq[ScratchpadDataPort])

  //  private def getMemPWidth(name: String): Int = {
  //    outer.memParams.filter(_.name == name)(0).asInstanceOf[CReadChannelParams].d
  //    ataBytes * 8
  //  }
  //
  def getReaderModule(name: String, idx: Int = 0): ReaderModuleChannel = {
    val a = getReaderModules(name, Some(idx))
    ReaderModuleChannel(a._1(0), a._2(0))
  }

  val read_ios = Map.from(outer.memParams.filter(_.isInstanceOf[CReadChannelParams]).map {
    case mp: CReadChannelParams =>
      (mp.name,
        (mp, (0 until mp.nChannels).map { channelIdx =>
          val io_pair = (IO(Decoupled(new ChannelTransactionBundle)), IO(Flipped(new DataChannelIO(mp.dataBytes * 8))))
          io_pair._1.suggestName(s"readRequest_${mp.name}_channel${channelIdx}")
          io_pair._2.suggestName(s"readData_${mp.name}_channel${channelIdx}")
          io_pair
        }))
  })
  val write_ios = Map.from(outer.memParams.filter(_.isInstanceOf[CWriteChannelParams]).map {
    case mp: CWriteChannelParams =>
      (mp.name,
        (mp, (0 until mp.nChannels).map { channelIdx =>
          val io_pair = (IO(Decoupled(new ChannelTransactionBundle)), IO(Flipped(new WriterDataChannelIO(mp.dataBytes * 8))))
          io_pair._1.suggestName(s"writeRequest_${mp.name}_channel${channelIdx}")
          io_pair._2.suggestName(s"writeData_${mp.name}_channel${channelIdx}")
          io_pair
        }))
  })
  val sp_ios = Map.from(outer.memParams.filter(_.isInstanceOf[CScratchpadParams]).map {
    case mp: CScratchpadParams =>
      (mp.name,
        (mp, (IO(new ScratchpadMemReqPort(_addrBits, mp.nDatas.intValue())), (0 until mp.nPorts).map(_ =>
          IO(Flipped(new ScratchpadDataPort(log2Up(mp.nDatas.intValue()), mp.dataWidthBits.intValue())))
        ))))
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
    //    val params = outer.memParams.filter(_.name == name)(0).asInstanceOf[CReadChannelParams]
    //    val mod = idx match {
    //      case Some(id_unpack) =>
    //        val clients = getTLClients(name, outer.readers)
    //        List(Module(new SequentialReader(
    //          params.dataBytes * 8,
    //          tl_bundle = clients(id_unpack).out(0)._1,
    //          tl_edge = clients(id_unpack).out(0)._2)))
    //      case None =>
    //        getTLClients(name, outer.readers).map(tab_id =>
    //          Module(new SequentialReader(
    //            params.dataBytes * 8,
    //            tl_bundle = tab_id.out(0)._1,
    //            tl_edge = tab_id.out(0)._2)))
    //    }
    //    //noinspection DuplicatedCode
    //    mod foreach { m =>
    //      m.tl_out <> m.tl_bundle
    //      m.suggestName(name)
    //    }
    //    val ret = (mod.map(_.io.req), mod.map(_.io.channel))
    //    // initially tie off everything to false and DontCare. Saves some pain down the line
    //    ret._2.foreach { dat =>
    //      dat.data.ready := false.B
    //    }
    //    ret._1.foreach { dat =>
    //      dat.bits := DontCare
    //      dat.valid := false.B
    //    }
    //    ret
  }

  def getWriterModule(name: String,
                      idx: Int = 0): WriterModuleChannel = {
    val a = getWriterModules(name, Some(idx))
    WriterModuleChannel(a._1(0), a._2(0))
  }

  def getScratchpad(name: String): ScratchpadModuleChannel = {
    //      val lm = outer.scratch_mod.filter(_._1 == name)(0)._2
    //      lm.suggestName(name)
    //      val mod = lm.module
    //
    //      ScratchpadModuleChannel(mod.req, mod.IOs)
    val a = sp_ios(name)._2
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

  def ComposerIO[T1 <: AccelCommand](bundleIn: T1): CustomIO[T1, AccelRoccUserResponse] = {
    ComposerIO[T1, AccelRoccUserResponse](bundleIn, new AccelRoccUserResponse)
  }

  def getSystemID(name: String): UInt = p(SystemName2IdMapKey)(name).U

  implicit def _addrBits: Int = log2Up(p(ExtMem).get.master.size)

  private var nCommands = 0

  def Address(): Address = {
    new Address(_addrBits)
  }

  implicit def addrToUInt(address: Address): UInt = address.address

  def ComposerIO[T1 <: AccelCommand, T2 <: AccelResponse](bundleIn: T1, bundleOut: T2): CustomIO[T1, T2] = {
    if (using_custom == CustomCommandUsage.default) {
      throw new Exception("Cannot use custom io after using the default io")
    }
    using_custom = CustomCommandUsage.custom
    val composerCustomCommandManager = Module(new ComposerCommandBundler[T1, T2](
      bundleIn, bundleOut, outer, outer.acc.sysNCmdSourceLookup(outer.systemParams.name), nCommands))
    composerCustomCommandManager.cio.cmd.req.bits := DontCare
    composerCustomCommandManager.cio.cmd.req.valid := false.B
    composerCustomCommandManager.cio.cmd_in_source := io_source

    composerCustomCommandManager.suggestName(outer.systemParams.name + "CustomCommand")
    val thisCommandInFlight = RegInit(false.B)
    composerCustomCommandManager.io.resp.bits.rd := 0.U

    when(io_declaration.req.bits.inst.funct === nCommands.U) {
      composerCustomCommandManager.cio.cmd.req <> io_declaration.req
      when (io_declaration.req.fire) {
        thisCommandInFlight := io_declaration.req.bits.inst.xd
      }
    }

    when(thisCommandInFlight) {
      io_declaration.resp <> composerCustomCommandManager.cio.cmd.resp
      when (io_declaration.resp.fire) {
        thisCommandInFlight := false.B
      }
    }.otherwise {
      composerCustomCommandManager.cio.cmd.resp.ready := false.B
    }

    nCommands = nCommands + 1
    composerCustomCommandManager.io
  }

  def ComposerIO(): ComposerCoreIO = {
    if (using_custom == CustomCommandUsage.custom) {
      throw new Exception("Cannot use io after generating a custom io")
    }
    using_custom = CustomCommandUsage.default
    io_declaration
  }

  case class CCoreChannelMultiAccessBundle[T](dats: Seq[Seq[T]]) {
    def getCoreSlice(core: Int): Seq[T] = dats(core)

    def getChannelSlice(channel: Int): Seq[T] = dats.map(_(channel))
  }

  class CCoreChannelMultiAccessBundleChannelMajor[T](dat: Seq[Seq[T]]) extends CCoreChannelMultiAccessBundle(dat.transpose) {}

  object CCoreChannelMultiAccessBundleChannelMajor {
    def apply[T](dat: Seq[Seq[T]]): CCoreChannelMultiAccessBundleChannelMajor[T] =
      new CCoreChannelMultiAccessBundleChannelMajor(dat)
  }
}

class AcceleratorBlackBoxCore(outer: ComposerSystem,
                              blackboxBuilder: ModuleConstructor)(implicit p: Parameters)
  extends AcceleratorCore(outer) {


  val aio = blackboxBuilder match {
    case bbc: BlackboxBuilderCustom => ComposerIO(bbc.coreCommand, bbc.coreResponse)
    case _ => ComposerIO()
  }

  val readerParams = outer.systemParams.memoryChannelParams.filter(_.isInstanceOf[CReadChannelParams]).map(_.asInstanceOf[CReadChannelParams])
  val writerParams = outer.systemParams.memoryChannelParams.filter(_.isInstanceOf[CWriteChannelParams]).map(_.asInstanceOf[CWriteChannelParams])
  val spParams = outer.systemParams.memoryChannelParams.filter(_.isInstanceOf[CScratchpadParams]).map(_.asInstanceOf[CScratchpadParams])

  val rrio = readerParams.map(pr => getReaderModules(pr.name))
  val writerIOs = writerParams.map(pr => getWriterModules(pr.name))
  val spIOs = spParams.map(pr => getScratchpad(pr.name))

  class bb extends BlackBox {
    override val desiredName = outer.systemParams.name + "Wrapper"
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

  def getStructureAsPorts(a: Data, primaryDirection: Boolean, structureDepth: Int = 0, yieldSubfieldOnlyWithPrefix: Option[String] = None): Iterable[VerilogPort] = {
    def getRName(s: String): String = {
      yieldSubfieldOnlyWithPrefix match {
        case None => fix2Real(s)
        case Some(t) => t + "_" + s.split("\\.").takeRight(structureDepth).mkString("_")
      }
    }

    a match {
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

  def getRWChannelAsPorts[T <: Data](a: MixedVec[MixedVec[T]], ps: List[CChannelParams], prefix: String, primaryDirection: Boolean): Iterable[VerilogPort] = {
    // first dimension corresponds directly to Channel Param names
    a.zip(ps).flatMap { case (mv: MixedVec[T], param: CChannelParams) =>
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
    m.map { a =>
      val dim = a.dim.map { b =>
        if (b == 1) "" else s"[${b - 1}:0]"
      }.reverse
      val dir = if (a.dir == OUTPUT) "output" else "input"
      s"  $dir ${dim.head} ${a.name}${(if (dim.tail.isEmpty) "" else " ") + dim.tail.mkString("")}"
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
       |module ${outer.systemParams.name} (
       |  input clock,
       |  input reset,
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
       |${outer.systemParams.name} ${outer.systemParams.name}_inst (
       |  .clock(clock),
       |  .reset(reset),
       |$portInit
       |  );
       |
       |endmodule
       |
       |""".stripMargin

  // Link in Wrapper using ComposerBuild,
  // write source to file first
  val wrapperFName = os.Path(ComposerBuild.composerGenDir) / s"${outer.systemParams.name}_chiselLink.v"
  os.write.over(wrapperFName, bbWrapper)
  ComposerBuild.addSource(wrapperFName)

  val bbFName = os.Path(ComposerBuild.composerGenDir) / s"${outer.systemParams.name}.v"
  os.write.over(bbFName, userBB)
}

class ComposerSystemIO(implicit p: Parameters) extends Bundle {
  val cmd = Flipped(Decoupled(new AccelRoccCommand))
  val resp = Decoupled(new AccelRoccResponse())
}