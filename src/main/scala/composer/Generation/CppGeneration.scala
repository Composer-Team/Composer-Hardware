package composer.Generation

import chipsalliance.rocketchip.config._
import chisel3.experimental._
import chisel3.util.log2Up
import composer.ComposerParams.{CoreIDLengthKey, SystemIDLengthKey}
import composer.RoccHelpers.MCRFileMap
import composer.Systems.{ComposerAcc, ComposerTop}
import composer._
import freechips.rocketchip.subsystem.ExtMem
import os.Path

import java.io.FileWriter

object CppGeneration {

  private case class CppDefinition(ty: String, name: String, value: String)

  private case class PreprocessorDefinition(ty: String, value: String)

  private case class HookDef(sysName: String, params: Seq[(String, Int, Boolean)]) {
    params.foreach{ p =>
      val paramLength = p._2
      val paramName = p._1
      assert(paramLength <= 64 || paramLength % 8 == 0,
      "Bit fields that are longer than 64b MUST be byte aligned (for our own implementation-simplicity-sake).\n" +
        " If this is an absolutely necessary functionality, contact developers.\n" +
        s"Variable in question: $paramName. Length: $paramLength\n")
    }
  }

  private var user_enums: List[EnumFactory] = List()
  private var user_defs: List[CppDefinition] = List()
  private var user_cpp_defs: List[PreprocessorDefinition] = List()
  private var hook_defs: List[HookDef] = List()

  def addUserCppDefinition[t](ty: String, name: String, value: t): Unit = {
    val ty_f = ty.trim
    val name_f = name.trim
    val existingDefs = user_defs.filter(_.name == name_f)
    existingDefs.foreach(a => require(a.ty == ty_f && a.value == value.toString, s"Redefining ${a.name} from (${a.ty}, ${a.value}) to ($ty, $value)"))
    if (existingDefs.isEmpty) user_defs = CppDefinition(ty_f, name_f, value.toString) :: user_defs
  }

  private[composer] def addUserCppFunctionDefinition(systemName: String, params: Seq[(String, Int, Boolean)]): Unit = {
    val h = HookDef(systemName, params)
    if (hook_defs.exists(_.sysName == systemName)) {
      val params_are_same = hook_defs.filter(_.sysName == systemName).map(_.params.equals(params)).reduce(_ && _)
      assert(params_are_same, "You are trying to expose a hook to a Composer System that appears to differ across cores!")
    } else {
      hook_defs = h :: hook_defs
    }
  }

  private def getCType(name: String, width: Int, unsigned: Boolean): String = {
    val isFloatingPoint = name.contains("FP")
    if (isFloatingPoint) {
      width match {
        case 32 => "float"
        case 64 => "double"
        case _ => throw new Exception(s"Trying to export a float type but has width $width. Expecting either 32 or 64")
      }
    } else {
      val prefix = if (unsigned) "u" else ""
      width match {
        case x if x <= 8 => f"${prefix}int8_t"
        case x if x <= 16 => f"${prefix}int16_t"
        case x if x <= 32 => f"${prefix}int32_t"
        case x if x <= 64 => f"${prefix}int64_t"
        case _ => "void*"
      }
    }
  }


  def addPreprocessorDefinition(name: String, value: String = ""): Unit = {
    val ppd = PreprocessorDefinition(name, value)
    if (!user_cpp_defs.contains(ppd)) {
      user_cpp_defs = ppd :: user_cpp_defs
    }
  }

  def addUserCppDefinition[t](elems: Seq[(String, String, t)]): Unit = {
    elems.foreach(a => addUserCppDefinition(a._1, a._2, a._3))
  }

  def exportChiselEnum(enum: ChiselEnum): Unit = {
    if (!user_enums.contains(enum))
      user_enums = enum :: user_enums
  }

  def genCPPHeader(cr: MCRFileMap, acc: ComposerAcc)(implicit p: Parameters): Unit = {
    // we might have multiple address spaces...
    val path = Path(ComposerBuild.composerGenDir)
    os.makeDir.all(path)
    val f = new FileWriter((path / "composer_allocator_declaration.h").toString())
    val mem = p(ExtMem).get
    f.write("// Automatically generated memory-allocator declaration from Composer-Hardware:CppGeneration\n" +
      "#include <composer/alloc.h>\n" +
      "#include <composer/rocc_cmd.h>\n" +
      "#include <cinttypes>\n" +
      "#ifndef COMPOSER_ALLOCATOR_GEN\n" +
      "#define COMPOSER_ALLOCATOR_GEN\n" +
      s"#define AXIL_BUS_WIDTH ${p(AXILSlaveBeatBytes) * 8}\n")
    if (!p(HasDiscreteMemory)) f.write("#ifdef SIM\n")
    f.write("#define COMPOSER_USE_CUSTOM_ALLOC\n" +
      "#define NUM_DDR_CHANNELS " + mem.nMemoryChannels + "\n" +
      "using composer_allocator=composer::device_allocator<" + p(PlatformPhysicalMemoryBytes) + ">;\n")
    if (!p(HasDiscreteMemory)) f.write("#endif\n")

    f.write(s"const uint8_t composerNumAddrBits = ${log2Up(mem.master.size)};\n")

    user_defs foreach { deff =>
      f.write(s"const ${deff.ty} ${deff.name} = ${deff.value};\n")
    }

    user_enums foreach { myEnum =>
      val enumClassName = myEnum.getClass.getSimpleName.split("\\$")(0)
      f.write(s"enum $enumClassName {\n")
      myEnum.all.foreach { enumMember =>
        val a = enumMember.toString()
        val byparen = a.split("\\(")
        // before paren is just class name
        // after is $name=$value)
        val byeq = byparen(1).split("=")
        val value = byeq(0).toInt
        val name = byeq(1).substring(0, byeq(1).length - 1)
        f.write(s"\t$name = $value ,\n")
      }
      f.write("};\n")
    }

    user_cpp_defs foreach { ppd =>
      f.write(s"#define ${ppd.ty} ${ppd.value}\n")
    }

    cr.printCRs(Some(f))

    acc.system_tups.filter(_._3.canReceiveSoftwareCommands) foreach { tup =>
      val sys_id = tup._2
      val sys_name = tup._3.name
      // write out system id so that users can reference it directly
      f.write(s"const uint8_t ${sys_name}_ID = $sys_id;\n")
    }
    // STILL WITHIN COMPOSER NAMESPACE
    val maxCmdLength = 128
    hook_defs.foreach { hook =>
      val sub_signature = hook.params.map { pa =>
        getCType(pa._1, pa._2, pa._3) + " " + pa._1
      }
      val signature = if (sub_signature.isEmpty) "" else {
        sub_signature.reduce(_ + ", " + _)
      }

      val assignments = hook.params.map(_._2).scan(0)(_ + _) zip hook.params flatMap { case (offset: Int, p: (String, Int, Boolean)) =>
        val payloadId = offset / 64
        val payloadLocalOffset = offset % 64
        val bitsLeftInPayload = 64 - payloadLocalOffset
        if (bitsLeftInPayload < p._2) {
          // we're going to roll over!
          Seq(
            f"\tpayloads[$payloadId] = payloads[$payloadId] | ((${p._1} & ${(1 << bitsLeftInPayload)-1}) << $payloadLocalOffset));",
            f"\tpayloads[${payloadId+1}] = payloads[${payloadId+1}] | ((${p._1} >> $bitsLeftInPayload) & ${(1 << (p._2 - bitsLeftInPayload))-1});"
          )
        } else {
          Seq(f"\tpayloads[$payloadId] = payloads[$payloadId] | (${p._1} << $payloadLocalOffset);")
        }
      }
      val numCommands = (hook.params.map(_._2).sum.toFloat / maxCmdLength).ceil.toInt
      f.write(
        f"""
           |rocc_response ${hook.sysName}Command(uint16_t core_id, $signature) {
           |  assert(core_id < (1 << 10));
           |  uint64_t payloads[${numCommands*2}];
           |  """.stripMargin + (if (assignments.length == 1) assignments(0) + "\n" else assignments.reduce(_ + "\n" + _) )+
        f"""
           |  for (int i = 0; i < ${numCommands}; ++i) {
           |    rocc_cmd::start_cmd(${hook.sysName}_ID, i == ${numCommands - 1}, 0, false, false, core_id, payloads[i*2], payloads[i*2+1]);
           |  }
           |}
           |""".stripMargin)
    }


    f.write(s"static const uint8_t system_id_bits = $SystemIDLengthKey;\n")
      f.write(s"static const uint8_t core_id_bits = $CoreIDLengthKey;\n")
      //    f.write(//s"static const uint8_t numChannelSelectionBits = ${p(ChannelSelectionBitsKey)}," +
      //      s"static const uint8_t channelTransactionLenBits = ${log2Up(p(MaxChannelTransactionLenKey))};\n")
      f.write(s"static const composer::composer_pack_info pack_cfg(system_id_bits, core_id_bits);\n")
      val addrSet = ComposerTop.getAddressSet(0)
      f.write(s"static const uint64_t addrMask = ${addrSet.mask};\n")

      // this next stuff is just for simulation
      def getVerilatorDtype(width: Int): String = {
        width match {
          case x if x <= 8 => "CData"
          case x if x <= 16 => "SData"
          case x if x <= 32 => "IData"
          case x if x <= 64 => "QData"
          case _ => "ERROR"
        }
      }

      if (p(HasDMA).isDefined) {
        f.write("#define COMPOSER_HAS_DMA\n")
      }

      p(ExtMem) match {
        case Some(a) =>
          val strobeDtype = getVerilatorDtype(p(ExtMem).get.master.beatBytes)
          val addrWid = log2Up(a.master.size)
          val addrDtype = getVerilatorDtype(addrWid)
          val idDtype = getVerilatorDtype(p(ExtMem).get.master.idBits)
          f.write(s"#ifdef SIM\n" +
            s"#include <verilated.h>\n" +
            s"using ComposerMemAddressSimDtype=$addrDtype;\n" +
            s"using ComposerStrobeSimDtype=$strobeDtype;\n" +
            s"using ComposerMemIDDtype=$idDtype;\n" +
            s"#define DEFAULT_PL_CLOCK ${p(DefaultClockRateKey)}\n" +
            (p(HasDMA) match {
              case None => ""
              case Some(a) => s"using ComposerDMAIDtype=${getVerilatorDtype(a)};\n"
            }) +
            s"#define DATA_BUS_WIDTH ${p(ExtMem).get.master.beatBytes * 8}\n" +
            s"#endif\n")
        case None =>
          f.write("// No memory detected, not defining Address Sim Dtype\n")
      }
      f.write("const uint64_t ComposerMMIOOffset = " + p(MMIOBaseAddress) + "L;\n")


      f.write("#endif\n")
      f.close()
    }
  }
