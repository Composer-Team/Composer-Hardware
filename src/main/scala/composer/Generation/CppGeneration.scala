package composer.Generation

import chipsalliance.rocketchip.config._
import chisel3.experimental._
import chisel3._
import chisel3.util.log2Up
import composer.ComposerParams.{CoreIDLengthKey, SystemIDLengthKey}
import composer.RoccHelpers.MCRFileMap
import composer.Systems.{ComposerAcc, ComposerTop}
import composer._
import composer.common.AbstractComposerCommand
import freechips.rocketchip.subsystem.ExtMem
import os.Path

import java.io.FileWriter
import scala.collection.SeqMap

object CppGeneration {

  private case class CppDefinition(ty: String, name: String, value: String)

  private case class PreprocessorDefinition(ty: String, value: String)

  private case class HookDef(sysName: String, cc: AbstractComposerCommand) {
    cc.elements.foreach { p =>
      val data = p._2
      val paramName = p._1
      assert(data.getWidth <= 64 || data.getWidth % 8 == 0,
        "Bit fields that are longer than 64b MUST be byte aligned (for our own implementation-simplicity-sake).\n" +
          " If this is an absolutely necessary functionality, contact developers.\n" +
          s"Variable in question: $paramName. Length: ${data.getWidth}\n")
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

  private[composer] def addUserCppFunctionDefinition(systemName: String, cc: AbstractComposerCommand): Unit = {
    val h = HookDef(systemName, cc)
    if (!hook_defs.exists(_.sysName == systemName)) {
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

  def enumToCpp(myEnum: ChiselEnum): String = {
    val enumClassName = myEnum.getClass.getSimpleName.split("\\$")(0)
    s"enum $enumClassName {\n" +
      myEnum.all.map { enumMember =>
        val a = enumMember.toString()
        val byparen = a.split("\\(")
        // before paren is just class name
        // after is $name=$value)
        val byeq = byparen(1).split("=")
        val value = byeq(0).toInt
        val name = byeq(1).substring(0, byeq(1).length - 1)
        s"\t$name = $value ,\n"
      }.reduce(_ + "\n" + _) + "\n};"
  }

  def customCommandToCpp(sysName: String, cc: AbstractComposerCommand): String = {
    val sub_signature = cc.realElements.map { pa =>
      val isSigned = pa._2.isInstanceOf[SInt]
      getCType(pa._1, pa._2.getWidth, isSigned) + " " + pa._1
    }
    val signature = if (sub_signature.isEmpty) "" else {
      sub_signature.reduce(_ + ", " + _)
    }

    val assignments = cc.fieldSubranges.flatMap { case (name: String, range: (Int, Int)) =>
      val high = range._1
      val low = range._2
      val width = 1 + high - low
      val payloadId = low / 64
      val payloadLocalOffset = low % 64
      val bitsLeftInPayload = 64 - payloadLocalOffset
      if (bitsLeftInPayload < width) {
        // we're going to roll over!
        Seq(
          f"\tpayloads[$payloadId] = payloads[$payloadId] | (($name & ${(1 << bitsLeftInPayload) - 1}) << $payloadLocalOffset);",
          f"\tpayloads[${payloadId + 1}] = payloads[${payloadId + 1}] | (($name >> $bitsLeftInPayload) & ${(1 << (width - bitsLeftInPayload)) - 1});"
        )
      } else {
        Seq(f"\tpayloads[$payloadId] = payloads[$payloadId] | ($name << $payloadLocalOffset);")
      }
    }
    val numCommands = cc.getNBeats()
    f"""
       |composer::rocc_response ${sysName}Command(uint16_t core_id, $signature) {
       |  assert(core_id < (1 << 10));
       |  uint64_t payloads[${numCommands * 2}];
       |  """.stripMargin + (if (assignments.length == 1) assignments(0) + "\n" else assignments.reduce(_ + "\n" + _)) +
      f"""
         |  for (int i = 0; i < ${numCommands}; ++i) {
         |    composer::rocc_cmd::start_cmd(${sysName}_ID, i == ${numCommands - 1}, 0, false, false, core_id, payloads[i*2], payloads[i*2+1]);
         |  }
         |}
         |""".stripMargin

  }

  def genCPPHeader(cr: MCRFileMap, acc: ComposerAcc)(implicit p: Parameters): Unit = {
    def safe_join(s: Seq[String]): String = if (s.isEmpty) "" else if (s.length == 1) s(0) else s.reduce(_ + "\n" + _)

    // we might have multiple address spaces...
    val path = Path(ComposerBuild.composerGenDir)
    os.makeDir.all(path)
    val mem = p(ExtMem).get

    val allocator =
      s"""
        |#ifdef SIM
        |#define NUM_DDR_CHANNELS ${mem.nMemoryChannels}
        |#endif
        |using composer_allocator=composer::device_allocator<${p(PlatformPhysicalMemoryBytes)}>;
        |${if (!p(HasDiscreteMemory)) "#ifdef SIM" else "" }
        |""".stripMargin + (if (p(HasDiscreteMemory)) "#define COMPOSER_USE_CUSTOM_ALLOC\n" else "") +
        (if (!p(HasDiscreteMemory)) "#ifdef SIM" else "")

    val addrBits = s"const uint8_t composerNumAddrBits = ${log2Up(mem.master.size)};"
    val defines = safe_join(user_defs map { deff => s"const ${deff.ty} ${deff.name} = ${deff.value};" })
    val enums = safe_join(user_enums map enumToCpp)
    val cpp_defs = safe_join(user_cpp_defs map { ppd => s"#define ${ppd.ty} ${ppd.value}\n" })
    val mmios = safe_join(cr.getCRdef)
    val system_ids = safe_join(acc.system_tups.filter(_._3.canReceiveSoftwareCommands) map { tup =>
      s"const uint8_t ${tup._3.name}_ID = ${tup._2};"
    })
    val customCommandHooks = safe_join(hook_defs map (a => customCommandToCpp(a.sysName, a.cc)))
    val maxCmdLength = 128
    val addrSet = ComposerTop.getAddressSet(0)
    val idLengths =
      s"""
         |static const uint8_t system_id_bits = $SystemIDLengthKey;
         |static const uint8_t core_id_bits = $CoreIDLengthKey;
         |static const composer::composer_pack_info pack_cfg(system_id_bits, core_id_bits);
         |static const uint64_t addrMask = ${addrSet.mask};
         |""".stripMargin

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

    val dma_def = if (p(HasDMA).isDefined) "#define COMPOSER_HAS_DMA" else ""
    val mem_def = p(ExtMem) match {
      case Some(a) =>
        val strobeDtype = getVerilatorDtype(p(ExtMem).get.master.beatBytes)
        val addrWid = log2Up(a.master.size)
        val addrDtype = getVerilatorDtype(addrWid)
        val idDtype = getVerilatorDtype(p(ExtMem).get.master.idBits)
        s"#ifdef SIM\n" +
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
          s"#endif"
      case None =>
        "// No memory detected, not defining Address Sim Dtype"
    }
    val mmio_addr = "const uint64_t ComposerMMIOOffset = " + p(MMIOBaseAddress) + "L;"

    val f = new FileWriter((path / "composer_allocator_declaration.h").toString())
    f.write(
      f"""
         |// Automatically generated header for Composer
         |
         |#include <composer/alloc.h>
         |#include <composer/rocc_cmd.h>
         |#include <cinttypes>
         |#ifndef COMPOSER_ALLOCATOR_GEN
         |#define COMPOSER_ALLOCATOR_GEN
         |#define AXIL_BUS_WIDTH ${p(AXILSlaveBeatBytes) * 8}
         |
         |// allocator declarations backends that do not have discrete memory or for simulator
         |$allocator
         |
         |// address bits used by FPGA
         |$addrBits
         |
         |// const defines declared by user cores
         |$defines
         |
         |// enums declared by user cores
         |$enums
         |
         |// C Preprocessor definitions declared by user cores
         |$cpp_defs
         |
         |// MMIO address + field offsets
         |$mmios
         |$mmio_addr
         |
         |// IDs to access systems directly through RoCC interface
         |$system_ids
         |
         |// Custom command interfaces
         |$customCommandHooks
         |
         |// misc
         |$idLengths
         |$dma_def
         |
         |// Verilator support
         |$mem_def
         |
         |#endif
         |//
    """.stripMargin)
    f.close()
  }
}