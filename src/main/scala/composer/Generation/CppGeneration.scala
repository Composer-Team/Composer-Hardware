package composer.Generation

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util.log2Up
import composer.ComposerParams.{CoreIDLengthKey, SystemIDLengthKey}
import composer.RoccHelpers.MCRFileMap
import composer.Systems.{ComposerAcc, ComposerTop}
import composer._
import composer.common.{AbstractAccelCommand, AccelResponse}
import composer.Platforms.{DefaultClockRateKey, FrontBusBaseAddress, FrontBusBeatBytes, HasDiscreteMemory, HasDMA}
import freechips.rocketchip.subsystem.ExtMem
import freechips.rocketchip.tile.XLen
import os.Path

import java.io.FileWriter
import scala.collection.SeqMap

object CppGeneration {

  private case class CppDefinition(ty: String, name: String, value: String)

  private case class PreprocessorDefinition(ty: String, value: String)

  private case class HookDef(sysName: String, cc: AbstractAccelCommand, resp: AccelResponse) {
    cc.elements.foreach { p =>
      val data = p._2
      val paramName = p._1
      assert(data.getWidth <= 64 || data.getWidth % 8 == 0,
        "Bit fields that are longer than 64b MUST be byte aligned (for our own implementation-simplicity-sake).\n" +
          " If this is an absolutely necessary functionality, contact developers.\n" +
          s"Variable in question: $paramName. Length: ${data.getWidth}\n")
    }
  }

  private var user_enums: List[chisel3.ChiselEnum] = List()
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

  private[composer] def addUserCppFunctionDefinition(systemName: String, cc: AbstractAccelCommand, resp: AccelResponse): Unit = {
    val h = HookDef(systemName, cc, resp)
    if (!hook_defs.exists(_.sysName == systemName)) {
      hook_defs = h :: hook_defs
    }
  }

  private def getCType(name: String, width: Int, unsigned: Boolean): String = {
    val isFloatingPoint = name.contains("FP")
    val isFpgaAddress = name.endsWith("_ADDR")
    if (isFloatingPoint) {
      width match {
        case 32 => "float"
        case 64 => "double"
        case _ => throw new Exception(s"Trying to export a float type but has width $width. Expecting either 32 or 64")
      }
    } else if (isFpgaAddress) {
      "composer::remote_ptr"
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

  def addPreprocessorDefinition(elems: Seq[(String, String)]): Unit = {
    elems.foreach(a => addPreprocessorDefinition(a._1, a._2))
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

  //noinspection ScalaUnusedSymbol
  def exportChiselEnum(enum: chisel3.ChiselEnum): Unit = {
    if (!user_enums.contains(enum))
      user_enums = enum :: user_enums
  }


  def enumToCpp(myEnum: chisel3.ChiselEnum): String = {
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

  private[composer] def safe_join(s: Seq[String], sep: String = "\n"): String = if (s.isEmpty) "" else if (s.length == 1) s(0) else s.reduce(_ + sep + _)

  def customCommandToCpp(sysName: String, cc: AbstractAccelCommand, resp: AccelResponse)(implicit p: Parameters): (String, String) = {
    val sub_signature = cc.realElements.sortBy(_._1).map { pa =>
      val isSigned = pa._2.isInstanceOf[SInt]
      getCType(pa._1, pa._2.getWidth, !isSigned) + " " + pa._1
    }
    val signature = if (sub_signature.isEmpty) "" else {
      sub_signature.reduce(_ + ", " + _)
    }

    def intToHexFlag(a: Long): String = {
      f"0x${a.toHexString}"
    }

    val numCommands = cc.getNBeats
    val payloads = cc.fieldSubranges.flatMap { case (name: String, range: (Int, Int)) =>
      val high = range._1
      val low = range._2
      val width = 1 + high - low
      val payloadId = low / 64
      val payloadLocalOffset = low % 64
      val bitsLeftInPayload = 64 - payloadLocalOffset
      val access = if (name.endsWith("_ADDR")) f"$name.getFpgaAddr()" else name
      if (bitsLeftInPayload < width) {
        // we're going to roll over!
        Seq((payloadId, f"((uint64_t)($access & ${intToHexFlag((1L << bitsLeftInPayload) - 1)}L) << $payloadLocalOffset)"),
          (payloadId + 1, f"(((uint64_t)$access >> $bitsLeftInPayload) & ${intToHexFlag((1L << (width - bitsLeftInPayload)) - 1)}L)")
        )
      } else {
        Seq((payloadId, f"((uint64_t)$access << $payloadLocalOffset)"))
      }
    }
    val assignments = (0 until (numCommands * 2)) map { payloadIdx =>
      val pl = {
        val sj = safe_join(payloads.filter(_._1 == payloadIdx).map(_._2), " | ")
        if (sj == "") "0"
        else sj
      }
      f"  payloads[$payloadIdx] = $pl;";
    }

    val structName = f"${sysName}Response_t"
    val structMembersWithType = resp.realElements.map(a => f"${getCType(a._1, a._2.getWidth, a._2.isInstanceOf[SInt])} ${a._1}")
    val response_struct = f"struct $structName {\n // struct members \n" +
      safe_join(structMembersWithType.map(_ + ";")) + "\n" +
      "// constructor\n" +
      s"$structName(${safe_join(structMembersWithType, ", ")}) : ${safe_join(resp.realElements.map(a => f"${a._1}(${a._1})"), ", ")} {}\n}"

    val template_sig = f"template<> $structName composer::response_handle<$structName>::get()"
    def command_sig(is_dec: Boolean) = f"composer::response_handle<$structName> ${sysName}Command(uint16_t core_id, $signature," +
      f" const std::vector<composer::remote_ptr>& memory_operands${if (is_dec) " = {}" else ""})"
    val template_def = resp.fieldSubranges.map { ele =>
      if (ele._1.endsWith("_FP")) {
        (ele._1, f"__${ele._1}")
      } else {
        //noinspection DuplicatedCode
        val shiftAmt = 1 + ele._2._1 - ele._2._2
        val mask = if (shiftAmt < 64) (1L << shiftAmt) - 1 else -1L
        (ele._1, f"(resp & (0x${mask.toHexString}L << ${ele._2._2})) >> ${ele._2._2}")
      }
    }.sortBy(_._1).map(_._2).reduce(_ + ", " + _)
    val template_decs = resp.fieldSubranges.filter(_._1.endsWith("_FP")).map { ele =>
      //noinspection DuplicatedCode
      val shiftAmt = 1 + ele._2._1 - ele._2._2
      val mask = if (shiftAmt < 64) (1L << shiftAmt) - 1 else -1L
      val (ty_name, ty_name_int) = if (shiftAmt == 32) {
        ("float", "uint32_t")
      } else {
        ("double", "uint64_t")
      }
      f"  $ty_name_int __${ele._1}_asInt = (resp & 0x${mask.toHexString}L) >> ${ele._2._2};\n" +
        f"  $ty_name __${ele._1} = reinterpret_cast<$ty_name&>(__${ele._1}_asInt);\n"
    } match {
      case a: Seq[String] if a.isEmpty => ""
      case a: Seq[String] => a.reduce(_ + " " + _)
    }

    val definition =
      f"""
         |
         |$template_sig {
         |  auto r = rg.get();
         |  auto resp = r.data;
         |
         |$template_decs
         |
         |  return $structName($template_def);
         |}
         |
         |${command_sig(false)} {
         |  assert(core_id < ${p(AcceleratorSystems).filter(_.name == sysName)(0).nCores});
         |  uint64_t payloads[${numCommands * 2}];
         |""".stripMargin + (if (assignments.length == 1) assignments(0) + "\n" else assignments.reduce(_ + "\n" + _)) +
        f"""
           |  for (int i = 0; i < ${numCommands - 1}; ++i) {
           |    composer::rocc_cmd::start_cmd(${sysName}_ID, false, 0, false, false, core_id, payloads[i*2+1], payloads[i*2]).send();
           |  }
           |  return composer::rocc_cmd::start_cmd(${sysName}_ID, true, 0, false, false, core_id, payloads[${(numCommands - 1) *2 + 1}], payloads[${(numCommands - 1)*2}]).send(memory_operands).to<$structName>();
           |}
           |""".stripMargin
    val declaration =
      f"""
         |$response_struct;
         |$template_sig;
         |${command_sig(true)};
         |""".stripMargin
    (declaration, definition)
  }

  def genCPPHeader(cr: MCRFileMap, acc: ComposerAcc)(implicit p: Parameters): Unit = {

    // we might have multiple address spaces...
    val path = Path(ComposerBuild.composerGenDir)
    os.makeDir.all(path)
    val mem = p(ExtMem).get

    val allocator =
      s"""
         |#ifdef SIM
         |#define NUM_DDR_CHANNELS ${mem.nMemoryChannels}
         |#endif
         |#define ALLOCATOR_SIZE_BYTES (0x${p(PlatformPhysicalMemoryBytes).toHexString}L)
         |${if (!p(HasDiscreteMemory)) "#ifdef SIM" else ""}
         |""".stripMargin + "#define COMPOSER_USE_CUSTOM_ALLOC\n" +
        (if (!p(HasDiscreteMemory)) "#endif" else "")

    val addrBits = s"const uint8_t composerNumAddrBits = ${log2Up(mem.master.size)};"
    val defines = safe_join(user_defs map { deff => s"const ${deff.ty} ${deff.name} = ${deff.value};" })
    val enums = safe_join(user_enums map enumToCpp)
    val cpp_defs = safe_join(user_cpp_defs map { ppd => s"#define ${ppd.ty} ${ppd.value}\n" })
    val mmios = safe_join(cr.getCRdef)
    val system_ids = safe_join(acc.system_tups.filter(_._3.canReceiveSoftwareCommands) map { tup =>
      s"const uint8_t ${tup._3.name}_ID = ${tup._2};"
    })
    val hooks_dec_def = hook_defs map (a => customCommandToCpp(a.sysName, a.cc, a.resp))
    val commandDeclarations = safe_join(hooks_dec_def.map(_._1))
    val commandDefinitions = safe_join(hooks_dec_def.map(_._2))
    val addrSet = ComposerTop.getAddressSet(0)
    val idLengths =
      s"""
         |static const uint8_t system_id_bits = $SystemIDLengthKey;
         |static const uint8_t core_id_bits = $CoreIDLengthKey;
         |static const composer::composer_pack_info pack_cfg(system_id_bits, core_id_bits);
         |static const uint64_t addrMask = 0x${addrSet.mask.toLong.toHexString};
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
    val mmio_addr = "const uint64_t ComposerMMIOOffset = 0x" + p(FrontBusBaseAddress).toHexString + "L;"

    val header = new FileWriter((path / "composer_allocator_declaration.h").toString())
    header.write(
      f"""
         |// Automatically generated header for Composer
         |
         |#include <composer/alloc.h>
         |#include <composer/rocc_cmd.h>
         |#include <cinttypes>
         |#include <cassert>
         |
         |#ifndef COMPOSER_ALLOCATOR_GEN
         |#define COMPOSER_ALLOCATOR_GEN
         |#define AXIL_BUS_WIDTH ${p(FrontBusBeatBytes) * 8}
         |#define XLEN ${p(XLen)}
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
         |$commandDeclarations
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
    header.close()

    val src = new FileWriter((path / "generated_composer_src.cc").toString())
    src.write(
      f"""
         |#include "composer_allocator_declaration.h"
         |
         |$commandDefinitions
         |""".stripMargin)
    src.close()
  }
}
