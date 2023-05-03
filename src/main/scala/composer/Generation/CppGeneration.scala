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

  private var user_enums: List[EnumFactory] = List()
  private var user_defs: List[CppDefinition] = List()
  private var user_cpp_defs: List[PreprocessorDefinition] = List()

  def addUserCppDefinition[t](ty: String, name: String, value: t): Unit = {
    val ty_f = ty.trim
    val name_f = name.trim
    val existingDefs = user_defs.filter(_.name == name_f)
    existingDefs.foreach(a => require(a.ty == ty_f && a.value == value.toString, s"Redefining ${a.name} from (${a.ty}, ${a.value}) to ($ty, $value)"))
    if (existingDefs.isEmpty) user_defs = CppDefinition(ty_f, name_f, value.toString) :: user_defs
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
    val path = Path(System.getenv("COMPOSER_ROOT")) / "Composer-Hardware" / "vsim" / "generated-src"
    os.makeDir.all(path)
    val f = new FileWriter((path / "composer_allocator_declaration.h").toString())
    val mem = p(ExtMem).get
    f.write("// Automatically generated memory-allocator declaration from Composer-Hardware:CppGeneration\n" +
      "#include <composer/alloc.h>\n" +
      "#include <composer/rocc_cmd.h>\n" +
      "#include <cinttypes>\n" +
      "#ifndef COMPOSER_ALLOCATOR_GEN\n" +
      "#define COMPOSER_ALLOCATOR_GEN\n" +
      s"#define AXIL_BUS_WIDTH ${p(AXILSlaveBeatBytes)*8}\n")
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

    val num_systems = acc.systems.length
    val max_core_per_system = acc.systems.map(_.nCores).max
    val max_channels_per_channelGroup = {
      val g = acc.systems.flatMap(_.module.core_io_mappings.flatMap(_.map(_.channel_subidx)))
      if (g.isEmpty) -1
      else g.max + 1
    }

    acc.system_tups.filter(_._3.canReceiveSoftwareCommands) foreach { tup =>
      val sys_id = tup._2
      val sys_name = tup._3.name
      // write out system id so that users can reference it directly
      f.write(s"const uint8_t ${sys_name}_ID = $sys_id;\n")
    }
    f.write("// index by... sys_id, core_id, name_id, id_within_channel\n")
    if (max_channels_per_channelGroup > 0) {
      // first gotta make channel_names in C++ header
      val allChannelNames = acc.systems.flatMap(_.module.core_io_mappings.flatten.map(_.channel_name))
      f.write("enum ComposerChannels {\n")
      allChannelNames.zipWithIndex.foreach { a =>
        f.write(s"\t${a._1} = ${a._2},\n")
      }
      f.write("};\n")
      val nNames = allChannelNames.length

      val name_invalid_list = "{ " + Seq.fill(max_channels_per_channelGroup - 1)("(char)0xFF, ").foldLeft("")(_ + _) + " (char)0xFF }"
      val core_invalid_list = "{ " + Seq.fill(nNames - 1)(name_invalid_list + ", ").foldLeft("")(_ + _) + name_invalid_list + " }"
      f.write(s"static const char __composer_channel_map[$num_systems][$max_core_per_system][$nNames][$max_channels_per_channelGroup] = {")
      (0 until num_systems) foreach { sys_id =>
        val sys = acc.system_tups.filter(_._2 == sys_id)(0)._1.module
        f.write("{ ")
        (0 until max_core_per_system) foreach { core_id =>
          if (core_id >= sys.cores.length) {
            f.write(core_invalid_list)
          } else {
            f.write("{ ")
            val core_ios = sys.core_io_mappings(core_id)
            allChannelNames.zipWithIndex foreach { case (name, name_idx) =>
              val members = core_ios.filter(_.channel_name == name)
              if (members.isEmpty)
              // this core doesn't have this group
                f.write(name_invalid_list)
              else {
                f.write("{ ")
                (0 until max_channels_per_channelGroup) foreach { ch_sidx =>
                  val hit = members.filter(_.channel_subidx == ch_sidx)
                  if (hit.isEmpty) {
                    // this channel does not use software addressing
                    f.write("(char)0xFF")
                  } else {
                    f.write(hit(0).io_idx.toString)
                  }
                  if (ch_sidx < max_channels_per_channelGroup - 1) f.write(", ")
                }
                f.write("}")
              }
              if (name_idx < nNames - 1) f.write(", ")
            }
            f.write("}")
          }
          if (core_id < max_core_per_system - 1) f.write(", ")
        }
        f.write("}")
        if (sys_id < num_systems - 1) f.write(", ")
      }
      f.write("};\n")
    } else f.write(s"static const char __composer_channel_map[1][1][1][1] = {{{{(char)0xFF}}}};\nenum ComposerChannels {};\n")
    f.write(
      """
        |namespace composer {
        |  static ChannelAddressInfo getChannelSubIdx(uint16_t system_id, uint8_t core_id, ComposerChannels name, int id) {
        |    return ChannelAddressInfo(system_id, core_id, __composer_channel_map[system_id][core_id][uint8_t(name)][id]);
        |  }
        |}
        |
        |""".stripMargin)


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
