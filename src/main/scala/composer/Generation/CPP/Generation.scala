package composer.Generation.CPP

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import composer.ComposerParams._
import composer.Generation.CPP.BundleParsing.customCommandToCpp
import composer.Generation.CPP.TypeParsing.enumToCpp
import composer.Generation.ComposerBuild
import composer.Generation.CppGeneration._
import composer.PlatformPhysicalMemoryBytes
import composer.Platforms._
import composer.RoccHelpers.MCRFileMap
import composer.Systems._
import freechips.rocketchip.subsystem.ExtMem
import freechips.rocketchip.tile.XLen
import os.Path

import java.io.FileWriter


object Generation {
  def genCPPHeader(cr: MCRFileMap, top: ComposerTop)(implicit p: Parameters): Unit = {
    val acc = top.accelerator_system.acc
    // we might have multiple address spaces...
    val path = Path(ComposerBuild.composerGenDir)
    os.makeDir.all(path)

    val (allocator, addrBits) = p(ExtMem) match {
      case Some(mem) =>
        (
          s"""
             |#ifdef SIM
             |#define NUM_DDR_CHANNELS ${mem.nMemoryChannels}
             |#endif
             |#define ALLOCATOR_SIZE_BYTES (0x${p(PlatformPhysicalMemoryBytes).toHexString}L)
             |${if (!p(HasDiscreteMemory)) "#ifdef SIM" else ""}
             |""".stripMargin + "#define COMPOSER_USE_CUSTOM_ALLOC\n" +
            (if (!p(HasDiscreteMemory)) "#endif" else ""),
          s"const uint8_t composerNumAddrBits = ${log2Up(mem.master.size)};")
      case None =>
        (
          s"""
             |#ifdef SIM
             |#define NUM_DDR_CHANNELS 0
             |#endif
             |#define ALLOCATOR_SIZE_BYTES (0x${p(PlatformPhysicalMemoryBytes).toHexString}L)
             |""".stripMargin,
          s"const uint8_t composerNumAddrBits = 0;")
    }
    val defines = safe_join(user_defs map { deff => s"const ${deff.ty} ${deff.name} = ${deff.value};" })
    val enums = safe_join(user_enums map enumToCpp)
    val cpp_defs = safe_join(user_cpp_defs map { ppd => s"#define ${ppd.ty} ${ppd.value}" })
    val mmios = safe_join(cr.getCRdef)
    val system_ids = safe_join(acc.system_tups.filter(_._3.canReceiveSoftwareCommands) map { tup =>
      s"const uint8_t ${tup._3.name}_ID = ${tup._2};"
    })
    val hooks_dec_def = hook_defs map customCommandToCpp _
    val commandDeclarations = safe_join(hooks_dec_def.map(_._1))
    val commandDefinitions = safe_join(hooks_dec_def.map(_._2))
    val (idLengths, dma_def, mem_def) = p(ExtMem) match {
      case Some(_) => {
        val addrSet = ComposerTop.getAddressSet(0)

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

        (
          s"""
             |static const uint64_t addrMask = 0x${addrSet.mask.toLong.toHexString};
             |""".stripMargin
          , if (p(HasDMA).isDefined) "#define COMPOSER_HAS_DMA" else "",
          p(ExtMem) match {
            case Some(a) =>
              val strobeDtype = getVerilatorDtype(p(ExtMem).get.master.beatBytes)
              val addrWid = log2Up(a.master.size)
              val addrDtype = getVerilatorDtype(addrWid)
              val idDtype = getVerilatorDtype(top.AXI_MEM.get(0).in(0)._1.ar.bits.id.getWidth)
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
        )
      }
      case None => ("", "", "")
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
         |#include <optional>
         |#include <cassert>
         |
         |#ifndef COMPOSER_ALLOCATOR_GEN
         |#define COMPOSER_ALLOCATOR_GEN
         |#define AXIL_BUS_WIDTH ${p(FrontBusBeatBytes) * 8}
         |#define XLEN ${p(XLen)}
         |// allocator declarations backends that do not have discrete memory or for simulator
         |$allocator
         |// address bits used by FPGA
         |$addrBits
         |// const defines declared by user cores
         |$defines
         |// enums declared by user cores
         |$enums
         |// C Preprocessor definitions declared by user cores
         |$cpp_defs
         |// MMIO address + field offsets
         |static const uint8_t system_id_bits = $SystemIDLengthKey;
         |static const uint8_t core_id_bits = $CoreIDLengthKey;
         |static const composer::composer_pack_info pack_cfg(system_id_bits, core_id_bits);
         |$mmios
         |$mmio_addr
         |// IDs to access systems directly through RoCC interface
         |$system_ids
         |// Custom command interfaces
         |$commandDeclarations
         |// misc
         |$idLengths
         |$dma_def
         |// Verilator support
         |$mem_def
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
