package beethoven.Generation.CPP

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import beethoven.Generation.BuildMode.Simulation
import beethoven.Generation.CPP.CommandParsing.customCommandToCpp
import beethoven.Generation.CPP.TypeParsing.{enumToCpp, getCType}
import beethoven.Generation.BeethovenBuild
import beethoven.Generation.CppGeneration._
import beethoven.Parameters.AcceleratorSystems
import beethoven.Parameters.BeethovenParams.{CoreIDLengthKey, SystemIDLengthKey}
import beethoven.Platforms.{BuildModeKey, PlatformHasSeparateDMA}
import beethoven.Protocol.RoCC.Helpers.MCRFileMap
import beethoven.Systems._
import beethoven._
import beethoven.common.CLog2Up
import freechips.rocketchip.subsystem.FrontBusKey
import freechips.rocketchip.tile.XLen
import os.Path

import java.io.FileWriter


object Generation {
  def genCPPHeader(top: BeethovenTop)(implicit p: Parameters): Unit = {
    val acc = p(AcceleratorSystems)
    // we might have multiple address spaces...
    val path = Path(BeethovenBuild.beethovenGenDir)
    os.makeDir.all(path)

    val actualChannels = if (top.AXI_MEM.isDefined) platform.memoryNChannels else 0
    val (allocator, addrBits) = {
      (
        s"""
           |#ifdef SIM
           |#define NUM_DDR_CHANNELS ${actualChannels}
           |#endif
           |#define ALLOCATOR_SIZE_BYTES (0x${platform.physicalMemoryBytes.toHexString}L)
           |${if (!platform.hasDiscreteMemory) "#ifdef SIM" else ""}
           |""".stripMargin + "#define BEETHOVEN_USE_CUSTOM_ALLOC\n" +
          (if (!platform.hasDiscreteMemory) "#endif" else ""),
        s"const uint8_t beethovenNumAddrBits = ${log2Up(platform.extMem.master.size)};")
    }
    val defines = safe_join(user_defs map { deff => s"const ${deff.ty} ${deff.name} = ${deff.value};" })
    val enums = safe_join(user_enums map enumToCpp)
    val cpp_defs = safe_join(user_cpp_defs map { ppd => s"#define ${ppd.ty} ${ppd.value}" })
    val system_ids = safe_join(acc.filter(_.canReceiveSoftwareCommands) map { tup =>
      s"const uint8_t ${tup.name}_ID = ${acc.indexWhere(_.name == tup.name)};"
    })
    val hooks_dec_def = hook_defs map customCommandToCpp _
    val commandDeclarations = safe_join(hooks_dec_def.map(_._1))
    val commandDefinitions = safe_join(hooks_dec_def.map(_._2))
    val (idLengths, dma_def, mem_def) = {
      val addrSet = BeethovenTop.getAddressSet(0)

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
      def getSignedCIntType(width: Int): String = {
        if (width <= 8) "int8_t"
        else if (width <= 16) "int16_t"
        else if (width <= 32) "int32_t"
        else if (width <= 64) "int64_t"
        else throw new Exception("Type is too big")
      }
      def getUnsignedCIntType(width: Int): String = "u" + getSignedCIntType(width)
      val addrWid = log2Up(platform.extMem.master.size)
      val frontBusWidth = CLog2Up(platform.frontBusAddressMask | platform.frontBusBaseAddress)

      (
        s"""
           |static const uint64_t addrMask = 0x${addrSet.mask.toLong.toHexString};
           |""".stripMargin
        , if (platform.isInstanceOf[PlatformHasSeparateDMA] && p(BuildModeKey) != Simulation) "#define BEETHOVEN_HAS_DMA" else "", {
        if (platform.memoryNChannels > 0 && top.AXI_MEM.isDefined) {
          val strobeDtype = getVerilatorDtype(platform.extMem.master.beatBytes)

          val idDtype = getVerilatorDtype(top.AXI_MEM.get(0).in(0)._1.ar.bits.id.getWidth)
          f"""
             |#ifdef SIM
             |#ifdef VERILATOR_VERSION
             |#include <verilated.h>
             |using BeethovenFrontBusAddr_t = ${getUnsignedCIntType(frontBusWidth)};
             |using BeethovenMemIDDtype=$idDtype;
             |#endif
             |#define DEFAULT_PL_CLOCK ${platform.clockRateMHz}
             |${platform match {case pWithDMA: PlatformHasSeparateDMA => s"using BeethovenDMAIDtype=${getVerilatorDtype(pWithDMA.DMAIDBits)};"; case _ => ""}}
             |#define DATA_BUS_WIDTH ${platform.extMem.master.beatBytes * 8}
             |#endif
             |""".stripMargin
        } else {
          f"""
             |#ifdef SIM
             |#ifdef VERILATOR
             |#include <verilated.h>
             |using BeethovenFrontBusAddr_t = ${getUnsignedCIntType(frontBusWidth)};
             |#endif
             |#endif
             |
             |#define DATA_BUS_WIDTH ${platform.extMem.master.beatBytes * 8}
             |#define DEFAULT_PL_CLOCK ${platform.clockRateMHz}
             |
             |""".stripMargin
        }
      })
    }
    val mmio_addr = "const uint64_t BeethovenMMIOOffset = 0x" + platform.frontBusBaseAddress.toHexString + "LL;"

    val header = new FileWriter((path / "beethoven_allocator_declaration.h").toString())
    header.write(
      f"""
         |// Automatically generated header for Beethoven
         |
         |#include <beethoven/alloc.h>
         |#include <beethoven/rocc_cmd.h>
         |#include <cinttypes>
         |#ifndef BAREMETAL
         |#include <optional>
         |#include <cassert>
         |#endif
         |
         |#ifndef BEETHOVEN_ALLOCATOR_GEN
         |#define BEETHOVEN_ALLOCATOR_GEN
         |#define AXIL_BUS_WIDTH ${platform.frontBusBeatBytes * 8}
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
         |static const beethoven::beethoven_pack_info pack_cfg(system_id_bits, core_id_bits);
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

    val src = new FileWriter((path / "generated_beethoven_src.cc").toString())
    src.write(
      f"""
         |#include "beethoven_allocator_declaration.h"
         |
         |$commandDefinitions
         |""".stripMargin)
    src.close()
  }

}
