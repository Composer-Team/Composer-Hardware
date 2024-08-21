package beethoven.Generation.CPP

import chipsalliance.rocketchip.config.Parameters
import beethoven.common._
import chisel3._
import beethoven.Parameters.AcceleratorSystems
import beethoven.Generation.CPP.TypeParsing.getCType
import beethoven.Generation.CppGeneration.HookDef

import scala.annotation.tailrec

object CommandParsing {
  def customCommandToCpp(hookDef: HookDef)(implicit p: Parameters): (String, String) = {
    val HookDef(sysName, cc, resp, opCode) = hookDef
    val sub_signature = cc.realElements.sortBy(_._1).map { pa =>
      getCType(cc.elements(pa._1), pa._1) + " " + pa._1
    }
    val signature = if (sub_signature.isEmpty) "" else { ", " +
      sub_signature.reduce(_ + ", " + _)
    }

    def intToHexFlag(a: Long): String = {
      f"0x${a.toHexString}"
    }

    val numCommands = cc.getNBeats

    @tailrec
    def unrollPayload(basePayload: Int, bitsLeftInPayload: Int,
                      accessStrs: Seq[String], accessWidth: Seq[Int],
                      payloadLocalOffset: Int, seqAcc: Seq[(Int, String)] = Seq.empty): Seq[(Int, String)] = {
      if (accessStrs.isEmpty) seqAcc
      else {
        val aw = accessWidth.head
        val acc = accessStrs.head
        if (bitsLeftInPayload < aw) {
          val pl = Seq(
            (basePayload,
              f"((uint64_t)($acc & ${intToHexFlag((1L << bitsLeftInPayload) - 1)}LL) << $payloadLocalOffset)"),
            (basePayload + 1,
              f"(((uint64_t)$acc >> $bitsLeftInPayload) & ${intToHexFlag((1L << (aw - bitsLeftInPayload)) - 1)}LL)"))
          unrollPayload(basePayload + 1,
            64 - (aw - bitsLeftInPayload),
            accessStrs.tail,
            accessWidth.tail,
            aw - bitsLeftInPayload,
            seqAcc ++ pl)
        } else {
          val pl = (basePayload, f"((uint64_t)$acc << $payloadLocalOffset)")
          val is_even_split = aw == bitsLeftInPayload
          unrollPayload(if (is_even_split) basePayload + 1 else basePayload,
            if (is_even_split) 64 else bitsLeftInPayload - aw,
            accessStrs.tail,
            accessWidth.tail,
            if (is_even_split) 0 else aw - bitsLeftInPayload,
            seqAcc :+ pl
          )
        }
      }
    }

    val payloads = cc.fieldSubranges.flatMap { case (name: String, range: (Int, Int)) =>
      val high = range._1
      val low = range._2
      val width = 1 + high - low
      val payloadId = low / 64
      val plo = low % 64
      val blip = 64 - plo
      val access = cc.elements(name) match {
        case _: Address => f"$name.getFpgaAddr()"
        case _ => name
      }

      cc.elements(name) match {
        case v: Vec[_] =>
          unrollPayload(payloadId, blip,
            Seq.tabulate(v.length)(sidx => f"$access[$sidx]"),
            Seq.fill(v.length)(v.head.getWidth),
            plo)
        case _ =>
          unrollPayload(payloadId, blip, Seq(access), Seq(width), plo)
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

      ResponseParsing.getResponseDeclaration(resp, sysName) match {
        case None =>
          val declaration = f"""
             |namespace $sysName {
             |  void ${hookDef.cc.commandName}(uint16_t core_id$signature);
             |}
             |""".stripMargin
          val definition =
            f"""
               |void $sysName::${hookDef.cc.commandName}(uint16_t core_id$signature) {
               |#ifndef BAREMETAL
               |  assert(core_id < ${p(AcceleratorSystems).filter(_.name == sysName)(0).nCores});
               |#endif
               |  uint64_t payloads[${Math.max(numCommands * 2, 2)}];
               |""".stripMargin +(if (assignments.length == 1) assignments(0) + "\n" else assignments.fold("")(_ + "\n" + _)) +
              f"""
                 |  for (int i = 0; i < ${numCommands - 1}; ++i) {
                 |    beethoven::rocc_cmd::start_cmd(${sysName}_ID, false, 0, false, false, core_id, payloads[i*2+1], payloads[i*2], $opCode).send();
                 |  }
                 |  beethoven::rocc_cmd::start_cmd(${sysName}_ID, false, 0, false, false, core_id, payloads[${(numCommands - 1) * 2 + 1}], payloads[${(numCommands - 1) * 2}], $opCode).send();
                 |}""".stripMargin
          (declaration, definition)
        case Some(responseInfo) =>
          val typeConversion = if (responseInfo.name.equals("beethoven::rocc_response")) "" else f".to<${responseInfo.name}>()";

          def command_sig(is_dec: Boolean) = (if (is_dec) f"namespace $sysName {\n\t" else "") +
            f"beethoven::response_handle<${responseInfo.name}> " +
            f"${if (is_dec) "" else f"$sysName::"}${cc.commandName}(uint16_t core_id$signature)" +
            (if (is_dec) ";\n}" else "")


          val definition =
            f"""
               |${responseInfo.definition}
               |${command_sig(false)} {
               |#ifndef BAREMETAL
               |  assert(core_id < ${p(AcceleratorSystems).filter(_.name == sysName)(0).nCores});
               |#endif
               |  uint64_t payloads[${Math.max(numCommands * 2, 2)}];
               |""".stripMargin + (if (assignments.length == 1) assignments(0) + "\n" else assignments.fold("")(_ + "\n" + _)) +
              f"""
                 |  for (int i = 0; i < ${numCommands - 1}; ++i) {
                 |    beethoven::rocc_cmd::start_cmd(${sysName}_ID, false, 0, false, false, core_id, payloads[i*2+1], payloads[i*2], $opCode).send();
                 |  }
                 |  return beethoven::rocc_cmd::start_cmd(${sysName}_ID, true, 0, false, false, core_id, payloads[${(numCommands - 1) * 2 + 1}], payloads[${(numCommands - 1) * 2}], $opCode).send()$typeConversion;
                 |}""".stripMargin
          val declaration =
            f"""
               |${responseInfo.dec}
               |${command_sig(true)};""".stripMargin
          (declaration, definition)
      }
  }

  def bundlesAreEquivalentEnough(a: Bundle, b: Bundle): Boolean = {
    val aFields = a.elements
    val bFields = b.elements
    if (aFields.size != bFields.size) return false

    aFields foreach { case (key, value) =>
      bFields.get(key) match {
        case None => return false
        case Some(v2) => if (v2.getWidth != value.getWidth) return false
      }
    }
    true
  }
}
