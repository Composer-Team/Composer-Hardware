package composer.Generation.CPP

import chipsalliance.rocketchip.config.Parameters
import composer.common._
import chisel3._
import composer.AcceleratorSystems
import composer.Generation.CPP.TypeParsing.getCType

object BundleParsing {
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

    val responseInfo = ResponseParsing.getResponseDeclaration(resp, sysName)
    val typeConversion = if (responseInfo.name.equals("composer::rocc_response")) "" else f".to<${responseInfo.name}>()";

    def command_sig(is_dec: Boolean) = f"composer::response_handle<${responseInfo.name}> ${sysName}Command(uint16_t core_id, $signature)"


    val definition =
      f"""
         |${responseInfo.definition}
         |${command_sig(false)} {
         |  assert(core_id < ${p(AcceleratorSystems).filter(_.name == sysName)(0).nCores});
         |  uint64_t payloads[${numCommands * 2}];
         |""".stripMargin + (if (assignments.length == 1) assignments(0) + "\n" else assignments.reduce(_ + "\n" + _)) +
        f"""
           |  for (int i = 0; i < ${numCommands - 1}; ++i) {
           |    composer::rocc_cmd::start_cmd(${sysName}_ID, false, 0, false, false, core_id, payloads[i*2+1], payloads[i*2]).send();
           |  }
           |  return composer::rocc_cmd::start_cmd(${sysName}_ID, true, 0, false, false, core_id, payloads[${(numCommands - 1) * 2 + 1}], payloads[${(numCommands - 1) * 2}]).send()$typeConversion;
           |}
           |""".stripMargin
    val declaration =
      f"""
         |${responseInfo.dec}
         |${command_sig(true)};
         |""".stripMargin
    (declaration, definition)
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
