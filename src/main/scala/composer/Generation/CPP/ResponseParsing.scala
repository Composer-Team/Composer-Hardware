package composer.Generation.CPP

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import composer.Generation.CPP.CommandParsing.bundlesAreEquivalentEnough
import composer.Generation.CPP.TypeParsing.getCType
import composer.common.{AccelResponse, AccelRoccUserResponse}

object ResponseParsing {
  private[composer] case class ComposerResponseDeclarations(name: String, dec: String, definition: String)

  def getResponseDeclaration(resp: AccelResponse, sysName: String)(implicit p: Parameters): ComposerResponseDeclarations = {
    if (bundlesAreEquivalentEnough(resp, new AccelRoccUserResponse()))
      return ComposerResponseDeclarations("composer::rocc_response", "", "")

    // otherwise we have a custom type
    val structName = f"$sysName::${resp.responseName}"
    val structMembersWithType = resp.realElements.map(a => f"${getCType(resp.elements(a._1), a._1)} ${a._1}")
    val response_struct =
      f"""
         |namespace $sysName {
         |  struct ${resp.responseName} {
         |    ${safe_join(structMembersWithType.map(_ + ";"), "\n    ")}
         |    ${resp.responseName}(${safe_join(structMembersWithType, ", ")}) :
         |      ${safe_join(resp.realElements.map(a => f"${a._1}(${a._1})"), ", ")}
         |      {}
         |  };
         |}
         |""".stripMargin

    val template_sig = f"template<> $structName composer::response_handle<$structName>::get()"
    val try_template_sig = f"template<> std::optional<$structName> composer::response_handle<$structName>::try_get()"
    val dec =
      f"""
         |$response_struct;
         |$template_sig;
         |#ifndef BAREMETAL
         |$try_template_sig;
         |#endif
         |""".stripMargin

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


    val definition =
      f"""
         |$template_sig {
         |  auto r = rg.get();
         |  auto resp = r.data;
         |$template_decs
         |  return $structName($template_def);
         |}
         |#ifndef BAREMETAL
         |$try_template_sig {
         |  auto r = rg.try_get();
         |  if (!r.has_value()) return {};
         |  auto resp = r->data;
         |$template_decs
         |  return $structName($template_def);
         |}
         |#endif
         |
         |""".stripMargin
    ComposerResponseDeclarations(structName, dec, definition)
  }
}
