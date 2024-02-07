package composer.Generation.CPP

import chisel3.Data
import composer.Systems.AcceleratorCore.Address

object TypeParsing {
  def getCType(dat: Data, name: String, width: Int, unsigned: Boolean): String = {
    val isFloatingPoint = name.contains("FP")
    val isFpgaAddress = dat.isInstanceOf[Address]
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

}