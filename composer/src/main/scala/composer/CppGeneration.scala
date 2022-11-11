package composer

import chipsalliance.rocketchip.config.Parameters
import chisel3.util.log2Up
import freechips.rocketchip.subsystem.ExtMem

import java.io.FileWriter

trait CPPLiteral {
  def typeString: String
  def toC: String
}

trait IntLikeLiteral extends CPPLiteral {
  def bitWidth: Int
  def literalSuffix: String
  def value: BigInt
  def toC = value.toString + literalSuffix

  require(bitWidth >= value.bitLength)
}

case class UInt32(value: BigInt) extends IntLikeLiteral {
  def typeString = "unsigned int"
  def bitWidth = 32
  def literalSuffix = ""
}

case class UInt64(value: BigInt) extends IntLikeLiteral {
  def typeString = "uint64_t"
  def bitWidth = 64
  def literalSuffix = "L"
}

case class CStrLit(value: String) extends CPPLiteral {
  def typeString = "const char* const"
  def toC = "\"%s\"".format(value)
}

object CppGenerationUtils {
  val indent = "  "

  def genEnum(name: String, values: Seq[String]): String =
    if (values.isEmpty) "" else s"enum $name {%s};\n".format(values mkString ",")

  def genArray[T <: CPPLiteral](name: String, values: Seq[T]): String = {
    val tpe = if (values.nonEmpty) values.head.typeString else "const void* const"
    val prefix = s"static $tpe $name [${math.max(values.size, 1)}] = {\n"
    val body = values map (indent + _.toC) mkString ",\n"
    val suffix = "\n};\n"
    prefix + body + suffix
  }

  def genStatic[T <: CPPLiteral](name: String, value: T): String =
    "static %s %s = %s;\n".format(value.typeString, name, value.toC)

  def genConstStatic[T <: CPPLiteral](name: String, value: T): String =
    "const static %s %s = %s;\n".format(value.typeString, name, value.toC)

  def genConst[T <: CPPLiteral](name: String, value: T): String =
    "const %s %s = %s;\n".format(value.typeString, name, value.toC)

  def genMacro(name: String, value: String = ""): String = s"#define $name $value\n"

  def genMacro[T <: CPPLiteral](name: String, value: T): String =
    "#define %s %s\n".format(name, value.toC)

  def genComment(str: String): String = "// %s\n".format(str)

  def genMemoryAllocatorDeclaration(cr: MCRFileMap, acc: ComposerAcc)(implicit p: Parameters): Unit = {
    // we might have multiple address spaces...
    val f = new FileWriter("vsim/generated-src/composer_allocator_declaration.h")
    val mem = p(ExtMem).get
      f.write("// Automatically generated memory-allocator declaration from Composer-Hardware:CppGeneration\n" +
        "#include <composer/alloc.h>\n" +
        "#include <composer/rocc_cmd.h>\n" +
        "#include <cinttypes>\n" +
        "#ifndef COMPOSER_ALLOCATOR_GEN\n" +
        "#define COMPOSER_ALLOCATOR_GEN\n" +
        "#define NUM_DDR_CHANNELS " + mem.nMemoryChannels + "\n" +
        "using composer_allocator=composer::device_allocator<" + mem.master.size + ">;\n")
      cr.printCRs(Some(f))
    acc.system_tups foreach { tup =>
      f.write(s"const uint8_t ${tup._3.name}_ID = ${tup._2};\n")
    }
    f.write(s"static const uint8_t system_id_bits = ${p(SystemIDLengthKey)};\n")
    f.write(s"static const uint8_t core_id_bits = ${p(CoreIDLengthKey)};\n")
    f.write(s"static const uint8_t numChannelSelectionBits = ${p(ChannelSelectionBitsKey)}," +
      s" channelTransactionLenBits = ${log2Up(p(MaxChannelTransactionLenKey))};\n")
    f.write(s"static const composer::composer_pack_info pack_cfg(system_id_bits, core_id_bits, numChannelSelectionBits, channelTransactionLenBits);\n")
    f.write("#endif\n")
    f.close()
  }

}


