package beethoven.Generation.Tune
import beethoven.Generation.BuildArgs

import scala.language.implicitConversions

object MainSetting {
  def apply(tag: String): MainSetting = new MainSetting(tag)

  implicit def mainSettingToInt(x: MainSetting): Int = x.intValue()

  implicit def mainSettingToBigInt(x: MainSetting): BigInt = BigInt(x.getValue)

  implicit def mainSettingToLong(x: MainSetting): Long = x.longValue()

}

class MainSetting(tag: String) extends Number {
  def getValue: Int = {
    val arg = BuildArgs.args.get(tag)
    arg match {
      case Some(a) => a
      case _ => throw new Exception(s"Expecting $tag to be defined when calling main(). ex: -D$tag=4 ")
    }
  }

  override def intValue(): Int = getValue

  override def longValue(): Long = getValue.toLong

  override def floatValue(): Float = throw new Exception("Main Setting cannot be a float")

  override def doubleValue(): Double = throw new Exception("Main Setting cannot be a double")
}
