package composer.Generation

import composer.Generation.Tunable._
import composer.{BuildArgs, ComposerBuild}
import freechips.rocketchip.diplomacy.ValName

import java.io.FileWriter
import scala.language.implicitConversions

object Tunable {
  private var tunables: Seq[Tunable] = Seq.empty

  def apply(default: Int, range: (Int, Int))(implicit valName: ValName): Tunable = {
    new Tunable(default, range)
  }

  private[composer] def exportNames(): Unit = {
    val fname = ComposerBuild.composerGenDir + "/tunables.json"
    val fwriter = new FileWriter(fname)
    val ts = tunables.map(tune =>
      f"\t\"${tune.name}\" : {\n\t\t\"default\" : ${tune.default},\n\t\t\"range\" : [${tune.range._1}, ${tune.range._2}]\n\t}")
    fwriter.write(if (ts.isEmpty) "" else f"{\n${ts.reduce(_ + ",\n" + _)}\n}")
    fwriter.close()
  }

  implicit def tunableToInt(x: BaseTunable): Int = x.getValue
  implicit def tunableToBigInt(x: BaseTunable): BigInt = BigInt(x.getValue)
  implicit def intToTunable(x: Int)(implicit valName: ValName): Tunable = new Tunable(x, (x, x))

}

abstract class BaseTunable() extends Number {
  def getValue: Int
  val range: (Int, Int)
  def isIdentity: Boolean

  def *(y: BaseTunable): BaseTunable = new TunableProduct(Seq(this, y))

  def *(y: Int): BaseTunable = new TunableScalarProduct(this, y)

  def +(y: BaseTunable): BaseTunable = new TunableSum(Seq(this, y))

  def +(y: Int): BaseTunable = new TunableScalarSum(this, y)

  def ==(y: Int): Boolean = getValue == y

  def ==(y: BaseTunable): Boolean = getValue == y.getValue

  def log2(): BaseTunable = new TunableLog2(this)

  def pow(power: Int): BaseTunable = new TunablePow(this, power)

  override def intValue: Int = getValue

  override def longValue: Long = getValue.toLong

  override def floatValue: Float = getValue.toFloat

  override def doubleValue: Double = getValue.toDouble
}

class Tunable(val default: Int, val range: (Int, Int))(implicit valName: ValName) extends BaseTunable {
  private val name: String = valName.name

  override def isIdentity: Boolean = default == range._1 && default == range._2
  if (!isIdentity) {
    assert(!tunables.contains(this) || tunables.filter(_.equals(this)).map(other => other.range == this.range && other.default == this.default).reduce(_ && _))
    if (!tunables.contains(this)) {
      tunables = tunables :+ this
    }
  }

  override def getValue: Int = {
    val arg = BuildArgs.args.get(name)
    arg.getOrElse(default)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case a: Tunable => a.name == this.name
      case _ => false
    }
  }
}

private[composer] class TunableSum(sum: Seq[BaseTunable]) extends BaseTunable {
  override def isIdentity: Boolean = sum.map(_.isIdentity).fold(true)(_ && _)
  val members: Seq[BaseTunable] = sum.flatMap { m =>
    m match {
      case a: TunableSum => a.members
      case a: BaseTunable => Seq(a)
    }
  }

  override val range: (Int, Int) = (sum.map(m => m.range._1).sum, sum.map(m => m.range._2).sum)

  override def getValue: Int = members.map(_.getValue).sum
}

private[composer] class TunableScalarProduct(val tunable: BaseTunable, val scalar: Int) extends BaseTunable {
  override def isIdentity: Boolean = tunable.isIdentity
  override val range: (Int, Int) = (tunable.range._1 * scalar, tunable.range._2 * scalar)

  override def getValue: Int = tunable.getValue * scalar
}

private[composer] class TunableProduct(val tunables: Seq[BaseTunable]) extends BaseTunable {
  override def isIdentity: Boolean = tunables.map(_.isIdentity).fold(true)(_ && _)
  override val range: (Int, Int) = (tunables.map(m => m.range._1).product, tunables.map(m => m.range._2).product)
  override def getValue: Int = tunables.map(_.getValue).product
}

private[composer] class TunableScalarSum(val tunable: BaseTunable, val scalar: Int) extends BaseTunable {
  override def isIdentity: Boolean = tunable.isIdentity
  override val range: (Int, Int) = (tunable.range._1 + scalar, tunable.range._2 + scalar)
  override def getValue: Int = tunable.getValue + scalar
}

private[composer] class TunablePow(val tunable: BaseTunable, val pow: Int) extends BaseTunable {
  override def isIdentity: Boolean = tunable.isIdentity
  override val range: (Int, Int) = (Math.pow(tunable.range._1, pow).toInt, Math.pow(tunable.range._2, pow).toInt)
  override def getValue: Int = Math.pow(tunable.getValue, pow).toInt
}

private[composer] class TunableLog2(val tunable: BaseTunable) extends BaseTunable {
  override def isIdentity: Boolean = tunable.isIdentity
  private def l2(x: Int): Int = (Math.log(x)/Math.log(Math.E)).toInt
  override val range: (Int, Int) = (l2(tunable.range._1), l2(tunable.range._2))
  override def getValue: Int = l2(tunable.getValue)
}