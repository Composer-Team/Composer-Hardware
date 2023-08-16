package composer.Generation.Tune

import composer.Generation.{BuildArgs, ComposerBuild}

import java.io.FileWriter
import scala.language.implicitConversions


object Tunable {
  implicit def tunableToInt(x: Tunable): Int = x.getValue

  implicit def tunableToBigInt(x: Tunable): BigInt = BigInt(x.getValue)

  implicit def intToTunable(x: Int): Tunable = new UnitTunable(x)

  private[composer] var tunables: Seq[Tunable with TaggedTunable] = Seq.empty
  protected[composer] var tuneCounter = 0

  private[composer] def exportNames(): Unit = {
    val fname = ComposerBuild.composerGenDir + "/tunables.json"
    val fwriter = new FileWriter(fname)
    val ts = tunables.map(tune =>
      f"\t\"${tune.tag}\" : {\n\t\t\"default\" : ${tune.default},\n\t\t\"range\" : [${tune.range._1}, ${tune.range._2}]\n\t}")
    fwriter.write(if (ts.isEmpty) "" else f"{\n${ts.reduce(_ + ",\n" + _)}\n}")
    fwriter.close()
  }

}

private[composer] class UnitTunable(x: Int) extends Tunable {
  override def getValue: Int = x

  override val default: Int = x
  override val range: (Int, Int) = (x, x)

  override def isIdentity: Boolean = true
}

object InstanceTunable {
  def apply(default: Int, range: (Int, Int), name: Option[String] = None) =
    new InstanceTunable(default, range, name)
}

class InstanceTunable(val default: Int, val range: (Int, Int), name: Option[String] = None) extends Tunable with TaggedTunable {
  val tag: String = name.map { n =>
    if (Tunable.tunables.exists(_.tag == n)) {
      Tunable.tuneCounter = Tunable.tuneCounter + 1
      n + (Tunable.tuneCounter - 1).toString
    } else n
  }.getOrElse("tunable_" + Tunable.tuneCounter)

  override def isIdentity: Boolean = default == range._1 && default == range._2

  if (!isIdentity) {
    if (!Tunable.tunables.contains(this)) {
      Tunable.tunables = Tunable.tunables :+ this
    }
  }

  override def getValue: Int = {
    val arg = BuildArgs.args.get(tag)
    arg match {
      case Some(a) => println("Found " + a + " in args for " + tag)
      case _ => println("Using " + default + " for " + tag)
    }
    arg.getOrElse(default)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case a: Tunable with TaggedTunable => a.tag == this.tag
      case _ => false
    }
  }
}

object GlobalTunable {
  def apply(default: Int, range: (Int, Int), name: String) = new GlobalTunable(default, range, name)
}

class GlobalTunable(val default: Int, val range: (Int, Int), name: String) extends Tunable with TaggedTunable {
  val tag: String = name

  override def isIdentity: Boolean = default == range._1 && default == range._2

  if (!Tunable.tunables.contains(this)) {
    Tunable.tunables = Tunable.tunables :+ this
  }

  override def getValue: Int = {
    val arg = BuildArgs.args.get(tag)
    arg.getOrElse(default)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case a: Tunable with TaggedTunable => a.tag == this.tag
      case _ => false
    }
  }
}

trait TaggedTunable {
  val tag: String
}

abstract class Tunable() extends Number {
  def getValue: Int

  val default: Int
  val range: (Int, Int)

  def isIdentity: Boolean

  def *(y: Tunable): Tunable = new TunableProduct(Seq(this, y))

  def *(y: Int): Tunable = new TunableScalarProduct(this, y)

  def +(y: Tunable): Tunable = new TunableSum(Seq(this, y))

  def +(y: Int): Tunable = new TunableScalarSum(this, y)

  def ==(y: Int): Boolean = getValue == y

  def ==(y: Tunable): Boolean = getValue == y.getValue

  def log2(): Tunable = new TunableLog2(this)

  def pow(power: Int): Tunable = new TunablePow(this, power)

  override def intValue: Int = getValue

  override def longValue: Long = getValue.toLong

  override def floatValue: Float = getValue.toFloat

  override def doubleValue: Double = getValue.toDouble
}

private[composer] class TunableSum(sum: Seq[Tunable]) extends Tunable {
  override def isIdentity: Boolean = sum.map(_.isIdentity).fold(true)(_ && _)
  val members: Seq[Tunable] = sum.flatMap { m =>
    m match {
      case a: TunableSum => a.members
      case a: Tunable => Seq(a)
    }
  }

  override val default: Int = sum.map(_.default).sum

  override val range: (Int, Int) = (sum.map(m => m.range._1).sum, sum.map(m => m.range._2).sum)

  override def getValue: Int = members.map(_.getValue).sum
}

private[composer] class TunableScalarProduct(val tunable: Tunable, val scalar: Int) extends Tunable {
  override def isIdentity: Boolean = tunable.isIdentity
  override val range: (Int, Int) = (tunable.range._1 * scalar, tunable.range._2 * scalar)
  override val default: Int = tunable.default * scalar
  override def getValue: Int = tunable.getValue * scalar
}

private[composer] class TunableProduct(val tunables: Seq[Tunable]) extends Tunable {
  override def isIdentity: Boolean = tunables.map(_.isIdentity).fold(true)(_ && _)
  override val range: (Int, Int) = (tunables.map(m => m.range._1).product, tunables.map(m => m.range._2).product)
  override val default: Int = tunables.map(_.default).product
  override def getValue: Int = tunables.map(_.getValue).product
}

private[composer] class TunableScalarSum(val tunable: Tunable, val scalar: Int) extends Tunable {
  override def isIdentity: Boolean = tunable.isIdentity
  override val range: (Int, Int) = (tunable.range._1 + scalar, tunable.range._2 + scalar)
  override val default: Int = tunable.default + scalar
  override def getValue: Int = tunable.getValue + scalar
}

private[composer] class TunablePow(val tunable: Tunable, val pow: Int) extends Tunable {
  override def isIdentity: Boolean = tunable.isIdentity
  override val range: (Int, Int) = (Math.pow(tunable.range._1, pow).toInt, Math.pow(tunable.range._2, pow).toInt)
  override val default: Int = Math.pow(tunable.default, pow).toInt
  override def getValue: Int = Math.pow(tunable.getValue, pow).toInt
}

private[composer] class TunableLog2(val tunable: Tunable) extends Tunable {
  override def isIdentity: Boolean = tunable.isIdentity
  private def l2(x: Int): Int = (Math.log(x)/Math.log(Math.E)).toInt
  override val range: (Int, Int) = (l2(tunable.range._1), l2(tunable.range._2))
  override val default: Int = l2(tunable.default)
  override def getValue: Int = l2(tunable.getValue)
}