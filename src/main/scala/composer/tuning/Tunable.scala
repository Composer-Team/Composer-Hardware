package composer.tuning

import composer.{BuildArgs, ComposerBuild}
import composer.tuning.Tunable.tunables
import freechips.rocketchip.diplomacy.ValName

import java.io.FileWriter
import scala.language.implicitConversions

object Tunable {
  private var tunables: Seq[Tunable] = Seq.empty

  def apply(default: Int, range: (Int, Int))(implicit valName: ValName): Tunable = {
    new Tunable(default, range)
  }

  def exportNames(): Unit = {
    val fname = ComposerBuild.composerGenDir + "/tunables.json"
    val fwriter = new FileWriter(fname)
    val ts = tunables.map ( tune =>
      f"\t\"${tune.name}\" : {\n\t\t\"default\" : ${tune.default},\n\t\t\"range\" : [${tune.range._1}, ${tune.range._2}]\n\t}")
    fwriter.write(if (ts.isEmpty) "" else f"{\n${ts.reduce(_ + ",\n" + _)}\n}")
    fwriter.close()
  }
  implicit def tunableToInt(x: Tunable): Int = x.getValue
}


class Tunable(val default: Int, val range: (Int, Int))(implicit valName: ValName) {
  private val name = valName.name
  assert(!tunables.contains(this) || tunables.filter(_.equals(this)).map(other => other.range == this.range && other.default == this.default).reduce(_ && _))
  if (!tunables.contains(this)) {
    tunables = tunables :+ this
  }
  def getValue: Int = {
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
