package composer.Generation

package object CPP {
  private[composer] def safe_join(s: Seq[String], sep: String = "\n"): String = if (s.isEmpty) "" else if (s.length == 1) s(0) else s.reduce(_ + sep + _)
}