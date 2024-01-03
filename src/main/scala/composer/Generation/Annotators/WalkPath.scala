package composer.Generation.Annotators

import os.Path
import java.nio.file

object WalkPath {
  def apply(p: Path, depth: Int = -1): Iterable[Path] = {
    if (file.Files.isRegularFile(java.nio.file.Paths.get(p.toString()))) {
      List(p)
    } else {
      if (depth == 0)
        os.walk(p).map(a => WalkPath(a, depth - 1)).reduce(_ ++ _)
      else
        os.walk(p)
    }
  }
}
