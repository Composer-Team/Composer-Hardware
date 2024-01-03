package composer.Generation.Annotators

import os.Path

import java.nio.file

object UniqueMv {
  def apply(sourceList: Seq[Path], outputDir: Path): Seq[Path] = {
    sourceList.distinct.map { src =>
      val srcFileName = src.segments.toSeq.last
      os.copy.over(src, outputDir / srcFileName)
      outputDir / srcFileName
    }
  }
}
