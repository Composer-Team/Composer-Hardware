package beethoven.Generation

object DotGen {
  var digraph = "digraph G {\noverlap=false;\n"
  var nodeCount: Int = 0
  var existingNames = List.empty[String]

  private def fixName(a: String): (String, String) = {
    val dedup = if (existingNames.contains(a)) {
      nodeCount += 1
      f"$a.$nodeCount"
    } else {
      existingNames = a :: existingNames
      a
    }
    val rname = dedup.replaceAll("\\.", "_").replaceAll("\\[", "_").replaceAll("\\]", "_")
    val label = a.replaceAll("\\.", "\\\\n").replaceAll("\\[", "_").replaceAll("\\]", "_")
    (rname, label)
  }

  def slr2color(slr: Int): String = {
    if (slr == -1) "black"
    else Seq("red", "green", "blue", "yellow", "purple", "orange", "cyan", "magenta")(slr)
  }

  def addPortNode(name: String, slr: Int = -1): String = {
    val (fname, label) = fixName(f"port_$name$nodeCount")
    digraph += s"  $fname [shape=box, color=${slr2color(slr)}, label=\"$label\", penwidth=4];\n"
    fname
  }

  // make the lines thicker
  def addModuleNode(name: String, slr: Int = -1): String = {
    val (fname, label) = fixName(f"mod_$name$nodeCount")
    digraph += s"  $fname [shape=box, color=${slr2color(slr)}, penwidth=4, label=\"$label\"];\n"
    fname
  }

  def addBufferNode(name: String, slr: Int = -1): String = {
    val (fname, label) = fixName(f"buf_$name$nodeCount")
    digraph += s"  $fname [shape=component, color=${slr2color(slr)}, penwidth=4, label=\"$label\"];\n"
    fname
  }

  // if imaginary, make the node dotted
  def addNode(name: String, slr: Int = -1, imaginary: Boolean = false): String = {
    val (fname, label) = fixName(f"$name$nodeCount")
    digraph += s"  $fname [color=${slr2color(slr)} style=${if (imaginary) "dotted" else "solid"}, label=\"$label\"];\n"
    fname
  }

  def addEdge(src: String, dst: String): Unit = {
    digraph += s"  $src -> $dst;\n"
  }

  def writeToFile(fileName: String): Unit = {
    digraph += "}"
    val writer = new java.io.PrintWriter(new java.io.File(fileName))
    writer.write(digraph)
    writer.close()
  }
}
