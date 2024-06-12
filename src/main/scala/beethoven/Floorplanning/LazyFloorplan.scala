package beethoven.Floorplanning

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3.stage.ChiselStage
import beethoven.Generation.{BeethovenBuild, BeethovenChipStage}
import beethoven.Generation.Stage.BeethovenAnnotations
import beethoven.Systems.AcceleratorCore

object LazyFloorplan {
  def registerSystem[T <: AcceleratorCore](nCores: Int, module: => T, outwardsConnectivity: Iterable[String])(implicit p: Parameters): Unit = {
    // estimate the cost of module using SNS
    val testPath = os.Path("/Users/chris/Desktop/testpath")
    os.remove.all(testPath)
    os.makeDir.all(testPath)
    new ChiselStage().emitVerilog(module, annotations = BeethovenAnnotations.no_class(new Config(p),
      emitAllModules = false,
      performDeadCodeElimination = true,
      targetDir = testPath))
    // find the name of the .v file that was created
    val fname = os.walk(testPath).filter(_.ext == "v").map(_.last).head
    val lines = os.read.lines(testPath / fname)
    val fpath = testPath / fname
    // now, clear the file, take all of the sources added through addSource and add them to the beginning, then write the lines back
    os.write.over(fpath, "")
    BeethovenBuild.sourceList.foreach {
      path =>
        // if the path is a directory, copy the directory using os.proc
        if (os.isDir(path)) {
          os.proc("cp", "-r", path.toString(), testPath.toString()).call()
        } else {
          os.write.append(fpath, os.read(path))
        }
    }
    // finally, write the lines back
    os.write.append(fpath, lines.mkString("\n"))
  }
}
