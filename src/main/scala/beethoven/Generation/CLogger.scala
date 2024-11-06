package beethoven.Generation

import beethoven.BeethovenBuild

import java.io.FileWriter

object CLogger {
  private[beethoven] var emitToStdErr: Boolean = true
  os.makeDir.all(BeethovenBuild.top_build_dir / "logs")
  private val logWriter = new FileWriter(BeethovenBuild.top_build_dir.toString() + s"/logs/beethoven_${java.time.LocalDateTime.now().toString}.log")
  def log(message: String): Unit = {
    logWriter.write(f"[${System.currentTimeMillis()}]: $message")
    if (emitToStdErr) {
      System.err.println(message)
    }
  }
  def flush(): Unit = logWriter.flush()
  def close(): Unit = logWriter.close()
}