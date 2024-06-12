package beethoven.Generation

import java.io.FileWriter

object CLogger {
  private[beethoven] var emitToStdErr: Boolean = true
  os.makeDir.all(os.Path(BeethovenBuild.beethovenGenDir) / "logs")
  private val logWriter = new FileWriter(BeethovenBuild.beethovenGenDir + s"/logs/beethoven_${java.time.LocalDateTime.now().toString}.log")
  def log(message: String): Unit = {
    logWriter.write(f"[${System.currentTimeMillis()}]: $message")
    if (emitToStdErr) {
      System.err.println(message)
    }
  }
  def flush(): Unit = logWriter.flush()
  def close(): Unit = logWriter.close()
}