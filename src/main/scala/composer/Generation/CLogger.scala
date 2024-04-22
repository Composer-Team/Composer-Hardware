package composer.Generation

import java.io.FileWriter

object CLogger {
  private[composer] var emitToStdErr: Boolean = true
  os.makeDir.all(os.Path(ComposerBuild.composerGenDir) / "logs")
  private val logWriter = new FileWriter(ComposerBuild.composerGenDir + s"/logs/composer_${java.time.LocalDateTime.now().toString}.log")
  def log(message: String): Unit = {
    logWriter.write(f"[${System.currentTimeMillis()}]: $message")
    if (emitToStdErr) {
      System.err.println(message)
    }
  }
  def flush(): Unit = logWriter.flush()
  def close(): Unit = logWriter.close()
}