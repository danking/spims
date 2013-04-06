import java.util.Date
import java.text.SimpleDateFormat

object Logging {
  abstract class LogLevel
  case object Info extends LogLevel
  case object Debug extends LogLevel

  var logLevel: LogLevel = Info
  final val DateFormat = new SimpleDateFormat("MM/dd/yy hh:mm:ss")
}

trait Logging {
  import Logging._
  def log: Unit = log("")
  def log(msg: String) = println((DateFormat.format(new Date) + " " + msg).trim)
  def info(msg: String) = log("INFO: " + msg)
  def debug(msg: String) = if (logLevel == Debug) log("DEBUG: " + msg)
  def warn(msg: String) = log("WARNING: " + msg)
}
