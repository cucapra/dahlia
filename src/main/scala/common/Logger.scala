package fuselang.common

import scala.util.parsing.input.Positional
import scribe._
import scribe.format._

object Logger {
  /** Makes all positionals logable by scribe */
  implicit object PositionalLoggable extends Loggable[(String,Positional)] {
    override def apply(value: (String, Positional)) = {
      val pos = value._2.pos

      new output.TextOutput(
        s"[${pos.line}.${pos.column}] ${value._1}\n${pos.longString}")
    }
  }

  def stringToLevel(str: String): Level = str match {
    case "warn" => Level.Warn
    case "info" => Level.Info
    case "debug" => Level.Debug
    case _ => throw new RuntimeException(s"Unknown level: $str")
  }

  /**
   * Stateful function to set the logging level in the compiler.
   */
  def setLogLevel(level: Level) = {
    scribe.Logger.root.clearHandlers().withHandler(
      formatter = format, minimumLevel = Some(level)).replace()
  }

  val format: Formatter = formatter"[$levelColored] $message$newLine"

}

