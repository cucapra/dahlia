package fuselang

import scala.util.parsing.input.Positional
import scribe._
import scribe.format._

object Log {
  /** Makes all positionals logable by scribe */
  implicit object PositionalLoggable extends Loggable[(String,Positional)] {
    override def apply(value: (String, Positional)) = {
      new output.TextOutput(s"${value._1}\n${value._2.pos.longString}")
    }
  }

  val format: Formatter = formatter"[$levelColored] $message$newLine"

}

