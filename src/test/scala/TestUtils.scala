package fuselang

import common._
import Compiler._

object TestUtils {

  import scala.language.implicitConversions

  // Allow for env("x") style calls.
  implicit def stringToId(s: String): Syntax.Id = {
    Syntax.Id(s)
  }

  def parseAst(s: String): Syntax.Prog = Parser(s).parse()
  def typeCheck(s: String): Syntax.Prog = checkStringWithError(s)
}
