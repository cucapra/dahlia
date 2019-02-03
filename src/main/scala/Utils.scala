package fuselang

object Utils {

  import scala.language.implicitConversions

  // Allow for env("x") style calls.
  implicit def stringToId(s: String) = {
    Syntax.Id(s)
  }

  def parseAst(s: String) = FuseParser.parse(s)
  def typeCheck(s: String) = TypeChecker.checkFuse(FuseParser.parse(s))

}
