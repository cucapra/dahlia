package fuselang

object TestUtils {

  import scala.language.implicitConversions

  // Allow for env("x") style calls.
  implicit def stringToId(s: String) = {
    Syntax.Id(s)
  }

  def parseAst(s: String) = FuseParser.parse(s)
  def typeCheck(s: String) = {
    val ast = FuseParser.parse(s);
    CapabilityChecker.check(ast);
    val env = TypeChecker.typeCheck(ast);
    BoundsChecker.check(ast);
    env
  }
}
