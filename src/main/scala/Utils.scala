package fuselang

object Utils {

  import scala.util.{Try, Success, Failure}

  def parseAst(s: String) = Try(FuseParser.parse(s)) match {
    case Success(p) => p
    case Failure(Errors.ParserError(msg)) => throw new RuntimeException(s"$msg")
    case Failure(f) => throw f
  }

  def typeCheck(s: String) = Try {
    TypeChecker.checkFuse(FuseParser.parse(s))
  } match {
    case Success(p) => p
    case Failure(f) => throw new RuntimeException(s"$f")
  }

}
