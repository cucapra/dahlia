package fuselang

import scala.util.{Try, Success, Failure}
import Errors.{TypeError, ParserError}

object Compiler {

  def compileString(prog: String) = Try {
    val ast = FuseParser.parse(prog)
    val env = TypeChecker.checkFuse(ast)
    Emit.emitC(ast, env)
  } match {
    case Success(out) => out
    case Failure(err) if err.isInstanceOf[TypeError] => s"[Type Error] $err"
    case Failure(ParserError(msg)) => s"[Parsing Error] $msg"
    case Failure(err) => throw err
  }
}
