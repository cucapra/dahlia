package fuselang

import scala.util.{Try, Success, Failure}

object Compiler {

  def compileString(prog: String) = Try {
    val ast = FuseParser.parse(prog)
    val env = TypeChecker.checkFuse(ast)
    Emit.emitC(ast, env)
  } match {
    case Success(out) => out
    case Failure(f: RuntimeException) => s"$f"
    case Failure(f) => throw f
  }
}
