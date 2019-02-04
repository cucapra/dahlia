package fuselang

import scala.util.{Try, Success, Failure}

object Compiler {

  def compileString(prog: String) = Try {
    val ast = FuseParser.parse(prog)
    TypeChecker.checkFuse(ast)
    Emit.emitC(ast)
  } match {
    case Success(out) => out
    case Failure(f: RuntimeException) =>
      "[" + Console.RED + "Error" + Console.RESET + "] " + f.getMessage
    case Failure(f) => throw f
  }
}
