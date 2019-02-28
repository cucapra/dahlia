package fuselang

import scala.util.{Try, Success, Failure}
import backend.VivadoBackend
import Utils._

object Compiler {

  def compileStringWithError(prog: String, c: Config = emptyConf ) = {
    val ast = FuseParser.parse(prog)
    TypeChecker.typeCheck(ast)
    val rast = RewriteView.rewriteProg(ast)
    VivadoBackend.emitProg(rast, c)
  }

  def compileString(prog: String, c: Utils.Config) = Try {
    compileStringWithError(prog, c)
  } match {
    case Success(out) => out
    case Failure(f: Errors.TypeError) =>
      "[" + Console.RED + "Type error" + Console.RESET + "] " + f.getMessage
    case Failure(f: RuntimeException) =>
      "[" + Console.RED + "Error" + Console.RESET + "] " + f.getMessage
    case Failure(f) => throw f
  }
}
