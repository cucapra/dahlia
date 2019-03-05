package fuselang

import scala.util.{Try, Success, Failure}
import java.nio.file.Path
import Utils._

object Compiler {

  def compileStringWithError(prog: String, c: Config = emptyConf ) = {
    val ast = FuseParser.parse(prog)
    TypeChecker.typeCheck(ast)
    val rast = RewriteView.rewriteProg(ast)
    c.backend.emitProg(rast, c)
  }

  def compileString(prog: String, c: Config) = Try {
    compileStringWithError(prog, c)
  } match {
    case Success(out) => out
    case Failure(f: Errors.TypeError) =>
      "[" + Console.RED + "Type error" + Console.RESET + "] " + f.getMessage
    case Failure(f: RuntimeException) =>
      "[" + Console.RED + "Error" + Console.RESET + "] " + f.getMessage
    case Failure(f) => throw f
  }

  def compileStringToFile(prog: String, c: Config, out: String): Path = {
    import java.nio.file.{Files, Paths, StandardOpenOption}

    Files.write(
      Paths.get(out),
      compileString(prog, c).toCharArray.map(_.toByte),
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING,
      StandardOpenOption.WRITE)
  }

}
