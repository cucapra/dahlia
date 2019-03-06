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

  def compileString(prog: String, c: Config): Either[String, String] = Try {
    compileStringWithError(prog, c)
  } match {
    case Success(out) => Right(out)
    case Failure(f: Errors.TypeError) =>
      Left("[" + Console.RED + "Type error" + Console.RESET + "] " + f.getMessage)
    case Failure(f: Errors.ParserError) =>
      Left("[" + Console.RED + "Parsing error" + Console.RESET + "] " + f.getMessage)
    case Failure(f: Errors.Impossible) =>
      Left("[" + Console.RED + "Impossible" + Console.RESET + "] " + f.getMessage +
        " This should never trigger. Please report this as a bug.")
    case Failure(f: RuntimeException) =>
      Left("[" + Console.RED + "Error" + Console.RESET + "] " + f.getMessage)
    case Failure(f) => Left(f.getMessage)
  }

  def compileStringToFile(prog: String, c: Config, out: String): Either[String, Path] = {
    import java.nio.file.{Files, Paths, StandardOpenOption}

    compileString(prog, c).map(p => {
      Files.write(
        Paths.get(out),
        p.toCharArray.map(_.toByte),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.WRITE)
    })
  }

}
