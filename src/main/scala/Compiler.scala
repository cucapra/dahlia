package fuselang

import scala.util.Try
import java.nio.file.Path

import CmdlineConfig._
import common._

object Compiler {

  def compileStringWithError(prog: String, c: CmdlineConfig = emptyConf ) = {
    val ast = FuseParser.parse(prog)
    passes.CapabilityChecker.check(ast)
    typechecker.TypeChecker.typeCheck(ast)
    passes.BoundsChecker.check(ast);
    val rast = passes.RewriteView.rewriteProg(ast)
    c.backend.emit(rast, c.toCommonConfig())
  }

  // Outputs red text to the console
  def red(txt: String): String = {
    Console.RED + txt + Console.RESET
  }

  def compileString(prog: String, c: Config): Either[String, String] = {
    Try(compileStringWithError(prog, c)).toEither.left.map(f => {
      scribe.debug(f.getStackTrace().mkString("\n"))
      f match {
        case _: Errors.TypeError => s"[${red("Type error")}] ${f.getMessage}"
        case _: Errors.ParserError => s"[${red("Parsing error")}] ${f.getMessage}"
        case _: CompilerError.Impossible =>
          s"[${red("Impossible")}] ${f.getMessage}. " +
          "This should never trigger. Please report this as a bug."
        case _ => s"[${red("Error")}] ${f.getMessage}"
      }
    })
  }

  def compileStringToFile(prog: String, c: CmdlineConfig, out: String): Either[String, Path] = {
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
