package fuselang

import scala.util.Try
import java.nio.file.Path

import common._
import Configuration._
import Syntax._

object Compiler {

  def showDebug(ast: Prog, pass: String, c: Config): Unit = {
    if (c.passDebug) {
      val top = ("=" * 15) + pass + ("=" * 15)
      println(top)
      println(Pretty.emitProg(ast)(true).trim)
      println("=" * top.length)
    }
  }

  def toBackend(str: BackendOption): fuselang.backend.Backend = str match {
    case Vivado => backend.VivadoBackend
    case Cpp    => backend.CppRunnable
    case Futil  => backend.futil.FutilBackend
  }

  def compileStringWithError(prog: String, c: Config = emptyConf) = {
    val ast = FuseParser.parse(prog)
    typechecker.CapabilityChecker.check(ast); showDebug(ast, "Capability Checking", c)
    typechecker.TypeChecker.typeCheck(ast); showDebug(ast, "Type Checking", c)
    passes.BoundsChecker.check(ast);  // Doesn't modify the AST.
    passes.LoopChecker.check(ast);  // Doesn't modify the AST.
    val rast = passes.RewriteView.rewriteProg(ast); showDebug(rast, "Rewrite Views", c)
    toBackend(c.backend).emit(rast, c)
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
        case _: Errors.ParserError =>
          s"[${red("Parsing error")}] ${f.getMessage}"
        case _: CompilerError.Impossible =>
          s"[${red("Impossible")}] ${f.getMessage}. " +
            "This should never trigger. Please report this as a bug."
        case _ => s"[${red("Error")}] ${f.getMessage}"
      }
    })
  }

  def compileStringToFile(
      prog: String,
      c: Config,
      out: String
  ): Either[String, Path] = {
    import java.nio.file.{Files, Paths, StandardOpenOption}

    compileString(prog, c).map(p => {
      Files.write(
        Paths.get(out),
        p.toCharArray.map(_.toByte),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.WRITE
      )
    })
  }

}
