package fuselang

import scala.util.Try
import scala.io.Source
import java.nio.file.{Files, Paths, Path, StandardOpenOption}

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
    case Cpp => backend.CppRunnable
    case Futil => backend.futil.FutilBackend
  }

  var allTime: Map[String, Double] = Map()

  def time[R](name: String, block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    val time = (t1 - t0) / 1000000.0
    allTime += s"$name: " -> time
    result
  }

  def checkStringWithError(prog: String, c: Config = emptyConf) = {
    val ast = time("parse", { FuseParser.parse(prog) })
    time("well formedness", { passes.WellFormedChecker.check(ast) })
    time("type checking", {
      typechecker.TypeChecker.typeCheck(ast); showDebug(ast, "Type Checking", c)
    })
    time(
      "bounds checking", { passes.BoundsChecker.check(ast) }
    ); // Doesn't modify the AST.
    time(
      "loop checking", { passes.LoopChecker.check(ast) }
    ); // Doesn't modify the AST.
    time(
      "dependent loop checking", { passes.DependentLoops.check(ast) }
    ); // Doesn't modify the AST.
    time("capability checking", { typechecker.CapabilityChecker.check(ast) });
    showDebug(ast, "Capability Checking", c)
    typechecker.AffineChecker.check(ast); // Doesn't modify the AST
    time(
      "affine checking", { typechecker.AffineChecker.check(ast) }
    ); // Doesn't modify the AST
    ast
  }

  def codegen(ast: Prog, c: Config = emptyConf) = {
    val rast = passes.RewriteView.rewrite(ast);
    showDebug(rast, "Rewrite Views", c)
    allTime.toList
      .sortBy(-_._2)
      .foreach({
        case (pass, time) =>
          System.err.println(f"$pass: ${time}%2.2f" + "ms")
      })

    // Perform program lowering if needed.
    val finalAst: Prog = if (c.enableLowering) {
      val fast = passes.LowerForLoops.rewrite(rast)
      showDebug(fast, "LowerForLoops", c)
      fast
    } else {
      rast
    }

    toBackend(c.backend).emit(finalAst, c)
  }

  // Outputs red text to the console
  def red(txt: String): String = {
    Console.RED + txt + Console.RESET
  }

  def compileString(prog: String, c: Config): Either[String, String] = {
    Try(codegen(checkStringWithError(prog, c), c)).toEither.left
      .map(err => {
        scribe.debug(err.getStackTrace().take(10).mkString("\n"))
        err match {
          case _: Errors.TypeError =>
            s"[${red("Type error")}] ${err.getMessage}"
          case _: Errors.ParserError =>
            s"[${red("Parsing error")}] ${err.getMessage}"
          case _: CompilerError.Impossible =>
            s"[${red("Impossible")}] ${err.getMessage}. " +
              "This should never trigger. Please report this as a bug."
          case _ => s"[${red("Error")}] ${err.getMessage}"
        }
      })
      .map(out => {
        // Get metadata about the compiler build.
        val version = getClass.getResourceAsStream("/version.properties")
        val meta = Source
          .fromInputStream(version)
          .getLines
          .filter(l => l.trim != "")
          .mkString(", ")
        val commentPre = toBackend(c.backend).commentPrefix
        s"$commentPre $meta\n" + out
      })
  }

  def compileStringToFile(
      prog: String,
      c: Config,
      out: String
  ): Either[String, Path] = {

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
