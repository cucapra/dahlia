package fuselang

import scala.util.Try
import scala.io.Source
import java.nio.file.{Files, Paths, Path, StandardOpenOption}

import common._
import Configuration._
import Syntax._
import Transformer.{PartialTransformer, TypedPartialTransformer}

object Compiler {

  // Transformers to execute *before* type checking.
  val preTransformers: List[(String, PartialTransformer)] = List(
    "Hoist memory reads" -> passes.HoistMemoryReads,
    "Lower unroll and bank" -> passes.LowerUnroll,
    "Lower for loops" -> passes.LowerForLoops,
  )

  // Transformers to execute *after* type checking.
  val postTransformers: List[(String, TypedPartialTransformer)] = List(
    "Rewrite views" -> passes.RewriteView
  )

  def showDebug(ast: Prog, pass: String, c: Config): Unit = {
    if (c.passDebug) {
      val top = ("=" * 15) + pass + ("=" * 15)
      println(top)
      println(Pretty.emitProg(ast)(c.logLevel == scribe.Level.Debug).trim)
      println("=" * top.length)
    }
  }

  def toBackend(str: BackendOption): fuselang.backend.Backend = str match {
    case Vivado => backend.VivadoBackend
    case Cpp => backend.CppRunnable
    case Futil => backend.futil.FutilBackend
  }

  def checkStringWithError(prog: String, c: Config = emptyConf) = {
    val preAst = FuseParser.parse(prog)

    // Run pre transformers if lowering is enabled
    val ast = if (c.enableLowering) {
      val transformed = preTransformers.foldLeft(preAst)({
        case (ast, (name, pass)) => {
          val newAst = pass.rewrite(ast)
          showDebug(newAst, name, c)
          newAst
        }
      })
      /// XXX(rachit): Prettify and reparse program to get better error
      // locations
      try {
        FuseParser.parse(Pretty.emitProg(transformed)(false))
      } catch {
        case _: Errors.ParserError => throw CompilerError.Impossible("Pretty printer generated a program that could not be parsed after code generation")
      }
    } else {
      preAst
    }
    passes.WellFormedChecker.check(ast)
    typechecker.TypeChecker.typeCheck(ast);
    showDebug(ast, "Type Checking", c)
    passes.BoundsChecker.check(ast); // Doesn't modify the AST.
    passes.LoopChecker.check(ast); // Doesn't modify the AST.
    passes.DependentLoops.check(ast); // Doesn't modify the AST.
    typechecker.CapabilityChecker.check(ast);
    showDebug(ast, "Capability Checking", c)
    typechecker.AffineChecker.check(ast); // Doesn't modify the AST
    ast
  }

  def codegen(ast: Prog, c: Config = emptyConf) = {
    // Run post transformers
    val transformedAst = postTransformers.foldLeft(ast)({
      case (ast, (name, pass)) => {
        val newAst = pass.rewrite(ast)
        showDebug(newAst, name, c)
        newAst
      }
    })
    toBackend(c.backend).emit(transformedAst, c)
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
            s"[${red("Impossible")}] ${err.getMessage}\n" +
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
