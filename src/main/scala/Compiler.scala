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
    "Sequentialize" -> passes.Sequentialize,
    "Lower unroll and bank" -> passes.LowerUnroll,
    "Lower for loops" -> passes.LowerForLoops,
    "Hoist slow binops" -> passes.HoistSlowBinop
  )

  // Transformers to execute *after* type checking. Boolean indicates if the
  // pass should only run when passed the `--lower` flag.
  val postTransformers: List[(String, (TypedPartialTransformer, Boolean))] =
    List(
      "Rewrite views" -> (passes.RewriteView, false),
      "Add bitwidth" -> (passes.AddBitWidth, true)
    )

  def showDebug(ast: Prog, pass: String, c: Config): Unit = {
    if c.passDebug then {
      val top = ("=" * 15) + pass + ("=" * 15)
      println(top)
      println(Pretty.emitProg(ast)(c.logLevel == scribe.Level.Debug).trim)
      println("=" * top.length)
    }
  }

  def toBackend(str: BackendOption): fuselang.backend.Backend = str match {
    case Vivado => backend.VivadoBackend
    case Cpp => backend.CppRunnable
    case Calyx => backend.calyx.CalyxBackend
  }

  def checkStringWithError(prog: String, c: Config = emptyConf) = {
    val preAst = Parser(prog).parse()

    showDebug(preAst, "Original", c)

    // Run pre transformers if lowering is enabled
    val ast = if c.enableLowering then {
      preTransformers.foldLeft(preAst)({
        case (ast, (name, pass)) => {
          val newAst = pass.rewrite(ast)
          showDebug(newAst, name, c)
          newAst
          /* if (c.passDebug) {
            try {
              // Print and re-parse program with pass debug
              Parser(Pretty.emitProg(newAst)(false)).parse()
            } catch {
              case _: Errors.ParserError =>
                throw CompilerError.Impossible(
                  "Pretty printer generated a program that could not be parsed after code generation.\nUse the --pass-debug flag to see what the generated program looked like."
                )
            }
          } else {
            newAst
          } */
        }
      })
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
    // Filter out transformers not running in this mode
    val toRun = postTransformers.filter({
      case (_, (_, onlyLower)) => {
        !onlyLower || c.enableLowering
      }
    })
    // Run post transformers
    val transformedAst = toRun.foldLeft(ast)({
      case (ast, (name, (pass, _))) => {
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
        scribe.info(err.getStackTrace().take(10).mkString("\n"))
        err match {
          case _: Errors.TypeError => {
            s"[${red("Type error")}] ${err.getMessage}" +
              (if c.enableLowering then
                 "\nDoes this program type check without the `--lower` flag? If it does, please report this as a bug: https://github.com/cucapra/dahlia/issues/new"
               else "")
          }
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
        val meta = scala.io.Source.fromResource("version.properties")
          .getLines()
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
