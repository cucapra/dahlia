package fuselang

import scala.util.parsing.input.Position
import scala.util.Try
import scala.io.Source
import java.nio.file.{Files, Paths, Path, StandardOpenOption}

import common._
import Configuration._
import Syntax._
import Transformer.{PartialTransformer, TypedPartialTransformer}
import upickle.default.*

object Compiler:

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

  def assignPathDescriptors(cmd: Command, current_id: String, descriptorMap: Map[Int, String]): Map[Int, String] = {
    cmd match {
      case CSeq(seq) => {
        val seqId = current_id + "-"
        val dmWithSeqId = descriptorMap + (cmd.pos.line -> seqId)
        val (_, finalMap) = seq.foldLeft((1, dmWithSeqId))((acc, seqChild) =>
          val (id, dm) = acc
          val childId = seqId + id
          val newDm = dm ++ assignPathDescriptors(seqChild, childId, dm)
          (id + 1, newDm)
        )
        finalMap
      }
      case CPar(par) => {
        val parId = current_id + "-"
        val dmWithParId = descriptorMap + (cmd.pos.line -> parId)
        val (_, finalMap) = par.foldLeft((1, dmWithParId))((acc, parChild) =>
          val (id, dm) = acc
          val childId = parId + id
          val newDm = dm ++ assignPathDescriptors(parChild, childId, descriptorMap)
          (id + 1, newDm)
        )
        finalMap
      }
      case CFor(r, p, par, combine) => {
        val forId = current_id + "-"
        val parId = forId + "pb"
        val combineId = forId + "cb"
        val parDm = assignPathDescriptors(par, parId, descriptorMap + (cmd.pos.line -> forId))
        assignPathDescriptors(combine, combineId, parDm)
      }
      case CIf(_, cons, alt) => {
        val ifId = current_id + "-"
        val consId = ifId + "t"
        val altId = ifId + "f"
        val consDm = assignPathDescriptors(cons, consId, descriptorMap + (cmd.pos.line -> ifId))
        assignPathDescriptors(alt, altId, consDm)
      }
      case CBlock(b) => {
        val blockId = current_id + "-"
        assignPathDescriptors(b, blockId, descriptorMap + (cmd.pos.line -> blockId))
      }
      case CEmpty => {descriptorMap}
      case _ => {
        descriptorMap + (cmd.pos.line -> current_id)
      }
    }
  }

  def writePathDescriptors(prog: Prog, pathString: String): Unit = {
    val cmd = prog.cmd
    val descriptorMap = assignPathDescriptors(cmd, "main.", Map())
    val p = os.Path(pathString)
    os.write(p, upickle.default.write(descriptorMap))
  }

  def showDebug(ast: Prog, pass: String, c: Config): Unit =
    if c.passDebug then
      val top = ("=" * 15) + pass + ("=" * 15)
      println(top)
      println(Pretty.emitProg(ast)(c.logLevel == scribe.Level.Debug).trim)
      println("=" * top.length)

  def toBackend(str: BackendOption): fuselang.backend.Backend = str match
    case Vivado => backend.VivadoBackend
    case Cpp => backend.CppRunnable
    case Calyx => backend.calyx.CalyxBackend

  def checkStringWithError(prog: String, c: Config = emptyConf) =
    val preAst = Parser(prog).parse()

    c.pathDescriptorPath match {
      case Some(pathString) => {
//        val p = Paths.get(pathString)
        writePathDescriptors(preAst, pathString)
      }
      case None => {}
    }
    showDebug(preAst, "Original", c)

    // Run pre transformers if lowering is enabled
    val ast = if c.enableLowering then
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
    else
      preAst
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

  def codegen(ast: Prog, c: Config = emptyConf) =
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

  // Outputs red text to the console
  def red(txt: String): String =
    Console.RED + txt + Console.RESET

  def compileString(prog: String, c: Config): Either[String, String] =
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

  def compileStringToFile(
      prog: String,
      c: Config,
      out: String
  ): Either[String, Path] =

    compileString(prog, c).map(p => {
      Files.write(
        Paths.get(out),
        p.toCharArray.map(_.toByte),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.WRITE
      )
    })
