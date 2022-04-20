package fuselang.common

import Syntax._
import MultiSet._
import scala.util.parsing.input.Position

object Errors:

  def withPos(s: String, pos: Position, postMsg: String = ""): String =
    s"[Line ${pos.line}, Column ${pos.column}] $s\n${pos.longString}\n${postMsg}"

  class TypeError(msg: String) extends RuntimeException(msg):
    def this(msg: String, pos: Position, postMsg: String) =
      this(withPos(msg, pos, postMsg))
    def this(msg: String, pos: Position) = this(msg, pos, "")

  def alreadyConsumedError(
      id: Id,
      bank: Int,
      origRes: Int,
      conLocs: MultiSet[Position],
      pos: Position,
      trace: Seq[String]
  ): String =
    val prevCons = conLocs.setMap
      .dropRight(1)
      .map({
        case (loc, counts) =>
          s"\n[${loc.line}.${loc.column}] Required $counts resource(s):\n${loc.longString}"
      })
      .mkString("")

    s"""
      |[Line ${pos.line}, Column ${pos.column}] Bank $bank for physical resource `$id' already consumed.
      |${pos.longString}
      |
      |`${id}' originally had ${origRes} resource(s).
      |
      |Previous locations that consumed bank $bank:
      |${prevCons}
      |
      |Last gadget trace was:
      |${trace.mkString("\n")}
      """.stripMargin.trim

  /* @deprecated(
    "MsgErrors are not informative. Either create a new Error case or reuse one of the exisiting ones",
    "fuse 0.0.1"
  ) */
  case class MsgError(msg: String, pos: Position) extends TypeError(msg, pos)

  // Type mismatch
  case class UnexpectedType(
      pos: Position,
      construct: String,
      exp: String,
      actual: Type
  ) extends TypeError(
        s"Expected type $exp in $construct, received: $actual.",
        pos
      )
  case class UnexpectedSubtype(
      pos: Position,
      con: String,
      exp: Type,
      actual: Type
  ) extends TypeError(
        s"Expected subtype of $exp in $con, received: $actual.",
        pos
      )
  case class ArgLengthMismatch(pos: Position, exp: Int, actual: Int)
      extends TypeError(
        s"Application expected $exp arguments, received $actual.",
        pos
      )

  // Return statements
  case class ReturnNotInFunc(pos: Position)
      extends TypeError(
        s"Return statements are only allowed in fucntions.",
        pos
      )

  // Reduce operators
  case class ReduceInsideUnroll(op: ROp, pos: Position)
      extends TypeError(s"$op cannot be inside an unrolled loop", pos)

  case class FuncInUnroll(pos: Position)
      extends TypeError("Cannot call function inside unrolled loop.", pos)

  // Unrolling and banking errors
  case class UnrollRangeError(pos: Position, rSize: Int, uFactor: Int)
      extends TypeError(
        s"Cannot unroll range of size $rSize by factor $uFactor.",
        pos
      )

  case class IndexOutOfBounds(id: Id, size: Int, mv: Int, pos: Position)
      extends TypeError(
        s"Index out of bounds for `$id'. Memory size is $size, iterator max val is $mv",
        pos
      )

  case class IncorrectAccessDims(id: Id, exp: Int, actual: Int)
      extends TypeError(
        s"Incorrect number of dimensions used to access `$id'. Expected: $exp, actual: $actual.",
        id.pos
      )

  case class UnknownDim(id: Id, dim: Int)(implicit pos: Position)
      extends TypeError(s"`$id' does not have dimension $dim", pos)

  case class UnknownBank(id: Id, bank: Int)(implicit pos: Position)
      extends TypeError(
        s"Physical resource `$id' does not have bank $bank.",
        pos
      )

  case class BankUnrollInvalid(arrId: Id, bf: Int, uf: Int)(
      implicit pos: Position
  ) extends TypeError(
        s"Invalid parallel access on `$arrId`. Banking factor ($bf) does not divide unrolling factor ($uf). ",
        pos
      )

  case class AlreadyConsumed(
      id: Id,
      bank: Int,
      origRes: Int,
      conLocs: MultiSet[Position]
  )(implicit pos: Position, trace: Seq[String])
      extends TypeError(
        alreadyConsumedError(id, bank, origRes, conLocs, pos, trace)
      )

  case class InvalidDynamicIndex(id: Id, bf: Int)
      extends TypeError(
        s"Dynamic access of array `$id' requires unbanked dimension. Actual banking factor: $bf. Use a shrink view to create unbanked array.",
        id.pos
      )

  // Invalid Capability error
  case class AlreadyWrite(e: Expr)
      extends TypeError(
        "Already written to this expression in this context.",
        e.pos
      )
  case class InsufficientResourcesInUnrollContext(exp: Int, ac: Int, expr: Expr)
      extends TypeError(
        s"Array access implies $ac safe writes are possible, but surrounding context requires $exp copies.",
        expr.pos
      )

  // Pipelining error
  case class PipelineError(pos: Position)
      extends TypeError(
        "Pipelining is only allowed on non-sequenced loops.",
        pos
      )

  // Subtyping error
  case class NoJoin(pos: Position, cons: String, t1: Type, t2: Type)
      extends TypeError(
        s"$t1 and $t2 are incomparable. Cannot create a join for construct $cons.",
        pos
      )

  // Operation errors
  case class BinopError(op: BOp, exp: String, t1: Type, t2: Type)
      extends TypeError(s"$op expected $exp, received: $t1 and $t2.", op.pos)

  // Binding errors
  case class Unbound(id: Id)
      extends TypeError(s"`$id' is not bound in scope.", id.pos)

  case class AlreadyBound(id: Id)
      extends TypeError(s"`$id' already bound in scope", id.pos)

  // Binding errors
  case class LoopDepSequential(id: Id)
      extends TypeError(
        s"`$id' cannot be used and then defined in an unrolled loop.",
        id.pos
      )

  case class LoopDepMerge(id: Id)
      extends TypeError(
        s"`$id' cannot be merged with conflicting state. The execution order is non-deterministic.",
        id.pos
      )

  case class LoopDynamicAccess(expr: Expr, used: Id)
      extends TypeError(
        s"Access depends on a loop iteration variable",
        expr.pos,
        s"\n[Line ${used.pos.line}, Column ${used.pos.column}] Last update was here:\n${used.pos.longString}"
      )

  // View errors
  case class InvalidShrinkWidth(pos: Position, bf: Int, width: Int)
      extends TypeError(
        s"Invalid shrinking factor for view. Expected factor of $bf (banking factor), received: $width",
        pos
      )
  case class InvalidAlignFactor(pos: Position, msg: String)
      extends TypeError(msg, pos)
  case class ViewInsideUnroll(pos: Position)
      extends TypeError(s"Cannot create view inside an unrolled context.", pos)
  case class InvalidSplitFactor(
      id: Id,
      arrId: Id,
      split: Int,
      bank: Int,
      dim: Int
  ) extends TypeError(
        s"Cannot create split view $id: split factor $split does not divide banking factor $bank in dimension $dim for $arrId",
        id.pos
      )

  // Record Errors
  case class UnknownRecordField(pos: Position, recType: Id, field: Id)
      extends TypeError(
        s"Record type `$recType' has no field named `$field'.",
        pos
      )
  case class MissingField(pos: Position, recType: Id, field: Id)
      extends TypeError(
        s"Record literal of type `$recType' missing field `$field'",
        pos
      )
  case class ExtraField(pos: Position, recType: Id, field: Id)
      extends TypeError(
        s"Record literal of type `$recType' has an extra field `$field'",
        pos
      )

  // Array errors
  case class LiteralLengthMismatch(pos: Position, expLen: Int, acLen: Int)
      extends TypeError(
        s"Given type requires $expLen elements but literals has $acLen elements.",
        pos
      )

  // Parsing errors
  case class ParserError(msg: String) extends RuntimeException(msg)

  // Malformed AST Errors
  case class UnexpectedLVal(e: Expr, construct: String)
      extends RuntimeException(
        withPos(s"Expected L-value in $construct.", e.pos)
      )
  case class ArrayInRecord(name: Id, field: Id, typ: Type)
      extends RuntimeException(
        withPos(
          s"Records can only contain primitive types and other structs. Found field $field with $typ in record definition for `$name'.",
          field.pos
        )
      )
  case class MalformedType(msg: String) extends RuntimeException(msg)
  case class NotInBinder(pos: Position, construct: String)
      extends RuntimeException(
        withPos(
          s"$construct can only be bound by `let'. Found in context:",
          pos
        )
      )
  case class ExplicitTypeMissing(pos: Position, litType: String, id: Id)
      extends RuntimeException(
        withPos(
          s"$litType requires explict types. Missing type for `$id'.",
          pos
        )
      )
  case class Unsupported(pos: Position, construct: String)
      extends RuntimeException(withPos(s"$construct are not supported.", pos))
  case class Malformed(pos: Position, msg: String)
      extends RuntimeException(withPos(msg, pos))

object CompilerError:

  // Errors generated by a pass. Usually occur when an assumption is
  // violated.
  case class PassError(msg: String) extends RuntimeException(msg)
  object PassError:
    def apply(msg: String, pos: Position): PassError =
      this(Errors.withPos(msg, pos))

  // Errors generated by backends.
  case class BackendError(msg: String) extends RuntimeException(msg)
  object BackendError:
    def apply(msg: String, pos: Position): BackendError =
      this(Errors.withPos(msg, pos))

  // Errors generated by fuse CLI
  case class HeaderMissing(hdr: String, hdrLoc: String)
      extends RuntimeException(s"Header $hdr is missing from location $hdrLoc.")

  // Used when a branch should be impossible at runtime.
  case class Impossible(msg: String)(
      implicit func: sourcecode.Enclosing,
      line: sourcecode.Line
  ) extends RuntimeException(s"[$func:$line] $msg")
  object Impossible:
    def apply(msg: String, pos: Position)(
        implicit func: sourcecode.Enclosing,
        line: sourcecode.Line
    ): Impossible =
      this(Errors.withPos(msg, pos))

  // Used when a feature is not yet implemented
  case class NotImplemented(msg: String)
      extends RuntimeException(
        s"$msg This feature is not yet implemented. Please open a feature request for it."
      )

  object NotImplemented:
    def apply(msg: String, pos: Position): NotImplemented =
      this(Errors.withPos(msg, pos))
