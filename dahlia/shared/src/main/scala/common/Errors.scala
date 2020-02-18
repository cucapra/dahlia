package fuselang.common

import Syntax._
import MultiSet._
import scala.util.parsing.input.Position

object Errors {

  class DahliaError(
    msg: String,
    ctx: String,
    pos: Position = null,
    postMsg: String = "")
    extends RuntimeException(msg) {
      def getMsg = msg
      def getCtx = ctx
      def getPos = pos
      def getPostMsg = postMsg
    }

  def alreadyConsumedError(
    id: Id,
    bank: Int,
    origRes: Int,
    conLocs: MultiSet[Position],
    trace: List[String]) = {
      val prevCons = conLocs.setMap.toList.dropRight(1).map({
        case (loc, counts) =>
          s"\n[${loc.line}.${loc.column}] Required $counts resource(s):\n${loc.longString}"
      }).mkString("")

      s"""
      |`${id}' originally had ${origRes} resource(s).
      |
      |Previous locations that consumed bank $bank:
      |${prevCons}
      |
      |Last gadget trace was:
      |${trace.mkString("\n")}
      """.stripMargin.trim
  }

  @deprecated("MsgErrors are not informative. Either create a new Error case or reuse one of the exisiting ones", "fuse 0.0.1")
  case class MsgError(msg: String, pos: Position)(implicit ctx: String)
    extends DahliaError(msg, ctx, pos)

  // Type mismatch
  case class UnexpectedType(
    pos: Position,
    construct: String,
    exp: String,
    actual: Type
  )(implicit ctx: String) extends DahliaError(
    s"Expected type $exp in $construct, received: $actual.", ctx, pos
  )

  case class UnexpectedSubtype(
    pos: Position,
    con: String,
    exp: Type,
    actual: Type
  )(implicit ctx: String) extends DahliaError(
    s"Expected subtype of $exp in $con, received: $actual.", ctx, pos
  )

  case class ArgLengthMismatch(
    pos: Position,
    exp: Int,
    actual: Int
  )(implicit ctx: String) extends DahliaError(
    s"Application expected $exp arguments, received $actual.", ctx, pos)

  // Return statements
  case class ReturnNotInFunc(pos: Position)(implicit ctx: String) extends DahliaError(
    s"Return statements are only allowed in fucntions.", ctx, pos)

  // Reduce operators
  case class ReduceInsideUnroll(op: ROp, pos: Position)(implicit ctx: String) extends DahliaError(
    s"$op cannot be inside an unrolled loop", ctx, pos)

  case class FuncInUnroll(pos: Position)(implicit ctx: String) extends DahliaError(
    "Cannot call function inside unrolled loop.", ctx, pos)

  // Unrolling and banking errors
  case class UnrollRangeError(pos: Position, rSize: Int, uFactor: Int)(implicit ctx: String) extends DahliaError(
    s"Cannot unroll range of size $rSize by factor $uFactor.", ctx, pos)

  case class IndexOutOfBounds(id : Id, size: Int, mv: Int, pos: Position)(implicit ctx: String) extends DahliaError(
    s"Index out of bounds for `$id'. Memory size is $size, iterator max val is $mv", ctx, pos)

  case class IncorrectAccessDims(id: Id, exp: Int, actual: Int)(implicit ctx: String) extends DahliaError(
    s"Incorrect number of dimensions used to access `$id'. Expected: $exp, actual: $actual.", ctx, id.pos)

  case class UnknownDim(id: Id, dim: Int)(implicit pos: Position, ctx: String) extends DahliaError(
    s"`$id' does not have dimension $dim", ctx, pos)

  case class UnknownBank(id: Id, bank: Int)(implicit pos: Position, ctx: String) extends DahliaError(
    s"Physical resource `$id' does not have bank $bank.", ctx, pos)

  case class BankUnrollInvalid(arrId: Id, bf: Int, uf: Int)(implicit pos: Position, ctx: String) extends DahliaError(
    s"Invalid parallel access on `$arrId`. Banking factor ($bf) does not divide unrolling factor ($uf). ", ctx, pos)

  case class AlreadyConsumed(
    id: Id,
    bank: Int,
    origRes: Int,
    conLocs: MultiSet[Position]
  )(implicit ctx: String, pos: Position, trace: List[String])
  extends DahliaError(
    s"Bank $bank for physical resource `$id' already consumed.",
    ctx,
    pos,
    alreadyConsumedError(id, bank, origRes, conLocs, trace)
  )

  case class InvalidDynamicIndex(id:Id, bf:Int)(implicit ctx: String) extends DahliaError(
    s"Dynamic access of array `$id' requires unbanked dimension. Actual banking factor: $bf. Use a shrink view to create unbanked array.", ctx, id.pos)

  // Invalid Capability error
  case class AlreadyWrite(e: Expr)(implicit ctx: String) extends DahliaError(
    "Already written to this expression in this context.", ctx, e.pos)
  case class InsufficientResourcesInUnrollContext(exp: Int, ac: Int, expr: Expr)(implicit ctx: String)
    extends DahliaError(s"Array access implies $ac safe writes are possible, but surrounding context requires $exp copies.", ctx, expr.pos)

  // Pipelining error
  case class PipelineError(pos: Position)(implicit ctx: String) extends DahliaError(
    "Pipelining is only allowed on non-sequenced loops.", ctx, pos)

  // Subtyping error
  case class NoJoin(pos: Position, cons: String, t1: Type, t2: Type)(implicit ctx: String) extends DahliaError(
    s"$t1 and $t2 are incomparable. Cannot create a join for construct $cons.", ctx, pos)

  // Operation errors
  case class BinopError(op: BOp, exp: String, t1: Type, t2: Type)(implicit ctx: String) extends DahliaError(
    s"$op expected $exp, received: $t1 and $t2.", ctx, op.pos)

  // Binding errors
  case class Unbound(id: Id)(implicit ctx: String) extends DahliaError(
    s"`$id' is not bound in scope.", ctx, id.pos)

  case class AlreadyBound(id: Id)(implicit ctx: String) extends DahliaError(
    s"`$id' already bound in scope", ctx, id.pos)

  // Binding errors
  case class LoopDepSequential(id: Id)(implicit ctx: String) extends DahliaError(
    s"`$id' cannot be used and then defined in an unrolled loop.", ctx, id.pos)

  case class LoopDepMerge(id: Id)(implicit ctx: String) extends DahliaError(
    s"`$id' cannot be merged with conflicting state. The execution order is non-deterministic.", ctx, id.pos)

  case class LoopDynamicAccess(expr: Expr, used: Id)(implicit ctx: String) extends DahliaError(
    s"Access depends on a loop iteration variable",
    ctx,
    expr.pos,
    s"\n[${used.pos.line}.${used.pos.column}] Last update was here:\n${used.pos.longString}"
  )

  // View errors
  case class InvalidShrinkWidth(pos: Position, bf: Int, width: Int)(implicit ctx: String) extends DahliaError(
    s"Invalid shrinking factor for view. Expected factor of $bf (banking factor), received: $width", ctx, pos)
  case class InvalidAlignFactor(pos: Position, msg: String)(implicit ctx: String) extends DahliaError(
    msg, ctx, pos)
  case class ViewInsideUnroll(pos: Position)(implicit ctx: String) extends DahliaError(
    s"Cannot create view inside an unrolled context.", ctx, pos)
  case class InvalidSplitFactor(id: Id, arrId: Id, split: Int, bank: Int, dim: Int)(implicit ctx: String) extends DahliaError(
    s"Cannot create split view $id: split factor $split does not divide banking factor $bank in dimension $dim for $arrId", ctx, id.pos)

  // Record Errors
  case class UnknownRecordField(pos: Position, recType: Id, field: Id)(implicit ctx: String) extends DahliaError(
    s"Record type `$recType' has no field named `$field'.", ctx, pos)
  case class MissingField(pos: Position, recType: Id, field: Id)(implicit ctx: String) extends DahliaError(
    s"Record literal of type `$recType' missing field `$field'", ctx, pos)
  case class ExtraField(pos: Position, recType: Id, field: Id)(implicit ctx: String) extends DahliaError(
    s"Record literal of type `$recType' has an extra field `$field'", ctx, pos)

  // Array errors
  case class LiteralLengthMismatch(pos: Position, expLen: Int, acLen: Int)(implicit ctx: String) extends DahliaError(
    s"Given type requires $expLen elements but literals has $acLen elements.", ctx, pos)

  // Parsing errors
  case class ParserError(msg: String) extends DahliaError(msg, "Parser")

  // Malformed AST Errors
  case class UnexpectedLVal(e: Expr, construct: String)(implicit ctx: String) extends DahliaError(
    s"Expected L-value in $construct.", ctx, e.pos)
  case class ArrayInRecord(name: Id, field: Id, typ: Type)(implicit ctx: String) extends DahliaError(
    s"Records can only contain primitive types and other structs. Found field $field with $typ in record definition for `$name'.", ctx, field.pos)
  case class MalformedType(msg: String)(implicit ctx: String) extends DahliaError(msg, ctx)
  case class NotInBinder(pos: Position, construct: String)(implicit ctx: String) extends DahliaError(
    s"$construct can only be bound by `let'. Found in context:", ctx, pos)
  case class ExplicitTypeMissing(pos: Position, litType: String, id: Id)(implicit ctx: String) extends DahliaError(
    s"$litType requires explict types. Missing type for `$id'.", ctx, pos)
  case class Unsupported(pos: Position, construct: String)(implicit ctx: String) extends DahliaError(
    s"$construct are not supported.", ctx, pos)
}

object CompilerError {

  // Errors generated by backends.
  case class BackendError(msg: String) extends RuntimeException(msg)

  // Errors generated by fuse CLI
  case class HeaderMissing(hdr: String, hdrLoc: String) extends RuntimeException(
    s"Header $hdr is missing from location $hdrLoc.")

  // Used when a branch should be impossible at runtime.
  case class Impossible(msg: String)
                       (implicit func: sourcecode.Enclosing,
                         line: sourcecode.Line)
                       extends RuntimeException(s"[$func:$line] $msg")

  // Used when a feature is not yet implemented
  case class NotImplemented(msg: String) extends RuntimeException(s"$msg This feature is not yet implemented. Please open a feature request for it.")
}
