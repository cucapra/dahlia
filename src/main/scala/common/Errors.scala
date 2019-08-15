package fuselang.common

import Syntax._
import scala.util.parsing.input.Position

object Errors {

  def withPos(s: String, pos: Position, postMsg: String = "") =
    s"[${pos.line}.${pos.column}] $s\n${pos.longString}\n${postMsg}"

  class TypeError(msg: String,
                  pos: Position,
                  postMsg: String) extends RuntimeException(withPos(msg, pos, postMsg)) {
    def this(msg: String, pos: Position) = this(msg, pos, "")
  }

  @deprecated("MsgErrors are not informative. Either create a new Error case or reuse one of the exisiting ones", "fuse 0.0.1")
  case class MsgError(msg: String, pos: Position) extends TypeError(msg, pos)

  // Type mismatch
  case class UnexpectedType(pos: Position, construct: String, exp: String, actual: Type) extends TypeError(
    s"Expected type $exp in $construct, received: $actual.", pos)
  case class UnexpectedSubtype(pos: Position, con: String, exp: Type, actual: Type) extends TypeError(
    s"Expected subtype of $exp in $con, received: $actual.", pos)
  case class ArgLengthMismatch(pos: Position, exp: Int, actual: Int) extends TypeError(
    s"Application expected $exp arguments, received $actual.", pos)

  // Return statements
  case class ReturnNotInFunc(pos: Position) extends TypeError(
    s"Return statements are only allowed in fucntions.", pos)

  // Unrolling and banking errors
  case class UnrollRangeError(pos: Position, rSize: Int, uFactor: Int) extends TypeError(
    s"Cannot unroll range of size $rSize by factor $uFactor.", pos)

  case class IndexOutOfBounds(id : Id) extends TypeError(
    s"Index out of bounds for `$id'", id.pos)

  case class IncorrectAccessDims(id: Id, exp: Int, actual: Int) extends TypeError(
    s"Incorrect number of dimensions used to access `$id'. Expected: $exp, actual: $actual.", id.pos)

  case class UnknownDim(id: Id, dim: Int)(implicit pos: Position) extends TypeError(
    s"`$id' does not have dimension $dim", pos)

  case class UnknownBank(id: Id, bank: Int, dim: Int)(implicit pos: Position) extends TypeError(
    s"`$id' does not have bank $bank in dimension $dim.", pos)

  case class BankUnrollInvalid(arrId: Id, bf: Int, uf: Int)(implicit pos: Position) extends TypeError(
    s"Invalid parallel access on `$arrId`. Banking factor ($bf) not equal to unrolling factor ($uf). Create a shrink view `view v_$arrId = $arrId[_ : bank $uf]' and use it instead.", pos)

  case class AlreadyConsumed(id: Id, dim: Int, bank: Int, origLoc: Position)
                            (implicit pos: Position) extends TypeError(
    s"Bank $bank in dimension ${dim + 1} of physical resource `$id' already consumed.", pos,
    s"\n[${origLoc.line}.${origLoc.column}] Last consume happened here:\n${origLoc.longString}")

  case class InvalidDynamicIndex(id:Id, bf:Int) extends TypeError(
    s"Dynamic access of array `$id' requires unbanked dimension. Actual banking factor: $bf. Use a shrink view to create unbanked array.", id.pos)

  // Invalid Capability error
  case class InvalidCap(expr: Expr, exp: Capability, actual: Capability) extends TypeError(
    s"This expression requires $exp capability, but previous usage inferred $actual capability.", expr.pos)
  case class AlreadyWrite(e: Expr) extends TypeError(
    "Already written to this expression in this context.", e.pos)
  case class InsufficientResourcesInUnrollContext(exp: Int, ac: Int, expr: Expr)
    extends TypeError(s"Array access implies $ac safe writes are possible, but surrounding context requires $exp copies.", expr.pos)

  // Pipelining error
  case class PipelineError(pos: Position) extends TypeError(
    "Pipelining is only allowed on non-sequenced loops.", pos)

  // Subtyping error
  case class NoJoin(pos: Position, cons: String, t1: Type, t2: Type) extends TypeError(
    s"$t1 and $t2 are incomparable. Cannot create a join for construct $cons.", pos)

  // Operation errors
  case class BinopError(op: BOp, exp: String, t1: Type, t2: Type) extends TypeError(
    s"$op expected $exp, received: $t1 and $t2.", op.pos)

  // Binding errors
  case class Unbound(id: Id) extends TypeError(
    s"`$id' is not bound in scope.", id.pos)

  case class AlreadyBound(id: Id) extends TypeError(
    s"`$id' already bound in scope", id.pos)

  // View errors
  case class InvalidShrinkWidth(pos: Position, bf: Int, width: Int) extends TypeError(
    s"Invalid shrinking factor for view. Expected factor of $bf (banking factor), received: $width", pos)
  case class InvalidAlignFactor(pos: Position, afac: Int, bank: Int) extends TypeError(
    s"Aligned suffix factor $afac is not a multiple of the banking factor $bank.", pos)
  case class ViewInsideUnroll(pos: Position, arrId: Id) extends TypeError(
    s"Cannot create view for $arrId inside an unrolled context.", pos)
  case class InvalidSplitFactor(id: Id, arrId: Id, split: Int, bank: Int, dim: Int) extends TypeError(
    s"Cannot create split view $id: split factor $split does not divide banking factor $bank in dimension $dim for $arrId", id.pos)

  // Record Errors
  case class UnknownRecordField(pos: Position, recType: Id, field: Id) extends TypeError(
    s"Record type `$recType' has no field named `$field'.", pos)
  case class MissingField(pos: Position, recType: Id, field: Id) extends TypeError(
    s"Record literal of type `$recType' missing field `$field'", pos)
  case class ExtraField(pos: Position, recType: Id, field: Id) extends TypeError(
    s"Record literal of type `$recType' has an extra field `$field'", pos)

  // Array errors
  case class LiteralLengthMismatch(pos: Position, expLen: Int, acLen: Int) extends TypeError(
    s"Given type requires $expLen elements but literals has $acLen elements.", pos)

  // Parsing errors
  case class ParserError(msg: String) extends RuntimeException(msg)

  // Malformed AST Errors
  case class UnexpectedLVal(e: Expr, construct: String) extends RuntimeException(
    withPos(s"Expected L-value in $construct.", e.pos))
  case class ArrayInRecord(name: Id, field: Id, typ: Type) extends RuntimeException(
    withPos(s"Records can only contain primitive types and other structs. Found field $field with $typ in record definition for `$name'.", field.pos))
  case class MalformedType(msg: String) extends RuntimeException(msg)
  case class NotInBinder(pos: Position, construct: String) extends RuntimeException(
    withPos(s"$construct can only be bound by `let'. Found in context:", pos))
  case class ExplicitTypeMissing(pos: Position, litType: String, id: Id) extends RuntimeException(
    withPos(s"$litType requires explict types. Missing type for `$id'.", pos))
  case class Unsupported(pos: Position, construct: String) extends RuntimeException(
    withPos(s"$construct are not supported.", pos))
}

object CompilerError {

  // Errors generated by backends.
  case class BackendError(msg: String) extends RuntimeException(msg)

  // Errors generated by fuse CLI
  case class HeaderMissing(hdr: String, hdrLoc: String) extends RuntimeException(
    s"Header $hdr is missing from location $hdrLoc.")

  // Used when a branch should be impossible at runtime.
  case class Impossible(msg: String)
                       (implicit func: sourcecode.Enclosing)
                       extends RuntimeException(s"[$func] $msg")

  // Used when a feature is not yet implemented
  case class NotImplemented(msg: String) extends RuntimeException(s"$msg This feature is not yet implemented. Please open a feature request for it.")
}
