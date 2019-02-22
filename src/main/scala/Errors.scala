package fuselang

object Errors {
  import Syntax._
  import TypeEnv.Capability
  import scala.util.parsing.input.Position

  def withPos(s: String, pos: Option[Position]) =
    if (pos.isDefined) s"[${pos.get.line}.${pos.get.column}] $s\n${pos.get.longString}"
    else s

  class TypeError(msg: String, pos: Option[Position]) extends RuntimeException(withPos(msg, pos)) {
    def this(msg: String) = this(msg, None)
    def this(msg: String, pos: Position) = this(msg, Some(pos))
  }

  case class MsgError(msg: String, pos: Option[Position] = None) extends TypeError(msg, pos)

  // Type mismatch
  case class UnexpectedType(pos: Position, con: String, exp: String, actual: Type) extends TypeError(
    s"Expected type $exp in $con, received: $actual.", pos)

  case class UnexpectedSubtype(pos: Position, con: String, exp: Type, actual: Type) extends TypeError(
    s"Expected subtype of $exp in $con, received: $actual.", pos)

  // Unrolling and banking errors
  case class UnrollRangeError(pos: Position, rSize: Int, uFactor: Int) extends TypeError(
    s"Cannot unroll range of size $rSize by factor $uFactor.", pos)

  case class InvalidIndex(id: Id, actual: Type) extends TypeError(
    s"Invalid indexing type for $id. Expected: TIndex or TSizedInt, actual: $actual", id.pos)

  case class IncorrectAccessDims(id: Id, exp: Int, actual: Int) extends TypeError(
    s"Incorrect number of dimensions used to access $id. Expected: $exp, actual: $actual.", id.pos)

  case class UnknownDim(id: Id, dim: Int) extends TypeError(
    s"$id does not have dimension $dim", id.pos)

  case class BankUnrollInvalid(bf: Int, uf: Int) extends TypeError(
    s"Banking factor ($bf) not equal to ($uf).")

  case class AlreadyConsumed(id: Id, dim: Int, bank: Int) extends TypeError(
    s"Bank $bank in dimension $dim of $id already consumed.")

  // Invalid Capability error
  case class InvalidCap(expr: Expr, exp: Capability, actual: Capability) extends TypeError(
    s"This expression requires $exp capability, but previous usage inferred $actual capability.", expr.pos)

  case class AlreadyWrite(e: Expr) extends TypeError(
    "Already written to this expression in this context.", e.pos)

  case class InsufficientResourcesInUnrollContext(exp: Int, ac: Int, expr: Expr)
    extends TypeError(s"Array access implies $ac safe writes are possible, but surrounding context requires $exp copies.", expr.pos)

  // Subtyping error
  case class NoJoin(t1: Type, t2: Type) extends TypeError(
    s"$t1 and $t2 are incomparable. Cannot create a join.")

  // Operation errors
  case class BinopError(op: BOp, t1: Type, t2: Type) extends TypeError(
    s"$op expected integers or floats, received: $t1 and $t2.", op.pos)

  // Binding errors
  case class UnboundVar(id: Id) extends TypeError(
    s"Variable $id not defined.", id.pos)

  case class AlreadyBound(id: Id) extends TypeError(
    s"Variable $id already bound in scope", id.pos)

  // Reduction errors
  case class ReductionInvalidRHS(p: Position, rop: ROp, tl: Type, tr: Type) extends TypeError(
    s"Unexpected type on right hand side of $rop. Expected: $tl[N bank N], received: $tr", p)

  // View errors
  case class InvalidShrinkWidth(pos: Position, bf: Int, width: Int) extends TypeError(
    s"Invalid width for shrink view. Expected factor of $bf (banking factor), received: $width", pos)
  case class ViewInsideUnroll(pos: Position, vt: ViewType, arrId: Id) extends TypeError(
    s"Cannot create $vt view for $arrId inside an unrolled context.", pos)

  // Type definition errors
  case class AlreadyBoundType(id: Id) extends TypeError(
    s"Type alias $id already bound in scope.", id.pos)

  case class UnboundType(id: Id) extends TypeError(
    s"Type alias $id not bound in scope.", id.pos)

  // Record Errors
  case class UnknownRecordField(pos: Position, recType: Id, field: Id) extends TypeError(
    s"Record type `$recType' has no field named `$field'.", pos)
  case class RecLiteralNotInBinder(pos: Position) extends TypeError(
    s"Record literal can only be bound by `let'. Found in context:", pos)
  case class ExplicitRecTypeMissing(pos: Position, id: Id) extends TypeError(
    s"Record literals require explict types. Missing type for `$id'.", pos)
  case class MissingField(pos: Position, recType: Id, field: Id) extends TypeError(
    s"Record literal of type `$recType' missing field `$field'", pos)
  case class ExtraField(pos: Position, recType: Id, field: Id) extends TypeError(
    s"Record literal of type `$recType' has an extra field `$field'", pos)

  // Parsing errors
  case class ParserError(msg: String) extends RuntimeException(msg)

  // Malformed AST Errors
  case class UnexpectedLVal(e: Expr, construct: String) extends RuntimeException(
    withPos(s"Expected L-value in $construct.", Some(e.pos)))
  case class MalformedShrink(vt: Shrink, w: Int, step: Int) extends RuntimeException(
    withPos(s"shrink view expects step size == width. Received $step (step), $w (width)", Some(vt.pos)))
  case class ArrayInRecord(name: Id, field: Id, typ: Type) extends RuntimeException(
    withPos(s"Records can only contain primitive types and other structs. Found field $field with $typ in record definition for `$name'.", Some(field.pos)))

  // Used when a branch should be impossible at runtime.
  case class Impossible(msg: String) extends RuntimeException(s"Impossible: $msg")

}

