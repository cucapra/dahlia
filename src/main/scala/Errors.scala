package fuselang

object Errors {
  import Syntax._
  import scala.util.parsing.input.Position

  def withPos(s: String, pos: Option[Position]) =
    if (pos.isDefined) s"[${pos.get.line}.${pos.get.column}] $s\n${pos.get.longString}"
    else s

  class TypeError(msg: String, pos: Option[Position]) extends RuntimeException(withPos(msg, pos))

  case class MsgError(msg: String, pos: Option[Position] = None) extends TypeError(msg, pos)

  // Type mismatch
  case class UnexpectedType(pos: Position, con: String, exp: String, actual: Type) extends TypeError(
    s"Expected type $exp in $con, received: $actual.", Some(pos))

  case class UnexpectedSubtype(pos: Position, con: String, exp: Type, actual: Type) extends TypeError(
    s"Expected subtype of $exp in $con, received: $actual.", Some(pos))

  // Unrolling and banking errors
  case class UnrollRangeError(pos: Position, rSize: Int, uFactor: Int) extends TypeError(
    s"Cannot unroll range of size $rSize by factor $uFactor.", Some(pos))

  case class InvalidIndex(id: Id, actual: Type) extends TypeError(
    s"Invalid indexing type for $id. Expected: TIndex or TSizedInt, actual: $actual", Some(id.pos))

  case class IncorrectAccessDims(id: Id, exp: Int, actual: Int) extends TypeError(
    s"Incorrect number of dimensions used to access $id. Expected: $exp, actual: $actual.", Some(id.pos))

  case class UnknownDim(id: Id, dim: Int) extends TypeError(
    s"$id does not have dimension $dim", Some(id.pos))

  case class BankUnrollInvalid(bf: Int, uf: Int) extends TypeError(
    s"Banking factor ($bf) not equal to ($uf).", None)


  // Subtyping error
  case class NoJoin(t1: Type, t2: Type) extends TypeError(
    s"$t1 and $t2 are incomparable. Cannot create a join.", None)

  // Operation errors
  case class BinopError(op: BOp, t1: Type, t2: Type) extends TypeError(
    s"$op expected integers or floats, received: $t1 and $t2.", Some(op.pos))

  // Binding errors
  case class UnboundVar(id: Id) extends TypeError(
    s"Variable $id not defined.", Some(id.pos))

  case class AlreadyBound(id: Id) extends TypeError(
    s"Variable $id already bound in scope at ${id}.", Some(id.pos))

  // Reduction errors
  case class ReductionInvalidRHS(p: Position, rop: ROp, tl: Type, tr: Type) extends TypeError(
    s"Unexpected type on right hand side of $rop. Expected: $tl[N bank N], received: $tr", Some(p))

  // Parsing errors
  case class ParserError(msg: String) extends RuntimeException(msg)

  // Used when a branch should be impossible at runtime.
  case class Impossible(msg: String) extends RuntimeException(s"Impossible: $msg")

}

