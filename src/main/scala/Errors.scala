package fuselang

object Errors {
  import Syntax._
  import scala.util.parsing.input.Position

  def withPos(s: String, pos: Position) = s"[${pos.line}.${pos.column}] $s\n${pos.longString}"

  case class MsgError(msg: String) extends RuntimeException(msg)

  // Type mismatch
  case class UnexpectedType(construct: String, exp: Type, actual: Type) extends RuntimeException(
    s"Expected type $exp in $construct, received: $actual."
  )
  case class UnexpectedTypeWithString(construct: String, exp: String, actual: Type) extends RuntimeException(
    s"Expected type $exp in $construct, received: $actual."
  )
  case class UnexpectedSubtype(construct: String, exp: Type, actual: Type) extends RuntimeException(
    s"Expected subtype of $exp in $construct, received: $actual."
  )

  // Unrolling and banking errors
  case class UnrollRangeError(rangeSize: Int, unrollFactor: Int) extends RuntimeException(
    s"Cannot unroll range of size $rangeSize by factor $unrollFactor."
  )
  case class InvalidIndex(id: Id, actual: Type) extends RuntimeException(
    withPos(s"Invalid indexing type for $id. Expected: TIndex or TSizedInt, actual: $actual", id.pos)
  )
  case class IncorrectAccessDims(id: Id, exp: Int, actual: Int) extends RuntimeException(
    withPos(s"Incorrect number of dimensions used to access $id. Expected: $exp, actual: $actual.", id.pos)
  )
  case class UnknownDim(id: Id, dim: Int) extends RuntimeException(
    withPos(s"$id does not have dimension $dim", id.pos)
  )
  case class BankUnrollInvalid(bf: Int, uf: Int) extends RuntimeException(
    s"Banking factor ($bf) not equal to ($uf). Use `shrink` for `flex` to create view with a smaller banking factor."
  )


  // Subtyping error
  case class NoJoin(t1: Type, t2: Type) extends RuntimeException(
    s"$t1 and $t2 are incomparable. Cannot create a join.")

  // Operation errors
  case class BinopError(op: Op2, t1: Type, t2: Type) extends RuntimeException(
    withPos(s"$op expected integers or floats, received: $t1 and $t2.", op.pos)
  )

  // Binding errors
  case class UnboundVar(id: Id) extends RuntimeException(
    withPos(s"Variable $id not defined.", id.pos)
  )
  case class AlreadyBound(id: Id) extends RuntimeException(
    withPos(s"Variable $id already bound in scope at ${id}.", id.pos)
  )

  // Reduction errors
  case class ReductionInvalidRHS(p: Position, rop: ROp, t: Type) extends RuntimeException(
    withPos(
      s"Expected right hand side of $rop to be a fully banked 1-dimensional array, received: $t", p)
  )

  // Parsing errors
  case class ParserError(msg: String) extends RuntimeException(msg)

}

