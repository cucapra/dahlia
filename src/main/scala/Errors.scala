package fuselang

object TypeErrors {
  import Syntax._

  case class MsgError(msg: String) extends RuntimeException(msg)
  case class UnexpectedType(construct: String, exp: Type, actual: Type) extends RuntimeException(
    s"Expected type $exp in $construct, received: $actual."
  )
  case class UnexpectedSubtype(construct: String, exp: Type, actual: Type) extends RuntimeException(
    s"Expected subtype of $exp in $construct, received: $actual."
  )
  case class UnrollRangeError(rangeSize: Int, unrollFactor: Int) extends RuntimeException(
    s"Cannot unroll range of size $rangeSize by factor $unrollFactor."
  )
  case class InvalidIndex(id: Id, actual: Type) extends RuntimeException(
    s"Invalid indexing type for $id. Expected: TIndex or TSizedInt, actual: $actual"
  )
  case class BankUnrollError(bf: Int, uf: Int) extends RuntimeException
  case class IncorrectAccessDims(id: Id, exp: Int, actual: Int) extends RuntimeException(
    s"Incorrect number of dimensions used to access $id. Expected: $exp, actual: $actual."
  )
  case class NoJoin(t1: Type, t2: Type) extends RuntimeException(
    s"$t1 and $t2 are incomparable. Cannot create a join."
  )

}

