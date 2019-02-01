package fuselang

object Errors {
  import Syntax._

  sealed trait TypeError extends Throwable
  case class MsgError(msg: String) extends TypeError {
    override def toString = msg
  }
  case class UnexpectedType(construct: String, exp: Type, actual: Type) extends TypeError {
    override def toString = s"Expected type $exp in $construct, received: $actual."
  }
  case class UnexpectedSubtype(construct: String, exp: Type, actual: Type) extends TypeError{
    override def toString = s"Expected subtype of $exp in $construct, received: $actual."
  }
  case class UnrollRangeError(rangeSize: Int, unrollFactor: Int) extends TypeError{
    override def toString = s"Cannot unroll range of size $rangeSize by factor $unrollFactor."
  }
  case class InvalidIndex(id: Id, actual: Type) extends TypeError{
    override def toString = s"Inoverride defid indexing type for $id. Expected: TIndex or TSizedInt, actual: $actual"
  }
  case class IncorrectAccessDims(id: Id, exp: Int, actual: Int) extends TypeError{
    override def toString = s"Incorrect number of dimensions used to access $id. Expected: $exp, actual: $actual."
  }
  case class NoJoin(t1: Type, t2: Type) extends TypeError{
    override def toString = s"$t1 and $t2 are incomparable. Cannot create a join."
  }

  case class ParserError(msg: String) extends Throwable {
    override def toString = msg
  }

}

