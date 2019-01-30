package fuselang

object TypeErrors {
  import Syntax._

  sealed trait TypeError extends RuntimeException
  case class MsgError(msg: String) extends TypeError
  case class UnexpectedType(construct: String, exp: Type, actual: Type) extends TypeError {
    override def toString = s"Expected type $exp in $construct, received: $actual."
  }
  case class UnexpectedSubtype(construct: String, exp: Type, actual: Type) extends TypeError {
    override def toString = s"Expected subtype of $exp in $construct, received: $actual."
  }
  case class BankUnrollError(bf: Int, uf: Int) extends TypeError
  case class UnrollRangeError(rangeSize: Int, unrollFactor: Int) extends TypeError {
    override def toString = s"Cannot unroll range of size $rangeSize by factor $unrollFactor."
  }

}

