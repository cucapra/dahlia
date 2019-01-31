package fuselang

object Syntax {

  import TypeErrors._

  type Id = String

  sealed trait Type {
    def :<(that: Type): Boolean = (this, that) match {
      case (TStaticInt(_), TStaticInt(_)) => true
      case (TStaticInt(_), TSizedInt(_)) | (TSizedInt(_), TStaticInt(_)) => true
      case _ => this == that
    }
  }
  case object TBool extends Type {
    override def toString = "bool"
  }
  case class TSizedInt(len: Int) extends Type {
    override def toString = s"bit<$len>"
  }
  case class TStaticInt(v: Int) extends Type {
    override def toString = s"s($v)"
  }
  case class TArray(typ: Type, dims: List[(Int, Int)]) extends Type
  case class TIndex(static: (Int, Int), dynamic: (Int, Int)) extends Type

  sealed trait Op2
  case object OpEq extends Op2
  case object OpAdd extends Op2

  sealed trait Expr
  case class EInt(v: Int) extends Expr
  case class EBool(v: Boolean) extends Expr
  case class EBinop(op: Op2, e1: Expr, e2: Expr) extends Expr
  case class EAA(id: Id, idxs: List[Expr]) extends Expr
  case class EVar(id: Id) extends Expr

  case class CRange(s: Int, e: Int, u: Int) {
    def idxType: TIndex = {
      if ((e - s) % u != 0) {
        throw UnrollRangeError(e - s, u)
      } else {
        TIndex((0, u), (s/u, e/u))
      }
    }
  }

  sealed trait Command
  case class CSeq(c1: Command, c2: Command) extends Command
  case class CLet(id: Id, e: Expr) extends Command
  case class CIf(cond: Expr, cons: Command) extends Command
  case class CFor(iter: Id, range: CRange, par: Command) extends Command
  case class CUpdate(lhs: Expr, rhs: Expr) extends Command
  case class CExpr(exp: Expr) extends Command
  case class CDecl(id: Id, typ: Type) extends Command
  case object CRefreshBanks extends Command
  case object CEmpty extends Command
}
