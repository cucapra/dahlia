package fuselang

object Syntax {

  type Id = String

  sealed trait Type {
    def :<(that: Type) : Boolean =  this == that
  }
  case object TBool extends Type
  case class TSizedInt(len: Int) extends Type
  case class TArray(typ: Type, dims: List[(Int, Int)]) extends Type
  case class TIndex(static: (Int, Int), dynamic: (Int, Int)) extends Type

  sealed trait Op
  case object opEq
  case object opAdd

  sealed trait Expr
  case class EInt(v: Int) extends Expr
  case class EBool(v: Boolean) extends Expr
  case class EBinop(op: Op, e1: Expr, e2: Expr) extends Expr
  case class EAA(id: Id, idxs: List[Expr]) extends Expr
  case class EVar(id: Id) extends Expr

  sealed trait Command
  case class CSeq(c1: Command, c2: Command) extends Command
  case class CLet(id: Id, e: Expr) extends Command
  case class CIf(cond: Expr, cons: Command) extends Command
  case class CFor(iter: Id, range: Range, unroll: Int, par: Command) extends Command
  case class CUpdate(lhs: Expr, rhs: Expr) extends Command
  case class CExpr(exp: Expr) extends Command
  case object CEmpty extends Command
}
