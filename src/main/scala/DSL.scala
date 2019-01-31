package fuselang

object FuseLangDSL {

  import scala.language.implicitConversions
  import Syntax._

  implicit class RichExpr(e: Expr) {
    def +(e2: Expr) = EBinop(OpAdd, e, e2)
    def <(e2: Expr) = EBinop(OpEq, e, e2)
  }

  implicit class RichCommand(c: Command) {
    def |(c2: Command) = CSeq(c, c2)
  }

  implicit class RichId(id: Id) {
    def :=(e: Expr) = CLet(id, e)
    def <=(e: Expr) = CUpdate(EVar(id), e)
    def apply(es: List[Expr]) = EAA(id, es)
  }

  implicit class RichInt(s: Int) {
    def upto(e: Int) = CRange(s, e, 1)
  }

  implicit class RichCRange(r: CRange) {
    def unroll(u: Int) = r match {
      case CRange(s, e, 1) => CRange(s, e, u)
      case _ => throw new RuntimeException(s"$r already unrolled.")
    }
  }

  implicit def exprToCommand(e: Expr) = CExpr(e)
  implicit def intToExpr(i: Int) = EInt(i)
  implicit def boolToExpr(b: Boolean) = EBool(b)
  implicit def idToExpr(id: Id) = EVar(id)

  def For(iter: Id, range: CRange)(par: Command) = CFor(iter, range, par)

  def If(cond: Expr)(cons: Command) = CIf(cond, cons)

}
