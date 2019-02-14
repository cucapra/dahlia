package fuselang

object CodeGenHelpers {
  import Syntax._

  implicit class RichExpr(e1: Expr) {
    def +(e2: Expr) =
      binop(OpAdd(), e1, e2)

    def *(e2: Expr) =
      binop(OpMul(), e1, e2)
  }

  // Simple peephole optimization to turn: 1 * x => x, 0 + x => x, 0 * x => 0
  def binop(op: BOp, l: Expr, r: Expr) = (op, l, r) match {
    case (OpMul(), EInt(1), r) => r
    case (OpMul(), l, EInt(1)) => l
    case (OpMul(), EInt(0), _) => EInt(0)
    case (OpMul(), _, EInt(0)) => EInt(0)
    case (OpAdd(), l, EInt(0)) => l
    case (OpAdd(), EInt(0), r) => r
    case _ => EBinop(op, l, r)
  }

}
