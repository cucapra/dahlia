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
    case (OpMul(), EInt(1, _), r) => r
    case (OpMul(), l, EInt(1, _)) => l
    case (OpMul(), EInt(0, b), _) => EInt(0, b)
    case (OpMul(), _, EInt(0, b)) => EInt(0, b)
    case (OpAdd(), l, EInt(0, _)) => l
    case (OpAdd(), EInt(0, _), r) => r
    case _ => EBinop(op, l, r)
  }

}
