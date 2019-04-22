package fuselang

import scala.math.log10

object CodeGenHelpers {
  import Syntax._

  implicit class RichExpr(e1: Expr) {
    def +(e2: Expr) =
      binop(OpAdd(), e1, e2)

    def *(e2: Expr) =
      binop(OpMul(), e1, e2)

    def /(e2: Expr) =
      binop(OpDiv(), e1, e2)

    def >>(e2: Expr) =
      binop(OpRsh(), e1, e2)
  }

  // Using the trick defined here: https://www.geeksforgeeks.org/program-to-find-whether-a-no-is-power-of-two/
  def isPowerOfTwo(x: Int) =
    x != 0 && ((x & (x - 1)) == 0)

  def fastDiv(l: Expr, r: Expr) = r match {
    case EInt(n, _) => if (isPowerOfTwo(n)) {
      l >> EInt((log10(n) / log10(2)).toInt, 10)
    } else {
      scribe.warn(s"Failed to generate fast division for factor $n")
      l / r
    }
    case e => {
      scribe.warn(s"Cannot generate fast division for dynamic expression $e")
      l / r
    }
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
