package fuselang

import scala.math.log10

object CodeGenHelpers {
  import Syntax._

  implicit class RichExpr(e1: Expr) {
    def +(e2: Expr) =
      binop(OpAdd(), e1, e2)

    def *(e2: Expr) =
      binop(OpMul(), e1, e2)

    def div(e2: Expr) =
      binop(OpDiv(), e1, e2)

    def /(e2: Expr) = fastDiv(e1, e2)

    def >>(e2: Expr) =
      binop(OpRsh(), e1, e2)

    def mod(e2: Expr) =
      binop(OpMod(), e1, e2)

    def %(e2: Expr) = fastMod(e1, e2)

    def &(e2: Expr) =
      binop(OpBAnd(), e1, e2)
  }

  // Using the trick defined here: https://www.geeksforgeeks.org/program-to-find-whether-a-no-is-power-of-two/
  def isPowerOfTwo(x: Int) =
    x != 0 && ((x & (x - 1)) == 0)

  def log2(n: Int) = log10(n) / log10(2)

  def fastDiv(l: Expr, r: Expr) = r match {
    case EInt(n, _) if (isPowerOfTwo(n)) => l >> EInt(log2(n).toInt, 10)
    case e => {
      scribe.warn(s"Cannot generate fast division for dynamic expression $e")
      l div r
    }
  }

  // Using the trick defined here: http://mziccard.me/2015/05/08/modulo-and-division-vs-bitwise-operations/
  def fastMod(l: Expr, r: Expr) = r match {
    case EInt(n, _) if (isPowerOfTwo(n)) => l & EInt(log2(n).toInt - 1, 10)
    case e => {
      scribe.warn(s"Cannot generate fast division for dynamic expression $e")
      l mod r
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
