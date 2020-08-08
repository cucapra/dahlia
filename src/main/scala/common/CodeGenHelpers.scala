package fuselang.common

import scala.math.log10

object CodeGenHelpers {
  import Syntax._

  implicit class RichExpr(e1: Expr) {
    import Syntax.{OpConstructor => OC}

    def +(e2: Expr) =
      binop(NumOp("+", OC.add), e1, e2)

    def *(e2: Expr) =
      binop(NumOp("*", OC.mul), e1, e2)

    def div(e2: Expr) =
      binop(NumOp("/", OC.div), e1, e2)

    def /(e2: Expr) = fastDiv(e1, e2)

    def <<(e2: Expr) =
      binop(BitOp("<<"), e1, e2)

    def >>(e2: Expr) =
      binop(BitOp(">>"), e1, e2)

    def mod(e2: Expr) =
      binop(NumOp("%", OC.mod), e1, e2)

    def %(e2: Expr) = fastMod(e1, e2)

    def &(e2: Expr) =
      binop(BitOp("&"), e1, e2)
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
    case EInt(n, _) if (isPowerOfTwo(n)) => l & EInt(n - 1, 10)
    case e => {
      scribe.warn(s"Cannot generate fast modulus for dynamic expression $e")
      l mod r
    }
  }

  // Simple peephole optimization to turn: 1 * x => x, 0 + x => x, 0 * x => 0
  def binop(op: BOp, l: Expr, r: Expr) = (op, l, r) match {
    case (NumOp("*", _), EInt(1, _), r) => r
    case (NumOp("*", _), l, EInt(1, _)) => l
    case (NumOp("*", _), EInt(0, b), _) => EInt(0, b)
    case (NumOp("*", _), _, EInt(0, b)) => EInt(0, b)
    case (NumOp("+", _), l, EInt(0, _)) => l
    case (NumOp("+", _), EInt(0, _), r) => r
    case _ => EBinop(op, l, r)
  }

}
