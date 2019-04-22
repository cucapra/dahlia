package fuselang

import Syntax._
import Errors._
import Logger._

object BoundsChecker {

  def check(p: Prog) : Unit = {
    checkC(p.cmd)
  }

  private def checkE(e: Expr) : Unit = e match {
    case EArrAccess(id, idxs) => id.typ.map({
      case TArray(_, dims) => (idxs.map(_.typ) zip dims).foldLeft(())({
        case ((_, (t, (size, _)))) => t.map({
          case idx@TSizedInt(n) =>
            if (math.pow(2, n) >= size) {
              scribe.warn(
                (s"$idx is used for an array access. This could be unsafe.", e))
            }
          case TStaticInt(v) => if (v >= size) throw IndexOutOfBounds(id)
          case t@TIndex(_, _) => if (t.maxVal > size) throw IndexOutOfBounds(id)
          case t => throw UnexpectedType(id.pos, "array access", s"[$t]", t)
        }); ()
      })
      case t => throw UnexpectedType(id.pos, "array access", s"$t[]", t)
    }); ()
    case _ => ()
  }

  private def checkC(c: Command) : Unit = c match {
    case CPar(c1, c2) => checkC(c1) ; checkC(c2)
    case CSeq(c1, c2) => checkC(c1) ; checkC(c2)
    case CLet(_, _, exp) => checkE(exp)
    case _:CView | _:CSplit => ()
    case CIf(cond, tbranch, fbranch) => checkE(cond) ; checkC(tbranch) ; checkC(fbranch)
    case CFor(_, par, combine) => checkC(par) ; checkC(combine)
    case CWhile(cond, body) => checkE(cond) ; checkC(body)
    case CUpdate(lhs, rhs) => checkE(lhs) ; checkE(rhs)
    case CReduce(_, l, r) => checkE(l) ; checkE(r)
    case CExpr(e) => checkE(e)
    case CEmpty => ()
  }

}
