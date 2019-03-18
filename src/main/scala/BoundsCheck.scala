package fuselang

import Syntax._
import Errors._

object BoundsChecker {

  def check(p: Prog) : Unit = {
    checkC(p.cmd)
  }

  private def checkE(e: Expr) : Unit = {
    e match {
      case e@EArrAccess(id, idxs) => e.dims match {
        case Some(dims) => (idxs.map(_.typ) zip dims).foldLeft(())((_, ele) => ele match {
          case ((t, (size, _))) => t match {
            case Some(TSizedInt(n)) =>
              if (math.pow(2, n) >= size)
                println("Warning! A SizedInt is used for an array access! This could be unsafe.")
            case Some(TStaticInt(v)) => if (v >= size) throw IndexOutOfBounds(id)
            case Some(t@TIndex(_, _)) => if (t.maxVal > size) throw IndexOutOfBounds(id)
            case _ => ()
          }
        })
        case None => ()
      }
      case _ => ()
    }
  }

  private def checkC(c: Command) : Unit = c match {
    case CPar(c1, c2) => checkC(c1) ; checkC(c2)
    case CSeq(c1, c2) => checkC(c1) ; checkC(c2)
    case CLet(_, _, exp) => checkE(exp)
    case CView(_, _) => ()
    case CIf(cond, tbranch, fbranch) => checkE(cond) ; checkC(tbranch) ; checkC(fbranch)
    case CFor(_, par, combine) => checkC(par) ; checkC(combine)
    case CWhile(cond, body) => checkE(cond) ; checkC(body)
    case CUpdate(lhs, rhs) => checkE(lhs) ; checkE(rhs)
    case CReduce(_, l, r) => checkE(l) ; checkE(r)
    case CExpr(e) => checkE(e)
    case CEmpty => ()
  }

}
