package fuselang

import Syntax._
import Errors._
import CompilerError._
import Logger._
import Utils.RichOption

object BoundsChecker {

  // XXX(rachit): This is incorrect. It doesn't do bounds checking on function
  // bodies.
  def check(p: Prog) : Unit = {
    checkC(p.cmd)
  }

  // XXX(rachit): This is incorrect. It needs to recur down to exprs contained
  // in EBinop, EApp, etc.
  private def checkE(e: Expr): Unit = e match {
    case EArrAccess(id, idxs) =>
      id.typ
        .getOrThrow(Impossible(s"$id missing type in $e"))
        .matchOrError(id.pos, "array access", s"array type"){
          case TArray(_, dims) =>
            idxs
              .map(idx => idx -> idx.typ)
              .zip(dims)
              .foreach({
                case ((idx, t), (size, _)) => t.foreach({
                  case idxt@TSizedInt(n) => if (math.pow(2, n) >= size) {
                    scribe.warn(
                      (s"$idxt is used for an array access. This might be out of bounds at runtime.", idx))
                  }
                  case TStaticInt(v) => if (v >= size) throw IndexOutOfBounds(id)
                  case t@TIndex(_, _) => if (t.maxVal >= size) throw IndexOutOfBounds(id)
                  case t => throw UnexpectedType(id.pos, "array access", s"[$t]", t)
                })
              })
        }
    case _ => ()
  }

  /**
   * Given a view with a known prefix length, check if it **might** cause an
   * out of bound access when accessed.
   */
  private def checkView(arrLen: Int, viewId: Id, view: View) = {
    if (view.prefix.isDefined) {
      val View(suf, Some(pre), _) = view

      val (sufExpr, fac) = suf match {
        case Aligned(fac, e) => (e, fac)
        case Rotation(e) => (e, 1)
      }

      val maxVal: Int =
        sufExpr.typ
          .getOrThrow(Impossible(s"$sufExpr is missing type"))
          .matchOrError(viewId.pos, "view", "Integer Type"){
          case idx:TIndex => fac * idx.maxVal
          case TStaticInt(v) => fac * v
          case idx@TSizedInt(_) =>
            scribe.warn(
              (s"$idx is used to create view $viewId. This could be unsafe.", idx));
            1
        }

      if (maxVal + pre > arrLen) {
        throw IndexOutOfBounds(viewId)
      }
    }
  }

  private def checkC(c: Command): Unit = c match {
    case CPar(c1, c2) => checkC(c1); checkC(c2)
    case CSeq(c1, c2) => checkC(c1); checkC(c2)
    case CLet(_, _, exp) => exp.foreach(checkE(_))
    case CView(viewId, arrId, views) => {
      val typ = arrId.typ.getOrThrow(Impossible(s"$arrId is missing type in $c"))
      typ.matchOrError(c.pos, "view", "array type"){ case TArray(_, dims) =>
        views.zip(dims).foreach({ case (view, (len, _)) =>
          checkView(len, viewId, view)
        })
      }
    }
    case _:CSplit => ()
    case CIf(cond, tbranch, fbranch) => checkE(cond) ; checkC(tbranch) ; checkC(fbranch)
    case CFor(_, par, combine) => checkC(par) ; checkC(combine)
    case CWhile(cond, body) => checkE(cond) ; checkC(body)
    case CUpdate(lhs, rhs) => checkE(lhs) ; checkE(rhs)
    case CReduce(_, l, r) => checkE(l) ; checkE(r)
    case CExpr(e) => checkE(e)
    case CEmpty => ()
  }

}
