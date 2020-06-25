package fuselang.passes

import scala.{PartialFunction => PF}

import fuselang.Utils.RichOption

import fuselang.common._
import Syntax._
import Errors._
import CompilerError._
import Logger._
import Checker._
import EnvHelpers._

object BoundsChecker {

  def check(p: Prog) = BCheck.check(p)

  // Bounds checker environment doesn't need to track any information. Empty
  // environment that just runs the commands.
  private case class BEnv() extends ScopeManager[BEnv] {
    // Neither environment contains anything.
    def merge(that: BEnv): BEnv = this
  }

  private final case object BCheck extends PartialChecker {

    type Env = BEnv

    val emptyEnv = BEnv()

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
            .matchOrError(viewId.pos, "view", "Integer Type") {
              case idx: TIndex => fac * idx.maxVal
              case TStaticInt(v) => fac * v
              case idx: TSizedInt =>
                scribe.warn(
                  (
                    s"$idx is used to create view $viewId. This could be unsafe.",
                    idx
                  )
                );
                1
            }

        if (maxVal + pre > arrLen) {
          throw IndexOutOfBounds(viewId, arrLen, maxVal + pre, viewId.pos)
        }
      }
    }

    override def myCheckE: PF[(Expr, Env), Env] = {
      case (EArrAccess(id, idxs), e) => {
        id.typ
          .getOrThrow(Impossible(s"$id missing type in $e"))
          .matchOrError(id.pos, "array access", s"array type") {
            case TArray(_, dims, _) =>
              idxs
                .map(idx => idx -> idx.typ)
                .zip(dims)
                .foreach({
                  case ((idx, t), (size, _)) =>
                    t.foreach({
                      case idxt @ TSizedInt(n, _) =>
                        if (math.pow(2, n) >= size) {
                          scribe.warn(
                            (
                              s"$idxt is used for an array access. " +
                                "This might be out of bounds at runtime.",
                              idx
                            )
                          )
                        }
                      case TStaticInt(v) =>
                        if (v >= size)
                          throw IndexOutOfBounds(id, size, v, idx.pos)
                      case t @ TIndex(_, _) =>
                        if (t.maxVal >= size)
                          throw IndexOutOfBounds(id, size, t.maxVal, idx.pos)
                      case t =>
                        throw UnexpectedType(id.pos, "array access", s"[$t]", t)
                    })
                })
          }
        e
      }
    }

    override def myCheckC: PF[(Command, Env), Env] = {
      case (c @ CView(viewId, arrId, views), e) => {
        val typ =
          arrId.typ.getOrThrow(Impossible(s"$arrId is missing type in $c"))
        typ.matchOrError(c.pos, "view", "array type") {
          case TArray(_, dims, _) =>
            views
              .zip(dims)
              .foreach({
                case (view, (len, _)) =>
                  checkView(len, viewId, view)
              })
        }
        e
      }
    }
  }
}
