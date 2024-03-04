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

  private case object BCheck extends PartialChecker {

    type Env = UnitEnv

    val emptyEnv = UnitEnv()

    /**
      * Given a view with a known prefix length, check if it **might** cause an
      * out of bound access when accessed.
      */
    private def checkView(arrLen: Int, viewId: Id, view: View) = {
      if view.prefix.isDefined then {
        val View(suf, Some(pre), _) = view : @unchecked

        val (sufExpr, fac) = suf match {
          case Aligned(fac, e) => (e, fac)
          case Rotation(e) => (e, 1)
        }

        val maxVal: BigInt =
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

        if maxVal + pre > arrLen then {
          throw IndexOutOfBounds(viewId, arrLen, maxVal + pre, viewId.pos)
        }
      }
    }

    def myCheckE: PF[(Expr, Env), Env] = {
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
                        if (math.pow(2, n) - 1) >= size then {
                          scribe.warn(
                            (
                              s"$idxt is used for an array access. " +
                                "This might be out of bounds at runtime.",
                              idx
                            )
                          )
                        }
                      case TStaticInt(v) =>
                        if v >= size then
                          throw IndexOutOfBounds(id, size, v, idx.pos)
                      case t @ TIndex(_, _) =>
                        if t.maxVal >= size then
                          throw IndexOutOfBounds(id, size, t.maxVal, idx.pos)
                      case t =>
                        throw UnexpectedType(id.pos, "array access", s"[$t]", t)
                    })
                })
          }
        e
      }
    }

    def myCheckC: PF[(Command, Env), Env] = {
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

    override def checkE(expr: Expr)(implicit env: Env) =
      mergeCheckE(myCheckE)(expr, env)
    override def checkC(cmd: Command)(implicit env: Env) =
      mergeCheckC(myCheckC)(cmd, env)
  }
}
