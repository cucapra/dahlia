package fuselang.passes

import scala.{PartialFunction => PF}
import fuselang.common._
import Syntax._
import CodeGenHelpers._
import CompilerError._
import Transformer._
import EnvHelpers._

import fuselang.Utils.RichOption

/**
  * AST pass to rewrite views into simple array accesses. Should be used after
  * type checking.
  *
  * TODO(rachit): Update description.
  *
  * If `a` itself is a view, we keep rewriting it until we reach a true array.
  *
  * For information about the monadic implementation, refer to the docs for
  * [[fuselang.StateHelper.State]].
  */
object RewriteView extends TypedPartialTransformer {
  case class ViewEnv(map: Map[Id, Seq[Expr] => Expr])
      extends ScopeManager[ViewEnv]
      with Tracker[Id, Seq[Expr] => Expr, ViewEnv] {
    def merge(that: ViewEnv) = {
      if (this.map.keys != that.map.keys)
        throw Impossible("Tried to merge ViewEnvs with different keys.")
      this
    }

    def get(arrId: Id) = this.map.get(arrId)

    def add(arrId: Id, func: Seq[Expr] => Expr) =
      ViewEnv(this.map + (arrId -> func))
  }

  type Env = ViewEnv
  val emptyEnv = ViewEnv(Map())

  private def genViewAccessExpr(view: View, idx: Expr): Expr =
    view.suffix match {
      case Aligned(factor, e2) => (EInt(factor) * e2) + idx
      case Rotation(e) => e + idx
    }

  private def splitAccessExpr(
      i: Expr,
      j: Expr,
      arrBank: Int,
      viewBank: Int
  ): Expr = {
    (i * EInt(viewBank)) +
      ((j / EInt(viewBank)) * EInt(arrBank)) +
      (j % EInt(viewBank))
  }

  def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
    case (acc @ EArrAccess(arrId, idxs), env) => {
      // Rewrite the indexing expressions
      val (nIdxs, nEnv) = super.rewriteESeq(idxs)(env)
      val rewrite = nEnv.get(arrId)
      if (rewrite.isDefined) {
        rewriteE((rewrite.get)(nIdxs.toSeq))(nEnv)
      } else {
        acc.copy(idxs = nIdxs.toSeq) -> nEnv
      }
    }
    case (acc @ EPhysAccess(arrId, bankIdxs), env) => {
      // Rewrite the indexing expressions
      val (nBankIdxs, nEnv) = super.rewriteSeqWith({
        case ((bank, idx), env) =>
          val (nIdx, env1) = super.rewriteE(idx)(env)
          (bank, nIdx) -> env1
      }: ((Int, Expr), Env) => ((Int, Expr), Env))(bankIdxs)(env)

      if (nEnv.get(arrId).isDefined) {
        throw NotImplemented("Rewriting physical accesses on views.")
      }
      acc.copy(bankIdxs = nBankIdxs.toSeq) -> nEnv
    }
  }

  def myRewriteC: PF[(Command, Env), (Command, Env)] = {
    case (CView(id, arrId, dims), env) => {
      val f = (es: Seq[Expr]) =>
        EArrAccess(
          arrId,
          es.zip(dims)
            .map({
              case (idx, view) =>
                genViewAccessExpr(view, idx)
            })
        )
      (CEmpty, env.add(id, f))
    }
    case (c @ CSplit(id, arrId, factors), env) => {
      val arrBanks = arrId.typ
        .getOrThrow(Impossible(s"$arrId is missing type in $c")) match {
        case TArray(_, dims, _) => dims.map(_._2)
        case t => throw Impossible(s"Array has type $t in $c")
      }
      val f = (es: Seq[Expr]) => {
        val it = es.iterator
        // For each dimension, if it was split by more than 1, group the next
        // two accessors.
        val groups = factors.map({
          case factor => Seq(it.next, it.next) -> factor
        })
        val idxs = groups
          .zip(arrBanks)
          .map({
            case ((Seq(i), _), _) => i
            case ((Seq(i, j), factor), arrBank) =>
              splitAccessExpr(i, j, arrBank, arrBank / factor)
          })
        EArrAccess(arrId, idxs)
      }
      (CEmpty, env.add(id, f))
    }
  }

  // Compose custom traversal with parent's generic traversal.
  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)
  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)
}
