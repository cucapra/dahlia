package fuselang.common

import Syntax._
import EnvHelpers._
import scala.{PartialFunction => PF}

object Transformer {

  /**
    * Helper function for lifting checkE and checkC. Only we define a partial
    * function that handles some of the cases in checkE or checkC that we
    * care about, we can define the checkE for the Checker as:
    *
    * [[myRewriteC.orElse(asPartial(checkE(_: Expr)(_: Env)))]]
    *
    */
  def asPartial[A, B, C](f: (A, B) => C): PF[(A, B), C] = {
    case (a, b) => f(a, b)
  }

  /**
    * A checker is a compiler pass that collects information using some Environment
    * type and is only used for it's side effects. A Checker cannot modify the
    * AST beyond adding annotations on it.
    *
    * The Environment is threaded through the nodes while checking happens.
    * For example, if we have Node(e1, e2), the checker runs:
    *
    * val env1 = check(e1)(currEnv)
    * check(e2)(env1)
    */
  abstract class Transformer {

    type Env <: ScopeManager[Env]

    val emptyEnv: Env

    /**
      * Top level function called on the AST.
      */
    def rewrite(p: Prog): Prog = {
      val Prog(_, defs, _, _, cmd) = p

      val (ndefs, env) = rewriteDefSeq(defs)(emptyEnv)
      val (ncmd, _) = rewriteC(cmd)(env)
      p.copy(defs = ndefs.toList, cmd = ncmd)
    }

    /**
      * Helper functions for checking sequences of the same element.
      */
    def rewriteSeqWith[T](
        f: (T, Env) => (T, Env)
    )(iter: Iterable[T])(env: Env): (Iterable[T], Env) = {
      val (ts, env1) = iter.foldLeft(List[T](), env)({
        case ((ts, env), t) =>
          val (t1, env1) = f(t, env)
          (t1 :: ts, env1)
      })
      (ts.reverse, env1)
    }

    def rewriteESeq(
        exprs: Iterable[Expr]
    )(implicit env: Env): (Iterable[Expr], Env) = {
      rewriteSeqWith[Expr](rewriteE(_: Expr)(_: Env))(exprs)(env)
    }

    def rewriteDefSeq(
        defs: Iterable[Definition]
    )(implicit env: Env): (Iterable[Definition], Env) = {
      rewriteSeqWith[Definition](rewriteDef(_: Definition)(_: Env))(defs)(env)
    }

    def rewriteDef(defi: Definition)(implicit env: Env) = defi match {
      case fdef @ FuncDef(_, _, _, bodyOpt) =>
        bodyOpt match {
          case None => (fdef, env)
          case Some(body) => {
            val (nbody, env1) = rewriteC(body)
            fdef.copy(bodyOpt = Some(nbody)) -> env1
          }
        }
      case _: RecordDef => (defi, env)
    }

    /** Public wrapper for [[rewriteE]] that transfers type annotations
      * from the input [[expr]] to the expression resulting from [[rewriteE]].
      * Any subclasses that overwrite `rewriteE` should call this function.
      */
    def transferType(expr: Expr, f: (Expr, Env) => (Expr, Env))(implicit env: Env): (Expr, Env) = {
      val (e1, env1) = f(expr, env)
      e1.typ = expr.typ
      (e1, env1)
    }

    def rewriteE(expr: Expr)(implicit env: Env): (Expr, Env) = {
      transferType(expr, _rewriteE(_)(_))
    }

    private def _rewriteE(expr: Expr)(implicit env: Env): (Expr, Env) =
      expr match {
        case _: ERational | _: EInt | _: EBool | _: EVar => (expr, env)
        case ERecLiteral(fields) => {
          val (fs, env1) = rewriteESeq(fields.map(_._2))
          ERecLiteral(fields.map(_._1).zip(fs).toMap) -> env1
        }
        case EArrLiteral(idxs) => {
          val (idxs1, env1) = rewriteESeq(idxs)
          EArrLiteral(idxs1.toList) -> env1
        }
        case EBinop(op, e1, e2) => {
          val (ne1, env1) = rewriteE(e1)
          val (ne2, env2) = rewriteE(e2)(env1)
          EBinop(op, ne1, ne2) -> env2
        }
        case app @ EApp(_, args) => {
          val (nargs, env1) = rewriteESeq(args)
          app.copy(args = nargs.toList) -> env1
        }
        case cast @ ECast(e, _) => {
          val (e1, env1) = rewriteE(e)
          cast.copy(e = e1) -> env1
        }
        case rec @ ERecAccess(e, _) => {
          val (e1, env1) = rewriteE(e)
          rec.copy(rec = e1) -> env1
        }
        case acc @ EArrAccess(_, idxs) => {
          val (nidxs, env1) = rewriteESeq(idxs)
          acc.copy(idxs = nidxs.toList) -> env1
        }
        case acc @ EPhysAccess(_, bankIdxs) => {
          val init = (List[(Expr, Expr)](), env)
          val (nBankIdxsReversed, nEnv) = bankIdxs.foldLeft(init)({
            case ((nBankIdxs, env), (bank, idx)) =>
              val (nBank, env1) = rewriteE(bank)(env)
              val (nIdx, env2) = rewriteE(idx)(env1)
              ((nBank, nIdx) :: nBankIdxs, env2)
          })
          acc.copy(bankIdxs = nBankIdxsReversed.reverse.toList) -> nEnv
        }
      }

    def rewriteLVal(e: Expr)(implicit env: Env): (Expr, Env) = rewriteE(e)

    def rewriteC(cmd: Command)(implicit env: Env): (Command, Env) = cmd match {
      case _: CSplit | _: CView | CEmpty | _: CDecorate => (cmd, env)
      case CPar(c1, c2) => {
        val (nc1, env1) = rewriteC(c1)
        val (nc2, env2) = rewriteC(c2)(env1)
        CPar(nc1, nc2) -> env2
      }
      case CSeq(c1, c2) => {
        val (nc1, env1) = rewriteC(c1)
        val (nc2, env2) = rewriteC(c2)(env1)
        CSeq(nc1, nc2) -> env2
      }
      case CUpdate(lhs, rhs) => {
        val (nlhs, env1) = rewriteLVal(lhs)
        val (nrhs, env2) = rewriteE(rhs)(env1)
        CUpdate(nlhs, nrhs) -> env2
      }
      case red @ CReduce(_, lhs, rhs) => {
        val (nlhs, env1) = rewriteLVal(lhs)
        val (nrhs, env2) = rewriteE(rhs)(env1)
        red.copy(lhs = nlhs, rhs = nrhs) -> env2
      }
      case let @ CLet(_, _, eOpt) =>
        eOpt match {
          case None => let -> env
          case Some(e) => {
            val (e1, env1) = rewriteE(e)
            let.copy(e = Some(e1)) -> env1
          }
        }
      case CExpr(e) => {
        val (e1, env1) = rewriteE(e)
        CExpr(e1) -> env1
      }
      case CReturn(e) => {
        val (e1, env1) = rewriteE(e)
        CReturn(e1) -> env1
      }
      case CIf(cond, c1, c2) => {
        val (nCond, env1) = rewriteE(cond)
        val (nc1, env2) = env1.withScopeAndRet(rewriteC(c1)(_))
        val (nc2, env3) = env1.withScopeAndRet(rewriteC(c2)(_))
        CIf(nCond, nc1, nc2) -> (env2 merge env3)
      }
      case fo @ CFor(_, _, par, combine) => {
        val (npar, env1) = env.withScopeAndRet(rewriteC(par)(_))
        val (ncomb, env2) = rewriteC(combine)(env1)
        fo.copy(par = npar, combine = ncomb) -> env2
      }
      case wh @ CWhile(cond, _, body) => {
        val (ncond, env1) = rewriteE(cond)
        val (nbody, env2) = env1.withScopeAndRet(rewriteC(body)(_))
        wh.copy(cond = ncond, body = nbody) -> env2
      }
    }

  }

  /**
    * Partial transformer defines helper functions for writing down
    * transformers and bootstrapping them correctly. Most of the time, we'll
    * override one of the my* methods and then use them with the default
    * checker methods to define a partial traversal pattern.
    *
    * To get a complete traversal, we can compose the my* function with
    * the partial* functions using Scala's [[PartialFunction.orElse]]
    * function.
    */
  abstract class PartialTranformer extends Transformer {

    // We create these two objects to get reference equality in the checkE
    // conditional below.

    def myRewriteE: PF[(Expr, Env), (Expr, Env)] =
      asPartial(rewriteE(_: Expr)(_: Env))

    def myRewriteC: PF[(Command, Env), (Command, Env)] =
      asPartial(rewriteC(_: Command)(_: Env))

    // Handle to the partial rewriters of the parent. Use when defining a
    // partial traversal pattern.
    def partialRewriteE: PF[(Expr, Env), (Expr, Env)] =
      asPartial(super.rewriteE(_: Expr)(_: Env))

    def partialRewriteC: PF[(Command, Env), (Command, Env)] =
      asPartial(super.rewriteC(_: Command)(_: Env))
  }
}
