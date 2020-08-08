package fuselang.common

import fuselang.Utils.asPartial
import Syntax._
import EnvHelpers._
import scala.{PartialFunction => PF}

object Transformer {

  abstract class Transformer {

    type Env <: ScopeManager[Env]

    val emptyEnv: Env

    /**
      * Top level function called on the AST.
      */
    def rewrite(p: Prog): Prog = {
      val Prog(_, defs, _, decls, cmd) = p

      val (ndefs, env) = rewriteDefSeq(defs)(emptyEnv)
      val (ndecls, env1) = rewriteDeclSeq(decls)(env)
      val (ncmd, _) = rewriteC(cmd)(env1)
      p.copy(defs = ndefs.toSeq, decls = ndecls.toSeq, cmd = ncmd)
    }

    /**
      * Helper functions for checking sequences of the same element.
      */
    def rewriteSeqWith[T](
        f: (T, Env) => (T, Env)
    )(iter: Iterable[T])(env: Env): (Iterable[T], Env) = {
      val (ts, env1) = iter.foldLeft(Seq[T](), env)({
        case ((ts, env), t) =>
          val (t1, env1) = f(t, env)
          (t1 +: ts, env1)
      })
      (ts.reverse, env1)
    }

    def rewriteESeq(
        exprs: Iterable[Expr]
    )(implicit env: Env): (Iterable[Expr], Env) = {
      rewriteSeqWith[Expr](rewriteE(_: Expr)(_: Env))(exprs)(env)
    }

    def rewriteCSeq(
        cmds: Iterable[Command]
    )(implicit env: Env): (Iterable[Command], Env) = {
      rewriteSeqWith[Command](rewriteC(_: Command)(_: Env))(cmds)(env)
    }

    def rewriteDefSeq(
        defs: Iterable[Definition]
    )(implicit env: Env): (Iterable[Definition], Env) = {
      rewriteSeqWith[Definition](rewriteDef(_: Definition)(_: Env))(defs)(env)
    }

    def rewriteDeclSeq(ds: Seq[Decl])(implicit env: Env): (Seq[Decl], Env) =
      (ds, env)

    def rewriteDef(defi: Definition)(implicit env: Env) = defi match {
      case fdef @ FuncDef(_, args, _, bodyOpt) => {
        val (nArgs, env1) = rewriteDeclSeq(args)

        val (nBody, env2) = bodyOpt match {
          case None => (None, env1)
          case Some(body) => {
            val (nbody, nEnv) = rewriteC(body)
            Some(nbody) -> nEnv
          }
        }

        fdef.copy(args = nArgs, bodyOpt = nBody) -> env2
      }
      case _: RecordDef => (defi, env)
    }

    def rewriteE(expr: Expr)(implicit env: Env): (Expr, Env) =
      expr match {
        case _: ERational | _: EInt | _: EBool | _: EVar => (expr, env)
        case ERecLiteral(fields) => {
          val (fs, env1) = rewriteESeq(fields.map(_._2))
          ERecLiteral(fields.map(_._1).zip(fs).toMap) -> env1
        }
        case EArrLiteral(idxs) => {
          val (idxs1, env1) = rewriteESeq(idxs)
          EArrLiteral(idxs1.toSeq) -> env1
        }
        case EBinop(op, e1, e2) => {
          val (ne1, env1) = rewriteE(e1)
          val (ne2, env2) = rewriteE(e2)(env1)
          EBinop(op, ne1, ne2) -> env2
        }
        case app @ EApp(_, args) => {
          val (nargs, env1) = rewriteESeq(args)
          app.copy(args = nargs.toSeq) -> env1
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
          acc.copy(idxs = nidxs.toSeq) -> env1
        }
        case acc @ EPhysAccess(_, bankIdxs) => {
          val init = (Seq[(Int, Expr)](), env)
          val (nBankIdxsReversed, nEnv) = bankIdxs.foldLeft(init)({
            case ((nBankIdxs, env), (bank, idx)) =>
              val (nIdx, env1) = rewriteE(idx)(env)
              ((bank, nIdx) +: nBankIdxs, env1)
          })
          acc.copy(bankIdxs = nBankIdxsReversed.reverse) -> nEnv
        }
      }

    def rewriteLVal(e: Expr)(implicit env: Env): (Expr, Env) = rewriteE(e)

    def rewriteC(cmd: Command)(implicit env: Env): (Command, Env) = cmd match {
      case _: CSplit | _: CView | CEmpty | _: CDecorate => (cmd, env)
      case CPar(cmds) => {
        val (ncmds, env1) = rewriteCSeq(cmds)
        CPar(ncmds.toSeq) -> env1
      }
      case CSeq(cmds) => {
        val (ncmds, env1) = rewriteCSeq(cmds)
        CSeq(ncmds.toSeq) -> env1
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
      case cb @ CBlock(body) => {
        val (nbody, env1) = env.withScopeAndRet(rewriteC(body)(_))
        cb.copy(cmd = nbody) -> env1
      }
    }

  }

  /**
    * Partial transformer defines helper functions for writing down
    * transformers and bootstrapping them correctly. We use
    * [[scala.PartialFunction]] to get a partial traversal pattern.
    *
    * In order to define a partial pattern, we first create a PartialFunction
    * that implements the rewrites we want:
    * [[
    * def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
    *   case (EInt(_), env) => ???
    * }
    * ]]
    *
    * We can then override default traversal pattern with ours:
    * [[
    * override def rewriteE(cmd: Command)(implicit env: Env) =
    *   (myRewriteE.orElse(partialRewriteE))(cmd, env)
    * ]]
    *
    * The "magic" here is in [[scala.PartialFunction.orElse]] which
    * executes myRewriteE first and if there are no matching cases, falls
    * back to partialRewriteE which has the default traversal behavior.
    */
  abstract class PartialTransformer extends Transformer {
    private val partialRewriteE: PF[(Expr, Env), (Expr, Env)] =
      asPartial(super.rewriteE(_: Expr)(_: Env))

    private val partialRewriteC: PF[(Command, Env), (Command, Env)] =
      asPartial(super.rewriteC(_: Command)(_: Env))

    // Convinience functions for when we want to compose the traversal
    // pattern.
    def mergeRewriteE(
        myRewriteE: PF[(Expr, Env), (Expr, Env)]
    ): PF[(Expr, Env), (Expr, Env)] = {
      myRewriteE.orElse(partialRewriteE)
    }
    def mergeRewriteC(
        myRewriteC: PF[(Command, Env), (Command, Env)]
    ): PF[(Command, Env), (Command, Env)] = {
      myRewriteC.orElse(partialRewriteC)
    }
  }

  /**
    * Transformer that adds type annotations to newly created AST nodes
    * returned from the transformer.
    *
    * XXX(rachit): The right way to make this work would be to use an
    * environment returned from the TypeChecker and use it to add type
    * annotations to every AST node.
    */
  abstract class TypedPartialTransformer extends PartialTransformer {
    private val partialRewriteE: PF[(Expr, Env), (Expr, Env)] =
      asPartial(super.rewriteE(_: Expr)(_: Env))

    /** Public wrapper for [[rewriteE]] that transfers type annotations
      * from the input [[expr]] to the expression resulting from [[rewriteE]].
      * Any subclasses that overwrite `rewriteE` should call this function.
      */
    def transferType(expr: Expr, f: (Expr, Env) => (Expr, Env))(
        implicit env: Env
    ): (Expr, Env) = {
      val (e1, env1) = f(expr, env)
      e1.typ = expr.typ
      (e1, env1)
    }

    override def mergeRewriteE(
        myRewriteE: PF[(Expr, Env), (Expr, Env)]
    ): PF[(Expr, Env), (Expr, Env)] = {
      val func = (expr: Expr, env: Env) => {
        val (e1, env1) = myRewriteE.orElse(partialRewriteE)(expr, env)
        e1.typ = expr.typ
        (e1, env1)
      }
      asPartial(func(_: Expr, _: Env))
    }
  }
}
