package fuselang.common
import scala.{PartialFunction => PF}

import fuselang.Utils.asPartial
import Syntax._
import EnvHelpers._

object Checker {

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
  abstract class Checker {

    type Env <: ScopeManager[Env]

    val emptyEnv: Env

    /**
      * Top level function called on the AST.
      */
    def check(p: Prog): Unit = {
      val Prog(_, defs, _, _, cmd) = p

      val env = defs.foldLeft(emptyEnv)({
        case (env, defi) => checkDef(defi)(env)
      })

      checkC(cmd)(env); ()
    }

    /**
      * Helper functions for checking sequences of the same element.
      */
    def checkSeqWith[T](f: (T, Env) => Env)(iter: Iterable[T])(env: Env): Env =
      iter.foldLeft(env)({ case (env, t) => f(t, env) })

    def checkESeq(exprs: Iterable[Expr])(implicit env: Env): Env = {
      checkSeqWith[Expr](checkE(_: Expr)(_: Env))(exprs)(env)
    }

    def checkCSeq(cmds: Iterable[Command])(implicit env: Env): Env = {
      checkSeqWith[Command](checkC(_: Command)(_: Env))(cmds)(env)
    }

    def checkDef(defi: Definition)(implicit env: Env) = defi match {
      case FuncDef(_, _, _, bodyOpt) => bodyOpt.map(checkC).getOrElse(env)
      case _: RecordDef => env
    }

    def checkE(expr: Expr)(implicit env: Env): Env = expr match {
      case _: ERational | _: EInt | _: EBool | _: EVar => env
      case ERecLiteral(fields) => checkESeq(fields.map(_._2))
      case EArrLiteral(idxs) => checkESeq(idxs)
      case EBinop(_, e1, e2) => checkESeq(Vector(e1, e2))
      case EApp(_, args) => checkESeq(args)
      case ECast(e, _) => checkE(e)
      case ERecAccess(rec, _) => checkE(rec)
      case EArrAccess(_, idxs) => checkESeq(idxs)
      case EPhysAccess(_, bankIdxs) =>
        checkESeq(bankIdxs.flatMap({ case (bank, idx) => List(bank, idx) }))
    }

    def checkLVal(e: Expr)(implicit env: Env): Env = checkE(e)

    def checkC(cmd: Command)(implicit env: Env): Env = cmd match {
      case _: CSplit | _: CView | CEmpty | _: CDecorate => env
      case CPar(c1, c2) => checkCSeq(Vector(c1, c2))
      case CSeq(c1, c2) => checkCSeq(Vector(c1, c2))
      case CUpdate(lhs, rhs) => checkE(rhs)(checkLVal(lhs))
      case CReduce(_, lhs, rhs) => checkE(rhs)(checkLVal(lhs))
      case CLet(_, _, eOpt) => eOpt.map(checkE).getOrElse(env)
      case CExpr(e) => checkE(e)
      case CReturn(e) => checkE(e)
      case CIf(cond, c1, c2) => {
        val nEnv = checkE(cond)
        val e1 = nEnv.withScope(checkC(c1)(_))
        val e2 = nEnv.withScope(checkC(c2)(_))
        e1 merge e2
      }
      case CFor(_, _, par, combine) => {
        val e1 = env.withScope(checkC(par)(_))
        checkC(combine)(e1)
      }
      case CWhile(cond, _, body) => {
        checkE(cond).withScope(checkC(body)(_))
      }
    }

  }

  /**
    * Partial checker defines helper functions for writing down
    * transformers and bootstrapping them correctly. We use
    * [[scala.PartialFunction]] to get a partial traversal pattern.
    *
    * In order to define a partial pattern, we first create a PartialFunction
    * that implements the rewrites we want:
    * [[
    * def myCheckE: PF[(Expr, Env), (Expr, Env)] = {
    *   case (EInt(_), env) => ???
    * }
    * ]]
    *
    * We can then override default traversal pattern with ours:
    * [[
    * override def checkE(cmd: Command)(implicit env: Env) =
    *   (myCheckE.orElse(partialCheckE))(cmd, env)
    * ]]
    *
    * The "magic" here is in [[scala.PartialFunction.orElse]] which
    * executes myCheckE first and if there are no matching cases, falls
    * back to partialRewriteE which has the default traversal behavior.
    */
  abstract class PartialChecker extends Checker {
    private val partialCheckE: PF[(Expr, Env), Env] =
      asPartial(super.checkE(_: Expr)(_: Env))

    private val partialCheckC: PF[(Command, Env), Env] =
      asPartial(super.checkC(_: Command)(_: Env))

    // Convinience functions for when we want to compose the traversal
    // pattern.
    def mergeCheckE(
        myCheckE: PF[(Expr, Env), Env]
    ): PF[(Expr, Env), Env] = {
        myCheckE.orElse(partialCheckE)
    }
    def mergeCheckC(
        myCheckC: PF[(Command, Env), Env]
    ): PF[(Command, Env), Env] = {
        myCheckC.orElse(partialCheckC)
    }
  }
}
