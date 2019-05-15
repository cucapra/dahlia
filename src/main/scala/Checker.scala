package fuselang

import Syntax._
import EnvHelpers._
import scala.{PartialFunction => PF}

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

    /**
     * Top level function called on the AST.
     */
    def check(p: Prog)

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
      case FuncDef(_, _, bodyOpt) => bodyOpt.map(checkC).getOrElse(env)
      case _:RecordDef => env
    }

    def checkE(expr: Expr)(implicit env: Env): Env = expr match {
      case _:EFloat | _:EInt | _:EBool | _:EVar => env
      case ERecLiteral(fields) => checkESeq(fields.map(_._2))
      case EArrLiteral(idxs) => checkESeq(idxs)
      case EBinop(_, e1, e2) => checkESeq(Vector(e1, e2))
      case EApp(_, args) => checkESeq(args)
      case ECast(e, _) => checkE(e)
      case ERecAccess(rec, _) => checkE(rec)
      case EArrAccess(_, idxs) => checkESeq(idxs)
    }

    def checkLVal(e: Expr)(implicit env: Env): Env = checkE(e)

    def checkC(cmd: Command)(implicit env: Env): Env = cmd match {
      case _:CSplit | _:CView | CEmpty => env
      case CPar(c1, c2) => checkCSeq(Vector(c1, c2))
      case CSeq(c1, c2) => checkCSeq(Vector(c1, c2))
      case CUpdate(lhs, rhs) => checkE(rhs)(checkLVal(lhs))
      case CReduce(_, lhs, rhs) => checkE(rhs)(checkLVal(lhs))
      case CLet(_, _, eOpt) => eOpt.map(checkE).getOrElse(env)
      case CExpr(e) => checkE(e)
      case CIf(cond, c1, c2) => {
        val nEnv = checkE(cond)
        val e1 = nEnv.withScope(checkC(c1)(_))
        val e2 = nEnv.withScope(checkC(c2)(_))
        e1 merge e2
      }
      case CFor(_, par, combine) => {
        val e1 = env.withScope(checkC(par)(_))
        checkC(combine)(e1)
      }
      case CWhile(cond, body) => {
        checkE(cond).withScope(checkC(body)(_))
      }
    }

    /**
     * Helper function for lifting checkE and checkC. Only we define a partial
     * function that handles some of the cases in checkE or checkC that we
     * care about, we can define the checkE for the Checker as:
     *
     * [[myCheckC.orElse(asPartial(checkE(_: Expr)(_: Env)))]]
     *
     */
    def asPartial[A, B, C](f: (A, B) => C): PF[(A, B), C ] = { case (a, b) => f(a, b) }

  }
}
