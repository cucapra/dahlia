package fuselang.common

import Syntax._
import EnvHelpers._
import scala.{PartialFunction => PF}

object Checker {

  /**
   * Helper function for lifting checkE and checkC. Only we define a partial
   * function that handles some of the cases in checkE or checkC that we
   * care about, we can define the checkE for the Checker as:
   *
   * [[myCheckC.orElse(asPartial(checkE(_: Expr)(_: Env)))]]
   *
   */
  def asPartial[A, B, C](f: (A, B) => C): PF[(A, B), C ] = { case (a, b) => f(a, b) }

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

      val env = defs.foldLeft(emptyEnv)({ case (env, defi) => checkDef(defi)(env)})

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
      case _:RecordDef => env
    }

    def checkE(expr: Expr)(implicit env: Env): Env = expr match {
      case _:ERational | _:EInt | _:EBool | _:EVar => env
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
      case _:CSplit | _:CView | CEmpty | _:CDecorate => env
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
   * Partial checker defines helper functions for writing down checkers and
   * bootstrapping them correctly. Most of the time, we'll override one
   * of the myCheck methods and then use them with the default checker methods.
   */
  abstract class PartialChecker extends Checker {

    // We create these two objects to get reference equality in the checkE
    // conditional below.
    private def defaultMyCheckE: PF[(Expr, Env), Env] =
      asPartial(checkE(_: Expr)(_: Env))

    private def defaultMyCheckC: PF[(Command, Env), Env] =
      asPartial(checkC(_: Command)(_: Env))

    def myCheckE: PF[(Expr, Env), Env] = defaultMyCheckE

    def myCheckC: PF[(Command, Env), Env] = defaultMyCheckC

    override def checkE(expr: Expr)(implicit env: Env): Env = {
      // Don't create a new function if myCheckE was not overriden
      if (myCheckE == defaultMyCheckE) {
        myCheckE(expr, env)
      } else {
        myCheckE.orElse(asPartial(super.checkE(_: Expr)(_: Env)))((expr, env))
      }
    }

    override def checkC(cmd: Command)(implicit env: Env): Env = {
      if (myCheckC == defaultMyCheckC) {
        myCheckC(cmd, env)
      } else {
        myCheckC.orElse(asPartial(super.checkC(_: Command)(_: Env)))((cmd, env))
      }
    }
  }
}
