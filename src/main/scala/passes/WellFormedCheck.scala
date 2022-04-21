package fuselang.passes

import scala.{PartialFunction => PF}

import fuselang.Utils._

import fuselang.common._
import Syntax._
import Errors._
import Checker._
import EnvHelpers._

object WellFormedChecker {

  def check(p: Prog): Unit = WFCheck.check(p)

  private case class WFEnv(
      insideUnroll: Boolean = false,
      insideFunc: Boolean = false
  ) extends ScopeManager[WFEnv] {
    def merge(that: WFEnv): WFEnv = this
  }

  private case object WFCheck extends PartialChecker {

    type Env = WFEnv
    val emptyEnv: WFEnv = WFEnv()

    override def checkDef(defi: Definition)(implicit env: Env): Env = defi match {
      case FuncDef(_, _, _, bodyOpt) =>
        bodyOpt.map(checkC(_)(env.copy(insideFunc = true))).getOrElse(env)
      case _: RecordDef => env
    }

    def myCheckE: PF[(Expr, Env), Env] = {
      case (expr: EPhysAccess, _) =>
        throw CompilerError.PassError(
          "Physical accesses should be removed up the lowering passes.",
          expr.pos
        )
      case (expr: ERecLiteral, _) =>
        throw NotInBinder(expr.pos, "Record Literal")
      case (expr: EArrLiteral, _) =>
        throw NotInBinder(expr.pos, "Array Literal")
      case (expr: EApp, env) => {
        assertOrThrow(env.insideUnroll == false, FuncInUnroll(expr.pos))
        env
      }
    }

    def myCheckC: PF[(Command, Env), Env] = {
      case (cmd @ CReduce(op, l, r), e) => {
        assertOrThrow(e.insideUnroll == false, ReduceInsideUnroll(op, cmd.pos))
        checkE(r)(checkE(l)(e))
      }
      case (l @ CLet(id, typ, Some(EArrLiteral(_))), e) => {
        val expTyp = typ
          .getOrThrow(ExplicitTypeMissing(l.pos, "Array literal", id))
        expTyp match {
          case TArray(_, dims, _) =>
            assertOrThrow(
              dims.length == 1,
              Unsupported(l.pos, "Multidimensional array literals")
            )
          case _ => ()
        }
        e
      }
      case (l @ CLet(id, typ, Some(ERecLiteral(_))), e) => {
        typ.getOrThrow(ExplicitTypeMissing(l.pos, "Record literal", id));
        e
      }
      case (cmd @ (_: CView | _: CSplit), env) => {
        assertOrThrow(env.insideUnroll == false, ViewInsideUnroll(cmd.pos))
        env
      }
      case (CFor(r @ CRange(_, _, _, s, e, u), _, par, combine), env) => {
        assertOrThrow(
          e > s,
          Malformed(
            r.pos,
            s"Loop range $s (inclusive) to $e (exclusive) contains no elements"
          )
        );
        val insideUnroll = u > 1 || env.insideUnroll
        val e1 = env.withScope(newScope =>
          checkC(par)(newScope.copy(insideUnroll = insideUnroll))
        )
        // Allow reduce operators inside combine
        val e2 = checkC(combine)(e1.copy(insideUnroll = false))
        e2.copy(insideUnroll = env.insideUnroll)
      }
      case (cmd @ CReturn(_), env) => {
        assertOrThrow(env.insideFunc, ReturnNotInFunc(cmd.pos))
        env
      }
    }

    override def checkE(expr: Expr)(implicit env: Env): Env =
      mergeCheckE(myCheckE)(expr, env)
    override def checkC(cmd: Command)(implicit env: Env): Env =
      mergeCheckC(myCheckC)(cmd, env)
  }
}
