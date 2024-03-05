package fuselang.passes

import scala.{PartialFunction => PF}

import fuselang.Utils._

import fuselang.common._
import Syntax._
import Errors._
import Checker._
import EnvHelpers._

object WellFormedChecker {

  def check(p: Prog) = WFCheck.check(p)

  private case class WFEnv(
      map: Map[Id, FuncDef] = Map(),
      insideUnroll: Boolean = false,
      insideFunc: Boolean = false
  ) extends ScopeManager[WFEnv]
    with Tracker[Id, FuncDef, WFEnv] {
    def merge(that: WFEnv): WFEnv = this

    override def add(k: Id, v: FuncDef): WFEnv =
      WFEnv(
        insideUnroll=insideUnroll,
        insideFunc=insideFunc,
        map=this.map + (k -> v)
      )

    override def get(k: Id): Option[FuncDef] = this.map.get(k)

    def canHaveFunctionInUnroll(k: Id): Boolean = {
      this.get(k) match {
        case Some(FuncDef(_, args, _, _)) =>
          if this.insideUnroll then {
            args.foldLeft(true)({
              (r, arg) => arg.typ match {
                case TArray(_, _, _) => false
                case _ => r
              }
            })
          } else
            true
        case None => true // This is supposed to be unreachable
      }
    }
  }

  private case object WFCheck extends PartialChecker {

    type Env = WFEnv
    val emptyEnv = WFEnv()

    override def checkDef(defi: Definition)(implicit env: Env) = defi match {
      case fndef @ FuncDef(id, _, _, bodyOpt) =>
        val nenv = env.add(id, fndef)
        bodyOpt.map(checkC(_)(nenv.copy(insideFunc = true))).getOrElse(nenv)
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
      case (expr @ EApp(id, _), env) => {
        assertOrThrow(env.canHaveFunctionInUnroll(id) == true, FuncInUnroll(expr.pos))
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

    override def checkE(expr: Expr)(implicit env: Env) =
      mergeCheckE(myCheckE)(expr, env)
    override def checkC(cmd: Command)(implicit env: Env) =
      mergeCheckC(myCheckC)(cmd, env)
  }
}
