package fuselang.typechecker

import scala.{PartialFunction => PF}

import fuselang.Utils._

import fuselang.common._
import Syntax._
import Errors._
import Checker._
import EnvHelpers._

object WellFormedChecker {

  def check(p: Prog) = WFCheck.check(p)

  private case class WFEnv() extends ScopeManager[WFEnv] {
    def withScope(inScope: WFEnv => WFEnv): WFEnv = inScope(this)

    def merge(that: WFEnv): WFEnv = this
  }

  private final case object WFCheck extends PartialChecker {

    type Env = WFEnv
    val emptyEnv = WFEnv()

    override def myCheckE: PF[(Expr, Env), Env] = {
      case (expr@ERecLiteral(_), _) => throw NotInBinder(expr.pos, "Record Literal")
      case (expr@EArrLiteral(_), _) => throw NotInBinder(expr.pos, "Array Literal")
    }

    override def myCheckC: PF[(Command, Env), Env] = {
      case (l@CLet(id, typ, Some(EArrLiteral(_))), e) => {
        val expTyp = typ.getOrThrow(ExplicitTypeMissing(l.pos, "Array literal", id))
        expTyp match {
          case TArray(_, dims) =>
            assertOrThrow(dims.length == 1, Unsupported(l.pos, "Multidimensional array literals"))
          case _ => ()
        }
        e
      }
      case (l@CLet(id, typ, Some(ERecLiteral(_))), e) => {
        typ.getOrThrow(ExplicitTypeMissing(l.pos, "Record literal", id));
        e
      }
      case (l@CLet(id, typ, None), e) => {
        typ.getOrThrow(ExplicitTypeMissing(l.pos, "Let binding without initializer", id))
        e
      }
    }
  }
}

