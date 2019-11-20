package fuselang.passes

import scala.{PartialFunction => PF}

import fuselang.common._
// import ScopeMap._
import Syntax._
import Errors._
// import CompilerError._
import Checker._
import EnvHelpers._

object DependentLoops {

  def check(p: Prog) = DepCheck.check(p)

  private case class UseEnv(
    used: Set[Id]
  ) extends ScopeManager[UseEnv] {
    def withScope(inScope: UseEnv => UseEnv): UseEnv = inScope(this)

    def merge(that: UseEnv): UseEnv = UseEnv(this.used ++ that.used)

    def add(id: Id): UseEnv = {
      UseEnv(this.used + id)
    }
  }

  private final case object UseCheck extends PartialChecker {

    type Env = UseEnv
    val emptyEnv = UseEnv(Set())

    override def myCheckE: PF[(Expr, Env), Env] = {
      case (EVar(id), env) => env.add(id)
    }
  }

  private case class DepEnv(
    loopVars: Set[Id],
    depVars: Set[Id]
  ) extends ScopeManager[DepEnv] {
    def withScope(inScope: DepEnv => DepEnv): DepEnv = {
      inScope(this)
    }

    def forgetScope(inScope: DepEnv => DepEnv): DepEnv = {
      inScope(this)
      this
    }

    def merge(that: DepEnv): DepEnv = DepEnv(this.loopVars ++ that.loopVars, this.depVars ++ that.depVars)

    def addLoopVar(id: Id): DepEnv = {
      // remove id and then add it back so that the most recent id is in the set
      DepEnv(this.loopVars + id, this.depVars)
    }

    def addDep(id: Id): DepEnv = {
      DepEnv(this.loopVars, (this.depVars - id) + id)
    }

    def removeDep(id: Id): DepEnv = {
      DepEnv(this.loopVars, this.depVars - id)
    }

    def intersect(set: Set[Id]): Set[Id] = {
      (this.loopVars.union(this.depVars)).intersect(set)
    }
  }

  private final case object DepCheck extends PartialChecker {

    type Env = DepEnv
    val emptyEnv = DepEnv(Set(), Set())

    override def myCheckE: PF[(Expr, Env), Env] = {
      case (EArrAccess(id@_, idxs), env) => {
        idxs.foreach(e => {
                       val used = UseCheck.checkE(e)(UseCheck.emptyEnv)
                       val intersect = env.depVars.intersect(used.used)
                       if (intersect.size != 0) {
                         val sourceId = intersect.toList(0)
                         throw LoopDynamicAccess(e, sourceId)
                       }
                     })
        env
      }
    }

    override def myCheckC: PF[(Command, Env), Env] = {
      case (CFor(range, _, par, _), env) => {
        if (range.u > 1) {
          env.forgetScope(e1 =>
            checkC(par)(e1.addLoopVar(range.iter))
          )
        } else {
          env.forgetScope(e1 =>
            checkC(par)(e1)
          )
        }
      }
      case (CLet(id, _, Some(exp)), env) => {
        val used = UseCheck.checkE(exp)(UseCheck.emptyEnv)
        if (env.intersect(used.used).size != 0) {
          env.addDep(id)
        } else {
          env
        }
      }
      case (CUpdate(EVar(id), rhs), env) => {
        val used = UseCheck.checkE(rhs)(UseCheck.emptyEnv)
        if (env.intersect(used.used).size != 0) {
          env.addDep(id)
        } else {
          env.removeDep(id)
        }
      }
    }
  }
}
