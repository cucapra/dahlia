package fuselang.typechecker

import fuselang.Utils._

import fuselang.common._
import EnvHelpers._
import Syntax._
import CompilerError._

object CapabilityEnv:

  val emptyEnv: CapabilityEnv = Env()

  sealed trait CapabilityEnv
      extends ScopeManager[CapabilityEnv]
      with Tracker[Expr, Capability, CapabilityEnv]

  private case class Env(
      readSet: ScopedSet[Expr] = ScopedSet(List(Set())),
      writeSet: ScopedSet[Expr] = ScopedSet(List(Set()))
  ) extends CapabilityEnv:

    def get(e: Expr): Option[Capability] =
      if readSet.contains(e) then Some(Read)
      else if writeSet.contains(e) then Some(Write)
      else None

    def add(e: Expr, cap: Capability): CapabilityEnv = cap match
      case Read => this.copy(readSet = readSet.add(e))
      case Write => this.copy(writeSet = writeSet.add(e))

    def endScope: Env =
      val scopes = for
        (_, rSet) <- readSet.endScope;
        (_, wSet) <- writeSet.endScope
      yield this.copy(readSet = rSet, writeSet = wSet)

      scopes.getOrThrow(Impossible("Removed topmost scope"))

    override def withScopeAndRet[R](
        inScope: CapabilityEnv => (R, CapabilityEnv)
    ): (R, CapabilityEnv) =
      inScope(
        this.copy(readSet = readSet.addScope, writeSet = writeSet.addScope)
      ) match
        case (r, that: Env) => (r, that.endScope)

    override def withScope(
        inScope: CapabilityEnv => CapabilityEnv
    ): CapabilityEnv =
      inScope(
        this.copy(readSet = readSet.addScope, writeSet = writeSet.addScope)
      ) match
        case that: Env => that.endScope

    def merge(that: CapabilityEnv): Env =
      assert(this == that, "Tried to merge different capability envs")
      this
