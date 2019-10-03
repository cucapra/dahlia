package fuselang.typechecker

import fuselang.Utils._

import fuselang.common._
import EnvHelpers._
import Syntax._
import CompilerError._

object CapabilityEnv {

  val emptyEnv: CapabilityEnv = Env()

  sealed trait CapabilityEnv
    extends ScopeManager[CapabilityEnv]
    with Tracker[Expr, Capability, CapabilityEnv]

  private case class Env(
    readSet: ScopedSet[Expr] = ScopedSet(),
    writeSet: ScopedSet[Expr] = ScopedSet()) extends CapabilityEnv {

      def get(e: Expr) =
        if (readSet.contains(e)) Some(Read)
        else if (writeSet.contains(e)) Some(Write)
        else None

      def add(e: Expr, cap: Capability) = cap match {
        case Read => this.copy(readSet = readSet.add(e))
        case Write => this.copy(writeSet = writeSet.add(e))
      }

      def endScope = {
        val scopes = for {
          (_, rSet) <- readSet.endScope;
          (_, wSet) <- writeSet.endScope
        } yield this.copy(readSet = rSet, writeSet = wSet)

        scopes.getOrThrow(Impossible("Removed topmost scope"))
      }

      def withScope(inScope: CapabilityEnv => CapabilityEnv): CapabilityEnv = {
        inScope(this.copy(readSet = readSet.addScope, writeSet = writeSet.addScope)) match {
          case that: Env => that.endScope
        }
      }

      def merge(that: CapabilityEnv) = {
        assert(this == that, "Tried to merge different capability envs")
        this
      }
  }
}
