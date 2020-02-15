package fuselang.typechecker

import fuselang.Utils._

import fuselang.common._
import EnvHelpers._
import Syntax._
import CompilerError._
import ScopeMap._

object CapabilityEnvironment {

  val emptyEnv = CapabilityEnv()

  case class CapabilityEnv(
    readSet: ScopedSet[Expr] = ScopedSet(),
    writeSet: ScopedSet[Expr] = ScopedSet(),
    intTrack: ScopedMap[Int, Int] = ScopedMap()) extends ScopeManager[CapabilityEnv] {

      def endScope = {
        val scopes = for {
          (_, rSet) <- readSet.endScope;
          (_, wSet) <- writeSet.endScope
        } yield this.copy(readSet = rSet, writeSet = wSet)

        scopes.getOrThrow(Impossible("Removed topmost scope"))
      }

      def withScope(inScope: CapabilityEnv => CapabilityEnv): CapabilityEnv = {
        inScope(this.copy(readSet = readSet.addScope, writeSet = writeSet.addScope)) match {
          case that => that.endScope
        }
      }

      def merge(that: CapabilityEnv) = {
        assert(this == that, "Tried to merge different capability envs")
        this
      }
  }

  object CapabilityEnv {
    implicit val capTracker = new NewTracker[Expr, Capability, CapabilityEnv] {
      def get(env: CapabilityEnv, e: Expr) =
        if (env.readSet.contains(e)) Some(Read)
        else if (env.writeSet.contains(e)) Some(Write)
        else None

      def add(env: CapabilityEnv, e: Expr, cap: Capability) = cap match {
        case Read => env.copy(readSet = env.readSet.add(e))
        case Write => env.copy(writeSet = env.writeSet.add(e))
      }
    }
  }
}
