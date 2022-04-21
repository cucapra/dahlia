package fuselang.typechecker

import scala.{PartialFunction => PF}

import fuselang.common._
import Syntax._
import Syntax.Annotations._
import Errors._
import CompilerError._
import CapabilityEnv._
import Checker._

object CapabilityChecker {

  def check(p: Prog): Unit = CapChecker.check(p)

  private final case object CapChecker extends PartialChecker {

    type Env = CapabilityEnv

    val emptyEnv = CapabilityEnv.emptyEnv

    /**
      * Check an array read.
      * - If there is already a Read capability for this expression, ignore.
      * - If there are no capabilities or a write capability, add a consume
      *   annotation.
      */
    def myCheckE: PF[(Expr, Env), Env] = {
      case (acc @ EArrAccess(_, idxs), env) => {
        val (nEnv, consumableAnn, cap) = env.get(acc) match {
          case Some(Read) => (env, SkipConsume, Read)
          case Some(Write) | None => (checkESeq(idxs)(env), ShouldConsume, Read)
        }
        acc.consumable = Some(consumableAnn)
        nEnv.add(acc, cap)
      }
      case (_: EPhysAccess, _) => {
        throw NotImplemented("Capability checking for physical accesses.")
      }
    }

    def myCheckC: PF[(Command, Env), Env] = {
      case (CSeq(cmds), env) => {
        // Check all seq commands under the same environment
        cmds.foreach(c => checkC(c)(env));
        env
      }
    }

    /**
      * Check an array write. If there is already a write capability, error.
      * Otherwise try to acquire a write capability.
      *
      * This doesn't need to be partial function since it deals with all
      * cases in checkLVal.
      */
    override def checkLVal(e: Expr)(implicit env: Env): Env = e match {
      case acc @ EArrAccess(_, idxs) => {
        val (nEnv, consumableAnn, cap) = env.get(e) match {
          case Some(Write) => throw AlreadyWrite(e)
          case Some(Read) | None => (checkESeq(idxs), ShouldConsume, Write)
        }

        acc.consumable = Some(consumableAnn)
        nEnv.add(e, cap)
      }
      case _ => checkE(e)
    }

    override def checkE(expr: Expr)(implicit env: Env): Env =
      mergeCheckE(myCheckE)(expr, env)
    override def checkC(cmd: Command)(implicit env: Env): Env =
      mergeCheckC(myCheckC)(cmd, env)

  }
}
