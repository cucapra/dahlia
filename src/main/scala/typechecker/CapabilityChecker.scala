package fuselang.typechecker

import scala.{PartialFunction => PF}

import fuselang.common._
import Syntax._
import Syntax.Annotations._
import Errors._
import CapabilityEnvironment._
import Checker._
import EnvHelpers._

object CapabilityChecker {

  def check(p: Prog) = CapChecker.check(p)

  private final case object CapChecker extends PartialChecker {

    type Env = CapabilityEnv

    val emptyEnv = CapabilityEnvironment.emptyEnv

    /**
     * Check an array read.
     * - If there is already a Read capability for this expression, ignore.
     * - If there are no capabilities or a write capability, add a consume
     *   annotation.
     */
    override def myCheckE: PF[(Expr, Env), Env] = {
      case (acc@EArrAccess(_, idxs), env) => {
        val (nEnv, consumableAnn, cap) = env.get(acc) match {
            case Some(Read) => (env, SkipConsume, Read)
            case Some(Write) | None => (checkESeq(idxs)(env), ShouldConsume, Read)
          }
        acc.consumable = Some(consumableAnn)
        nEnv.add(acc, cap)
      }
    }

    override def myCheckC: PF[(Command, Env), Env] = {
      case (CSeq(c1, c2), env) => checkC(c1)(env); checkC(c2)(env)
    }

    /**
     * Check an array write. If there is already a write capability, error.
     * Otherwise try to acquire a write capability.
     *
     * This doesn't need to be partial function since it deals with all
     * cases in checkLVal.
     */
    override def checkLVal(e: Expr)(implicit env: Env) = e match {
      case acc@EArrAccess(_, idxs) => {
        val (nEnv, consumableAnn, cap) = env.get(e) match {
          case Some(Write) => throw AlreadyWrite(e)
          case Some(Read) | None => (checkESeq(idxs), ShouldConsume, Write)
        }

        acc.consumable = Some(consumableAnn)
        nEnv.add(e, cap)
      }
      case _ => checkE(e)
    }

  }
}
