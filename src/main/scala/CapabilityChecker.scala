package fuselang

import scala.{PartialFunction => PF}

import Syntax._
import Syntax.Annotations._
import CapabilityEnv._
import Errors._

import Checker._

object CapabilityChecker {

  def check(p: Prog) = CapChecker.check(p)

  private final case object CapChecker extends Checker {

    type Env = CapabilityEnv

    def check(p: Prog): Unit = {
      val Prog(_, defs, _, cmd) = p

      defs.collect({ case FuncDef(_, _, bodyOpt) => bodyOpt.map(checkC(_)(emptyEnv)) })

      checkC(cmd)(emptyEnv); ()
    }

    def myCheckE: PF[(Expr, Env), Env] = {
      case (acc@EArrAccess(_, idxs), env) => {
        val (nEnv, consumableAnn, cap) = env.get(acc) match {
          case Some(Write) => throw InvalidCap(acc, Read, Write)
          case Some(Read) => (env, SkipConsume, Read)
          case None => (checkESeq(idxs)(env), ShouldConsume, Read)
        }
        acc.consumable = Some(consumableAnn)
        nEnv.add(acc, cap)
      }
    }

    def myCheckC: PF[(Command, Env), Env] = {
      case (CSeq(c1, c2), env) => checkC(c1)(env); checkC(c2)(env)
    }

    /**
     * This doesn't need to be partial function since it deals with all
     * cases in checkLVal.
     */
    override def checkLVal(e: Expr)(implicit env: Env) = e match {
      case acc@EArrAccess(_, idxs) => {
        val (nEnv, consumableAnn, cap) = env.get(e) match {
          case Some(Write) => throw AlreadyWrite(e)
          case Some(Read) => throw InvalidCap(e, Write, Read)
          case None => (checkESeq(idxs), ShouldConsume, Write)
        }

        acc.consumable = Some(consumableAnn)
        nEnv.add(e, cap)
      }
      case _ => checkE(e)
    }

    /**
     * Override the checkE function used for checking
     */
    override def checkE(expr: Expr)(implicit env: Env): Env =
      myCheckE.orElse(asPartial(super.checkE(_: Expr)(_: Env)))((expr, env))

    override def checkC(cmd: Command)(implicit env: Env): Env =
      myCheckC.orElse(asPartial(super.checkC(_: Command)(_: Env)))((cmd, env))
  }
}
