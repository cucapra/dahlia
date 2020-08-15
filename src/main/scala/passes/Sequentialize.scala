package fuselang.passes

import scala.{PartialFunction => PF}
import fuselang.common._
import Transformer._
import EnvHelpers._
import Syntax._
import CompilerError._

object Sequentialize extends PartialTransformer {

  case class SeqEnv(uses: Set[Id]) extends ScopeManager[SeqEnv] {
    def merge(that: SeqEnv) = {
      SeqEnv(this.uses union that.uses)
    }
    def add(x: Id) =
      this.copy(uses = this.uses + x)
  }

  type Env = SeqEnv
  val emptyEnv = SeqEnv(Set())

  def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
    case (e @ EVar(id), env) => e -> env.add(id)
    case (e @ EArrAccess(id, _), env) => e -> env.add(id)
    case (e @ EPhysAccess(id, _), env) => e -> env.add(id)
  }

  def myRewriteC: PF[(Command, Env), (Command, Env)] = {
    case (c@CUpdate(lhs, rhs), env) => {
      val (nRhs, e1) = rewriteE(rhs)(env)
      val i = lhs match {
        case EVar(id) => id
        case EArrAccess(id, _) => id
        case EPhysAccess(id, _) => id
        case _ => throw Impossible("Not an LVal")
      }
      c.copy(rhs = nRhs) -> e1.add(i)
    }
    case (c@CReduce(_, lhs, rhs), env) => {
      val (nRhs, e1) = rewriteE(rhs)(env)
      val i = lhs match {
        case EVar(id) => id
        case EArrAccess(id, _) => id
        case EPhysAccess(id, _) => id
        case _ => throw Impossible("Not an LVal")
      }
      c.copy(rhs = nRhs) -> e1.add(i)
    }
    case (c@CLet(id, _, Some(init)), env) => {
      val (nInit, e1) = rewriteE(init)(env)
      c.copy(e = Some(nInit)) -> e1.add(id)
    }
    case (CPar(cmds), _) => {
      import scala.collection.mutable.{Set => SetM, Buffer}
      val allConflicts: SetM[Id] = SetM()
      var curConficts: SetM[Id] = SetM()
      val newSeq: Buffer[Buffer[Command]] = Buffer(Buffer())

      for (cmd <- cmds) {
        val (nCmd, e1) = rewriteC(cmd)(SeqEnv(Set()))
        if (curConficts.intersect(e1.uses).size == 0) {
          curConficts ++= e1.uses
          newSeq.last += nCmd
        } else {
          curConficts = SetM()
          newSeq += Buffer(nCmd)
        }
        allConflicts ++= e1.uses
      }
      CSeq.smart(newSeq.map(ps => CPar.smart(ps.toSeq)).toSeq) -> SeqEnv(
        allConflicts.toSet
      )
    }
  }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)
  // No need to traverse expressions
  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)
}
