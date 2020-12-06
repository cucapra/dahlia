package fuselang.passes

import scala.{PartialFunction => PF}
import fuselang.common._
import Transformer._
import EnvHelpers._
import Syntax._
import CompilerError._

object Sequentialize extends PartialTransformer {

  case class SeqEnv(uses: Set[Id], defines: Set[Id])
      extends ScopeManager[SeqEnv] {
    def merge(that: SeqEnv) = {
      SeqEnv(this.uses union that.uses, this.defines union that.defines)
    }
    def add(x: Id) =
      this.copy(uses = this.uses + x)
    def addDefine(x: Id) =
      this.copy(defines = this.defines + x)
  }

  type Env = SeqEnv
  val emptyEnv = SeqEnv(Set(), Set())

  def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
    case (e @ EVar(id), env) => e -> env.add(id)
    case (e @ EArrAccess(id, idxs), env) => {
      val (nIdxs, e1) = rewriteESeq(idxs)(env)
      e.copy(idxs = nIdxs.toSeq) -> e1.add(id)
    }
    case (e: EPhysAccess, _) =>
      throw NotImplemented("Physical accesses in sequentialize", e.pos)
  }

  override def rewriteLVal(e: Expr)(implicit env: SeqEnv): (Expr, SeqEnv) =
    e match {
      case EVar(id) => e -> env.addDefine(id)
      case e @ EArrAccess(id, idxs) => {
        val (nIdxs, e1) = rewriteESeq(idxs)(env)
        e.copy(idxs = nIdxs.toSeq) -> e1.addDefine(id)
      }
      case e: EPhysAccess =>
        throw NotImplemented("Physical accesses in sequentialize", e.pos)
      case e =>
        throw Impossible(s"Not an LVal: ${Pretty.emitExpr(e)(false).pretty}")
    }

  def myRewriteC: PF[(Command, Env), (Command, Env)] = {
    case (CUpdate(lhs, rhs), env) => {
      val (nRhs, e1) = rewriteE(rhs)(env)
      val (nLhs, e2) = rewriteLVal(lhs)(e1)
      CUpdate(nLhs, nRhs) -> e2
    }
    case (c @ CReduce(_, lhs, rhs), env) => {
      val (nRhs, e1) = rewriteE(rhs)(env)
      val (nLhs, e2) = rewriteLVal(lhs)(e1)
      c.copy(lhs = nLhs, rhs = nRhs) -> e2
    }
    case (c @ CLet(id, _, Some(init)), env) => {
      val (nInit, e1) = rewriteE(init)(env)
      c.copy(e = Some(nInit)) -> e1.addDefine(id)
    }
    case (CPar(cmds), _) => {
      import scala.collection.mutable.{Set => SetM, Buffer}
      val allDefines: SetM[Id] = SetM()
      val allUses: SetM[Id] = SetM()
      var curDefines: SetM[Id] = SetM()
      var curUses: SetM[Id] = SetM()
      val newSeq: Buffer[Buffer[Command]] = Buffer(Buffer())

      for (cmd <- cmds) {
        val (nCmd, e1) = rewriteC(cmd)(SeqEnv(Set(), Set()))
        /*println(Pretty.emitCmd(cmd)(false).pretty)
        println(s"""
          uses: ${e1.uses}
          defines: ${e1.defines}
          curDefines: ${curDefines}
          curUses: ${curUses}
          conflicts: ${curDefines.intersect(e1.uses).size == 0 && curUses.intersect(e1.defines).size == 0}
          =====================
          """)*/
        if (curDefines.intersect(e1.uses).isEmpty &&
            curUses.intersect(e1.defines).isEmpty) {
          newSeq.last += nCmd
        } else {
          curUses = SetM()
          curDefines = SetM()
          newSeq += Buffer(nCmd)
        }
        curUses ++= e1.uses
        curDefines ++= e1.defines
        allDefines ++= e1.defines
        allUses ++= e1.uses
      }
      CSeq.smart(newSeq.map(ps => CPar.smart(ps.toSeq)).toSeq) -> SeqEnv(
        allUses.toSet,
        allDefines.toSet
      )
    }
  }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)
  // No need to traverse expressions
  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)
}
