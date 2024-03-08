package fuselang.passes

import scala.{PartialFunction => PF}
import scala.collection.mutable.{Set => SetM, Buffer}
import fuselang.common._
import Transformer._
import EnvHelpers._
import Syntax._
import CompilerError._

object Sequentialize extends PartialTransformer:

  case class SeqEnv(uses: Set[Id], defines: Set[Id], useLHS: Boolean)
      extends ScopeManager[SeqEnv]:
    def merge(that: SeqEnv) =
      assert(
        this.useLHS == that.useLHS,
        "Attempting to merge environment with different useLHS"
      )
      SeqEnv(
        this.uses union that.uses,
        this.defines union that.defines,
        this.useLHS
      )
    def addUse(x: Id) =
      this.copy(uses = this.uses + x)
    def addDefine(x: Id) =
      this.copy(defines = this.defines + x)
    def setUseLHS(useLHS: Boolean) =
      this.copy(useLHS = useLHS)

  type Env = SeqEnv
  val emptyEnv = SeqEnv(Set(), Set(), false)

  def myRewriteE: PF[(Expr, Env), (Expr, Env)] =
    case (e @ EVar(id), env) => e -> env.addUse(id)
    case (e @ EArrAccess(id, idxs), env) => {
      val (nIdxs, e1) = rewriteESeq(idxs)(env)
      e.copy(idxs = nIdxs.toSeq) -> e1.addUse(id)
    }
    case (e: EPhysAccess, _) =>
      throw NotImplemented("Physical accesses in sequentialize", e.pos)

  override def rewriteLVal(e: Expr)(implicit env: SeqEnv): (Expr, SeqEnv) =
    e match
      case EVar(id) => {
        val env1 = if env.useLHS then env.addUse(id) else env
        e -> env1.addDefine(id)
      }
      case e @ EArrAccess(id, idxs) => {
        val env1 = if env.useLHS then env.addUse(id) else env
        val (nIdxs, e1) = rewriteESeq(idxs)(env1)
        e.copy(idxs = nIdxs.toSeq) -> e1.addDefine(id)
      }
      case e: EPhysAccess =>
        throw NotImplemented("Physical accesses in sequentialize", e.pos)
      case e =>
        throw Impossible(s"Not an LVal: ${Pretty.emitExpr(e)(false).pretty}")

  def myRewriteC: PF[(Command, Env), (Command, Env)] =
    case (CUpdate(lhs, rhs), env) => {
      val (nRhs, e1) = rewriteE(rhs)(env)
      val (nLhs, e2) = rewriteLVal(lhs)(e1)
      CUpdate(nLhs, nRhs) -> e2
    }
    case (c @ CReduce(_, lhs, rhs), env) => {
      val (nRhs, e1) = rewriteE(rhs)(env)
      val (nLhs, e2) = rewriteLVal(lhs)(e1.setUseLHS(true))
      c.copy(lhs = nLhs, rhs = nRhs) -> e2.setUseLHS(false)
    }
    case (c @ CLet(id, _, Some(init)), env) => {
      val (nInit, e1) = rewriteE(init)(env)
      c.copy(e = Some(nInit)) -> e1.addDefine(id)
    }
    case (CPar(cmds), env) => {
      val allDefines: SetM[Id] = SetM()
      val allUses: SetM[Id] = SetM()
      var curDefines: SetM[Id] = SetM()
      var curUses: SetM[Id] = SetM()
      val newSeq: Buffer[Buffer[Command]] = Buffer(Buffer())

      for cmd <- cmds do
        val (nCmd, e1) = rewriteC(cmd)(emptyEnv)
        /* System.err.println(Pretty.emitCmd(cmd)(false).pretty)
        System.err.println(s"""
        uses: ${e1.uses}
        defines: ${e1.defines}
        curDefines: ${curDefines}
        curUses: ${curUses}
        conflicts: ${curDefines.intersect(e1.uses) union curUses.intersect(
          e1.defines
        )}
        =====================
        """) */
        // If there are no conflicts, add this to the current parallel
        // block.
        if curDefines.intersect(e1.uses).isEmpty &&
            curUses.intersect(e1.defines).isEmpty then
          newSeq.last += nCmd
        else
          curUses = SetM()
          curDefines = SetM()
          newSeq += Buffer(nCmd)
        curUses ++= e1.uses
        curDefines ++= e1.defines
        allDefines ++= e1.defines
        allUses ++= e1.uses

      // Add all the uses and defines from this loop into the summary.
      val allEnv = SeqEnv(allUses.toSet, allDefines.toSet, false).merge(env)

      CSeq.smart(newSeq.map(ps => CPar.smart(ps.toSeq)).toSeq) -> allEnv
    }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)
  // No need to traverse expressions
  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)
