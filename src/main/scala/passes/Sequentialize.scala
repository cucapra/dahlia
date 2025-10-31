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
      // the set of parallel commands, along with the defines and uses
      val newSeq: Buffer[(Buffer[Command], SetM[Id], SetM[Id])] = Buffer((Buffer(), SetM(), SetM()))

      for cmd <- cmds do
        val (nCmd, e1) = rewriteC(cmd)(emptyEnv)
        var added: Boolean = false
        for (pars, curDefines, curUses) <- newSeq do {
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
          if !added && curDefines.intersect(e1.uses).isEmpty && curUses.intersect(e1.defines).isEmpty then {
            pars += nCmd
            curDefines ++= e1.defines
            curUses ++= e1.uses
            added = true
          }
          else if !added then {
            // There was *some* conflict, so nCmd will be added to a "later" block.
            // We still need to update curDefines and curUses since cmds that come after nCmd should have the same
            // conflicts
            curDefines ++= e1.defines
            curUses ++= e1.uses
          }
        }
        // All of the parallel blocks had conflicts in them
        if !added then {
          val currDefines: SetM[Id] = SetM()
          val currUses: SetM[Id] = SetM()
          currDefines ++= e1.defines
          currUses ++= e1.uses
          val newEntry: (Buffer[Command], SetM[Id], SetM[Id]) = (Buffer(nCmd), currDefines, currUses)
          newSeq += newEntry
        }
        // If there are no conflicts, add this to the current parallel
        // block.
        allDefines ++= e1.defines
        allUses ++= e1.uses

      // Add all the uses and defines from this loop into the summary.
      val allEnv = SeqEnv(allUses.toSet, allDefines.toSet, false).merge(env)

      CSeq.smart(newSeq.map((ps, _, _) => CPar.smart(ps.toSeq)).toSeq) -> allEnv
    }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)
  // No need to traverse expressions
  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)
