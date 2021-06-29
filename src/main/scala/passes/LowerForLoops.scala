package fuselang.passes

import scala.{PartialFunction => PF}
import fuselang.common._
import Transformer._
import EnvHelpers._
import Syntax._
import CompilerError._

/**
  * Lower for loops to while loops.
  */
object LowerForLoops extends PartialTransformer {
  case class ForEnv(map: Map[Id, Type])
      extends ScopeManager[ForEnv]
      with Tracker[Id, Type, ForEnv] {
    def merge(that: ForEnv) = {
      ForEnv(this.map ++ that.map)
    }

    def get(key: Id) = this.map.get(key)

    def add(key: Id, typ: Type) = {
      ForEnv(this.map + (key -> typ))
    }
  }

  type Env = ForEnv
  val emptyEnv = ForEnv(Map())

  def myRewriteC: PF[(Command, Env), (Command, Env)] = {
    case (cfor@CFor(range, pipeline, par, combine), env) => {
      if (pipeline) throw NotImplemented("Lowering pipelined for loops.")

      val CRange(it, typ, rev, s, e, u) = range
      if (u != 1) throw NotImplemented("Lowering unrolled for loops.")

      // Generate a let bound variable sequenced with a while loop that
      // updates the iterator value.
      val itVar = EVar(it)
      itVar.typ = typ

      // Refuse lowering without explicit type on iterator.
      if (typ.isDefined == false) {
        throw NotImplemented(
          "Cannot lower `for` loop without iterator type. Add explicit type for the iterator",
          it.pos
        )
      }

      val t = typ.get
      val init = CLet(it, typ, Some(ECast(if (rev) EInt(e - 1) else EInt(s), t)))
      val upd = if (rev) {
        val sub = NumOp("-", OpConstructor.sub)
        CUpdate(itVar.copy(), EBinop(sub, itVar.copy(), ECast(EInt(1), t)))
      } else {
        val add = NumOp("+", OpConstructor.add)
        CUpdate(itVar.copy(), EBinop(add, itVar.copy(), ECast(EInt(1), t)))
      }
      val cond =
        if (rev) {
          EBinop(CmpOp(">="), itVar.copy(), ECast(EInt(s), t))
        } else {
          EBinop(CmpOp("<="), itVar.copy(), ECast(EInt(e - 1), t))
        }
      val nEnv = env.add(it.copy(), t)

      // Rewrite par and combine
      val (npar, _) = rewriteC(par)(nEnv)
      val (ncombine, _) = rewriteC(combine)(nEnv)
      val body = CSeq.smart(Seq(npar, ncombine, upd))
      val wh = CWhile(cond, false, body)
      wh.attributes = cfor.attributes
      CBlock(CSeq.smart(Seq(init, wh))) -> nEnv
    }
  }

  /** We need to change the types of iterator variables so that
    * they match the iterators annotated type. */
  def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
    case (v @ EVar(id), env) => {
      env.get(id) match {
        case Some(t) => v.typ = Some(t)
        case None => ()
      }
      v -> env
    }
  }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)

}
