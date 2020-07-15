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
object LowerForLoops {

  def rewrite(p: Prog) = ForToWhile.rewrite(p)

  private final case object ForToWhile extends PartialTranformer {
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

    override def myRewriteC: PF[(Command, Env), (Command, Env)] = {
      case (CFor(range, pipeline, par, combine), env) => {
        if (pipeline) throw NotImplemented("Lowering pipelined for loops.")

        val CRange(it, typ, s, e, u) = range
        if (u != 1) throw NotImplemented("Lowering unrolled for loops.")

        // Generate a let bound variable sequenced with a while loop that
        // updates the iterator value.
        val itVar = EVar(it)
        itVar.typ = typ

        val (init, upd, cond, nEnv) = typ match {
          case Some(t) => {
            val add = NumOp("+", OpConstructor.add)
            val init = CLet(it, typ, Some(ECast(EInt(s), t)))
            val upd = CUpdate(itVar, EBinop(add, itVar, ECast(EInt(1), t)))
            val cond = EBinop(CmpOp("<"), itVar, ECast(EInt(e), t))
            (init, upd, cond, env.add(it, t))
          }
          case None => {
            val add = NumOp("+", OpConstructor.add)
            val init = CLet(it, typ, Some(EInt(s)))
            val upd = CUpdate(itVar, EBinop(add, itVar, EInt(1)))
            val cond = EBinop(CmpOp("<"), itVar, EInt(e))
            (init, upd, cond, env)
          }
        }

        // Rewrite par and combine
        val (npar, _) = rewriteC(par)(nEnv)
        val (ncombine, _) = rewriteC(combine)(nEnv)
        val body = CSeq(CSeq(npar, ncombine), upd)
        CSeq(init, CWhile(cond, false, body)) -> nEnv
      }
    }

    override def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
      case (v @ EVar(id), env) => {
        env.get(id) match {
          case Some(t) => v.typ = Some(t)
          case None => ()
        }
        v -> env
      }
    }

    override def rewriteC(cmd: Command)(implicit env: Env) =
      (myRewriteC.orElse(partialRewriteC))(cmd, env)

    override def rewriteE(expr: Expr)(implicit env: Env) = {
      super.transferType(expr, myRewriteE.orElse(partialRewriteE)(_, _))(env)
    }

  }

}
