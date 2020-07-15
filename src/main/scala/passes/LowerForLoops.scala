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
    type Env = UnitEnv
    val emptyEnv = UnitEnv()

    override def myRewriteC: PF[(Command, Env), (Command, Env)] = {
      case (CFor(range, pipeline, par, combine), env) => {
        if (pipeline) throw NotImplemented("Lowering pipelined for loops.")

        val CRange(it, typ, s, e, u) = range
        if (u != 1) throw NotImplemented("Lowering unrolled for loops.")

        // Generate a let bound variable sequenced with a while loop that
        // updates the iterator value.
        val itVar = EVar(it)
        itVar.typ = typ
        // val init = CLet(
        //   it,
        //   t, // Some(range.idxType)
        //   Some(EInt(s))
        // )

        val (init, upd, cond) = typ match {
          case Some(t) => {
            val add = NumOp("+", OpConstructor.add)
            val init = CLet(it, typ, Some(ECast(EInt(s), t)))
            val upd = CUpdate(itVar, EBinop(add, itVar, ECast(EInt(1), t)))
            val cond = EBinop(CmpOp("<"), itVar, ECast(EInt(e), t))
            (init, upd, cond)
          }
          case None => {
            val add = NumOp("+", OpConstructor.add)
            val init = CLet(it, typ, Some(EInt(s)))
            val upd = CUpdate(itVar, EBinop(add, itVar, EInt(1)))
            val cond = EBinop(CmpOp("<"), itVar, EInt(e))
            (init, upd, cond)
          }
        }

        // Rewrite par and combine
        val (npar, _) = rewriteC(par)(env)
        val (ncombine, _) = rewriteC(combine)(env)
        val body = CSeq(CSeq(npar, ncombine), upd)
        CSeq(init, CWhile(cond, false, body)) -> env
      }
    }

    override def rewriteC(cmd: Command)(implicit env: Env) =
      (myRewriteC.orElse(partialRewriteC))(cmd, env)

  }

}
