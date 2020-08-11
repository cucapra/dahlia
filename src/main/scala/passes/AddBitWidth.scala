package fuselang.passes

import scala.{PartialFunction => PF}
import fuselang.Utils._
import fuselang.common._
import EnvHelpers.ScopeManager
import Transformer._
import Syntax._
import CompilerError.PassError
import fuselang.typechecker.Subtyping

// Add bitwidth information to all leaves of a binary expression by adding
// case expressions.
object AddBitWidth extends TypedPartialTransformer {

  case class ABEnv(curTyp: Option[Type]) extends ScopeManager[ABEnv] {
    def merge(that: ABEnv) = {
      assert(this == that, "Tried to merge different bitwidth envs")
      this
    }
  }

  type Env = ABEnv
  val emptyEnv = ABEnv(None)

  def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
    case (e: ECast, env) => e -> env
    case (e @ EArrAccess(arrId, idxs), env) => {
      val Some(TArray(_, dims, _)) = arrId.typ
      val nIdxs = idxs
        .zip(dims)
        .map({
          case (idx, (size, _)) => {
            val adaptorBits = bitsNeeded(size)
            val TSizedInt(idxBits, _) = idx.typ.get
            val ne = rewriteE(idx)(ABEnv(None))._1
            if (adaptorBits != idxBits) {
              ECast(ne, TSizedInt(adaptorBits, true))
            } else {
              ne
            }
          }
        })
      e.copy(idxs = nIdxs) -> env
    }
    case (e: EInt, env) =>
      if (env.curTyp.isDefined) {
        (ECast(e, env.curTyp.get), env)
      } else {
        e -> env
      }
    case (expr @ EBinop(op: EqOp, l, r), env) => {
      val typ = Subtyping.joinOf(l.typ.get, r.typ.get, op)
      val nEnv = ABEnv(
        Some(typ).getOrThrow(PassError("No join for comparision"))
      )
      val (nl, _) = rewriteE(l)(nEnv)
      val (nr, _) = rewriteE(r)(nEnv)
      expr.copy(e1 = nl, e2 = nr) -> env
    }
    case (expr @ EBinop(_: NumOp | _: BitOp, l, r), env) => {
      val nEnv = if (env.curTyp.isDefined) {
        env
      } else {
        ABEnv(
          Some(expr.typ.getOrThrow(PassError("Expression is missing type")))
        )
      }
      val (nl, _) = rewriteE(l)(nEnv)
      val (nr, _) = rewriteE(r)(nEnv)
      expr.copy(e1 = nl, e2 = nr) -> env
    }
  }

  def myRewriteC: PF[(Command, Env), (Command, Env)] = {
    case (cmd @ CLet(_, typ, Some(e)), env) => {
      val nEnv = ABEnv(
        Some(typ.getOrThrow(PassError("Binding is missing type")))
      )
      val (ne, _) = rewriteE(e)(nEnv)
      cmd.copy(e = Some(ne)) -> env
    }
  }

  override def transferType(expr: Expr, f: PF[(Expr, Env), (Expr, Env)])(
      implicit env: Env
  ): (Expr, Env) = {
    val (e1, env1) = f(expr, env)
    val nTyp = e1 match {
      case ECast(_, t) => {
        Some(t)
      }
      case _ => expr.typ
    }
    e1.typ = nTyp
    (e1, env1)
  }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)
  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)
}
