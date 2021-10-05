package fuselang.passes

import scala.{PartialFunction => PF}
import scala.collection.immutable.ListMap
import fuselang.common._
import Syntax._
import CompilerError._
import Transformer._
import EnvHelpers._

object HoistSlowBinop extends TypedPartialTransformer {

  case class ExprEnv(map: ListMap[Expr, CLet])
      extends ScopeManager[ExprEnv]
      with Tracker[Expr, CLet, ExprEnv] {
    def merge(that: ExprEnv) = {
      ExprEnv(this.map ++ that.map)
    }
    def get(key: Expr) = this.map.get(key)
    def add(key: Expr, value: CLet) = {
      ExprEnv(this.map + (key -> value))
    }
  }

  type Env = ExprEnv
  val emptyEnv = ExprEnv(ListMap())

  private val slowBinops = List("*", "/", "%")

  var idx: Map[String, Int] = Map();
  def genName(base: String): Id = {
    // update idx
    idx.get(base) match {
      case Some(n) => idx = idx + (base -> (n + 1))
      case None => idx = idx + (base -> 0)
    }
    Id(s"$base${idx(base)}_")
  }

  def construct(
      cmd: Command,
      env: Env,
      acc: Command = CEmpty
  ): (Command, Env) = {
    if (env.map.values.isEmpty && acc == CEmpty) {
      cmd -> emptyEnv
    } else {
      CSeq.smart(env.map.values.toSeq :+ acc :+ cmd) -> emptyEnv
    }
  }

  def binopRecur(expr: Expr, env: Env): (Expr, Env) = {
    expr match {
      // only recur when children are binops
      case EBinop(_, _, _) => rewriteE(expr)(env)
      case _ => (expr, env)
    }
  }

  def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
    case (e @ EBinop(op, left, right), env) if slowBinops.contains(op.op) => {
      env.get(e) match {
        case Some(let) => (EVar(let.id), env)
        case None => {
          val (leftRead, leftEnv) = binopRecur(left, env)
          val (rightRead, rightEnv) = binopRecur(right, leftEnv)
          val let = CLet(
            genName("bin_read"),
            None,
            Some(EBinop(op, leftRead, rightRead))
          )
          EVar(let.id) -> rightEnv.add(EBinop(op, leftRead, rightRead), let)
        }
      }
    }
  }

  def myRewriteC: PF[(Command, Env), (Command, Env)] = {
    case (CLet(id, typ, Some(e)), _) => {
      val (expr, env) = rewriteE(e)(emptyEnv)
      construct(CLet(id, typ, Some(expr)), env)
    }
    case (CIf(cond, cons, alt), _) => {
      val (expr, env) = rewriteE(cond)(emptyEnv)
      construct(
        CIf(expr, rewriteC(cons)(emptyEnv)._1, rewriteC(alt)(emptyEnv)._1),
        env
      )
    }
    case (wh @ CWhile(cond, _, body), _) => {
      val (nCond, env) = rewriteE(cond)(emptyEnv)
      val (nBody, _) = rewriteC(body)(env)
      val nWh = wh.copy(cond = nCond, body = nBody)
      nWh.attributes = wh.attributes
      construct(nWh, env)
    }
    case (CUpdate(lhs, rhs), _) => {
      val (rewrLhs, env) = rewriteE(lhs)(emptyEnv)
      val (rewrRhs, nEnv) = rewriteE(rhs)(env)
      construct(CUpdate(rewrLhs, rewrRhs), nEnv)
    }
    case (CReduce(rop, lhs, rhs), _) => {
      rop.op match {
        case "*=" | "/=" => throw NotImplemented(s"Hoisting $rop.op", rop.pos)
        case _ => ()
      }
      val (rewrLhs, env) = rewriteE(lhs)(emptyEnv)
      val (rewrRhs, nEnv) = rewriteE(rhs)(env)
      construct(CReduce(rop, rewrLhs, rewrRhs), nEnv)
    }
    case (CReturn(expr), _) => {
      val (nExpr, env) = rewriteE(expr)(emptyEnv)
      construct(CReturn(nExpr), env)
    }
    case (CExpr(expr), _) => {
      val (nExpr, env) = rewriteE(expr)(emptyEnv)
      construct(CExpr(nExpr), env)
    }
  }

  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)
  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)
}
