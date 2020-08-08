package fuselang.passes

import scala.{PartialFunction => PF}
import fuselang.common._
import Transformer._
import EnvHelpers._
import Syntax._

object HoistMemoryReads extends PartialTransformer {

  // Env for storing the assignments for reads to replace
  case class BufferEnv(map: Map[Expr, CLet])
      extends ScopeManager[BufferEnv]
      with Tracker[Expr, CLet, BufferEnv] {
    def merge(that: BufferEnv) = {
      BufferEnv(this.map ++ that.map)
    }

    def get(key: Expr) = this.map.get(key)

    def add(key: Expr, value: CLet) = {
      BufferEnv(this.map + (key -> value))
    }
  }

  type Env = BufferEnv
  val emptyEnv = BufferEnv(Map())

  /** Helper for generating unique names. */
  var idx: Map[String, Int] = Map();
  def genName(base: String): Id = {
    // update idx
    idx.get(base) match {
      case Some(n) => idx = idx + (base -> (n + 1))
      case None => idx = idx + (base -> 0)
    }
    Id(s"$base${idx(base)}")
  }

  /** Constructs a (Command, Env) tuple from a command
    * and an environment containing new let bindings. */
  def construct(
      cmd: Command,
      env: Env,
      acc: Command = CEmpty
  ): (Command, Env) = {
    if (env.map.values.isEmpty && acc == CEmpty) {
      cmd -> emptyEnv
    } else {
      CPar.smart(env.map.values.toSeq :+ acc :+ cmd) -> emptyEnv
    }
  }

  /** Replaces array accesses with reads from a temporary variable.
    * Inserts a let binding into the Env and relies on the rewriteC.
    * to insert this into the code. */
  def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
    case (e @ EArrAccess(id, _), env) => {
      env.get(e) match {
        case Some(let) => EVar(let.id) -> env
        case None => {
          val readTmp = genName(s"${id}_read")
          val read = CLet(readTmp, None, Some(e))
          val nEnv = env.add(e, read)
          EVar(readTmp) -> nEnv
        }
      }
    }
  }

  /** Simple wrapper that calls rewriteC with an emptyEnv
    * and projects the first element of the result. */
  def rewrC(c: Command): Command = {
    rewriteC(c)(emptyEnv)._1
  }

  def myRewriteC: PF[(Command, Env), (Command, Env)] = {
    // no reason to rewrite direct reads into a variable
    case (c @ CLet(_, _, Some(EArrAccess(_, _))), _) => {
      c -> emptyEnv
    }

    case (CLet(id, typ, Some(e)), _) => {
      val (expr, env) = rewriteE(e)(emptyEnv)
      construct(CLet(id, typ, Some(expr)), env)
    }

    case (CIf(cond, cons, alt), _) => {
      val (expr, env) = rewriteE(cond)(emptyEnv)
      construct(
        CIf(expr, rewrC(cons), rewrC(alt)),
        env
      )
    }

    case (CWhile(cond, pipeline, body), _) => {
      val (expr, env) = rewriteE(cond)(emptyEnv)
      construct(CWhile(expr, pipeline, rewrC(body)), env)
    }

    case (CReturn(expr), _) => {
      val (rewriteExpr, env) = rewriteE(expr)(emptyEnv)
      construct(CReturn(rewriteExpr), env)
    }

    case (CExpr(expr), _) => {
      val (rewrite, env) = rewriteE(expr)(emptyEnv)
      construct(CExpr(rewrite), env)
    }

    /*case (CUpdate(e @ EArrAccess(id, _), rhs), _) => {
      val (rhsRewrite, env) = rewriteE(rhs)(emptyEnv)
      val writeTmp = genName(s"${id}_write")
      val writeLet = CLet(writeTmp, e.typ, Some(rhsRewrite))
      construct(CUpdate(e, EVar(writeTmp)), env, writeLet)
    }*/

    case (CUpdate(e, rhs), _) => {
      val (rewrite, env) = rewriteE(rhs)(emptyEnv)
      construct(CUpdate(e, rewrite), env)
    }

    /*case (CReduce(rop, e @ EArrAccess(id, _), rhs), _) => {
      val writeTmp = genName(s"${id}_write")
      val (rewrite, env) = rewriteE(rhs)(emptyEnv)
      val writeLet = CLet(writeTmp, e.typ, Some(rewrite))
      construct(CReduce(rop, e, EVar(writeTmp)), env, writeLet)
    }*/

    case (CReduce(rop, e, rhs), _) => {
      val (rewrite, env) = rewriteE(rhs)(emptyEnv)
      construct(CReduce(rop, e, rewrite), env)
    }
  }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)

  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)
}