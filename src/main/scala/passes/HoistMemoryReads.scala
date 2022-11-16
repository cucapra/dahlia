package fuselang.passes

import scala.{PartialFunction => PF}
import scala.collection.immutable.ListMap
import fuselang.common._
import Transformer._
import EnvHelpers._
import Syntax._

object HoistMemoryReads extends PartialTransformer {

  // Env for storing the assignments for reads to replace
  case class BufferEnv(map: ListMap[Expr, CLet] = ListMap())
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
  val emptyEnv = BufferEnv()

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
  ): Command = {
    if (env.map.values.isEmpty && acc == CEmpty) {
      cmd
    } else {
      CPar.smart(env.map.values.toSeq :+ acc :+ cmd)
    }
  }

  /** Replaces array accesses with reads from a temporary variable.
    * Inserts a let binding into the Env and relies on the rewriteC.
    * to insert this into the code. */
  def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
    case (e @ EArrAccess(id, exprs), env) => {
      val (nexprs, env1) =
        rewriteSeqWith[Expr](rewriteE(_: Expr)(_: Env))(exprs)(env)
      env1.get(e) match {
        case Some(let) => EVar(let.id) -> env1
        case None => {
          val readTmp = genName(s"${id}_read")
          val read = CLet(readTmp, None, Some(EArrAccess(id, nexprs.toSeq))).withPos(e)
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
    // Don't rewrite directly-bound array reads. Rewrite access expressions
    // if any.
    case (c @ CLet(_, _, Some(arr @ EArrAccess(_, exprs))), env) => {
      val (nexprs, nEnv) =
        rewriteSeqWith[Expr](rewriteE(_: Expr)(_: Env))(exprs)(env)
      val nC = c.copy(e = Some(arr.copy(idxs = nexprs.toSeq))).withPos(c)
      construct(nC, nEnv) -> emptyEnv
    }

    case (CLet(id, typ, Some(e)), _) => {
      val (expr, env) = rewriteE(e)(emptyEnv)
      construct(CLet(id, typ, Some(expr)), env) -> emptyEnv
    }

    case (CIf(cond, cons, alt), _) => {
      val (expr, env) = rewriteE(cond)(emptyEnv)
      construct(
        CIf(expr, rewrC(cons), rewrC(alt)),
        env
      ) -> emptyEnv
    }

    case (CWhile(cond, pipeline, body), _) => {
      val (expr, env) = rewriteE(cond)(emptyEnv)
      construct(CWhile(expr, pipeline, rewrC(body)), env) -> emptyEnv
    }

    case (CReturn(expr), _) => {
      val (rewriteExpr, env) = rewriteE(expr)(emptyEnv)
      construct(CReturn(rewriteExpr), env) -> emptyEnv
    }

    case (CExpr(expr), _) => {
      val (rewrite, env) = rewriteE(expr)(emptyEnv)
      construct(CExpr(rewrite), env) -> emptyEnv
    }

    /*case (CUpdate(e @ EArrAccess(id, _), rhs), _) => {
      val (rhsRewrite, env) = rewriteE(rhs)(emptyEnv)
      val writeTmp = genName(s"${id}_write")
      val writeLet = CLet(writeTmp, e.typ, Some(rhsRewrite))
      construct(CUpdate(e, EVar(writeTmp)), env, writeLet)
    }*/

    case (c @ CUpdate(_, rhs), _) => {
      val (rewrite, env) = rewriteE(rhs)(emptyEnv)
      val nC = c.copy(rhs=rewrite).withPos(c)
      construct(nC, env) -> emptyEnv
    }

    /*case (CReduce(rop, e @ EArrAccess(id, _), rhs), _) => {
      val writeTmp = genName(s"${id}_write")
      val (rewrite, env) = rewriteE(rhs)(emptyEnv)
      val writeLet = CLet(writeTmp, e.typ, Some(rewrite))
      construct(CReduce(rop, e, EVar(writeTmp)), env, writeLet)
    }*/

    case (CReduce(rop, e, rhs), _) => {
      val (rewrite, env) = rewriteE(rhs)(emptyEnv)
      construct(CReduce(rop, e, rewrite), env) -> emptyEnv
    }
  }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)

  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)
}
