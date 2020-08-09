package fuselang.passes

import scala.{PartialFunction => PF}
import fuselang.common._
import Transformer._
import EnvHelpers._
import Syntax._
import CompilerError._

object HoistMemoryReads extends PartialTransformer {

  case class HoistEnv(
      // Map from Access expression to the bindings they created.
      map: Map[EArrAccess, Id],
      // Set of values modified by a command.
      defines: Set[Id]
  ) extends ScopeManager[HoistEnv]
      with Tracker[EArrAccess, Id, HoistEnv] {
    def merge(that: HoistEnv) = {
      HoistEnv(this.map ++ that.map, this.defines ++ that.defines)
    }

    def get(key: EArrAccess) = this.map.get(key)

    def add(key: EArrAccess, value: Id) = {
      this.copy(this.map + (key -> value))
    }

    def defineAdd(key: Id) = this.copy(defines = this.defines + key)
  }

  type Env = HoistEnv
  val emptyEnv = HoistEnv(Map(), Set())

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

  // Returns the list of variables this expression reads from.
  def readsFrom(e: Expr): Set[Id] = e match {
    case EVar(id) => Set(id)
    case _: EInt => Set()
    case _ => throw NotImplemented(s"readsFrom calculation for: $e")
  }

  // Return a sequence of let bindings for all of the variables are defined
  // by this environment.
  def construct(
      env: Env
  ): (Seq[Command], Env) = {
    // For all variables that read values being defined, return let-bindings.
    val defines = env.defines
    val (toBind, rest) = env.map
      .partition({
        case (e, _) => e.idxs.flatMap(readsFrom).exists(defines.contains)
      })
    val binds = toBind
      .map({
        case (acc, id) => CLet(id, None, Some(acc))
      })
      .toSeq
    (binds, env.copy(map = rest))
  }

  /** Replaces array accesses with reads from a temporary variable.
    * Inserts a let binding into the Env and relies on the rewriteC.
    * to insert this into the code. */
  def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
    case (e @ EArrAccess(id, _), env) => {
      env.get(e) match {
        case Some(id) => EVar(id) -> env
        case None => {
          val readTmp = genName(s"${id}_read")
          val nEnv = env.add(e, readTmp)
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
    // Memory reads cannot percolate beyond a CSeq boundary. This simply
    // traverses the program in normal order and defines reads from every
    // command
    case (CSeq(cmds), env) => {
      // Traversing in normal order because the let-bindings cannot percolate
      // beyond the ---.
      CSeq.smart(cmds.map(cmd => {
        // Use emptyEnv because rewrites from the surrounding context cannot
        // be used.
        val (nCmd, env) = rewriteC(cmd)(emptyEnv)
        val defs = env.map.map({ case (e, id) => CLet(id, None, Some(e)) }).toSeq
        CPar.smart(defs :+ nCmd)
      })) -> env
    }

    /**
      * IMPORTANT: The par rewriter traverses the program bottom up and
      * tries to percolate memory reads to the topmost scope it can.
      */
    case (CPar(cmds), env) => {
      cmds.foldRight[(Command, HoistEnv)]((CEmpty, env))({
        case (cmd, (acc, env)) => {
          val (nCmd, env1) = rewriteC(cmd)(env)
          val (defs, env2)  = construct(env1)
          CPar.smart(nCmd +: defs :+ acc) -> env2
        }
      })
    }
    case (CLet(id, _, Some(acc @ EArrAccess(_, _))), env) => {
      // Remove this and try to percolate it upwards.
      CEmpty -> env.add(acc, id)
    }

    case (c @ CLet(id, _, Some(e)), env) => {
      val (expr, env1) = rewriteE(e)(env)
      c.copy(e = Some(expr)) -> env1.defineAdd(id)
    }

    case (CIf(cond, cons, alt), env) => {
      val (expr, env1) = rewriteE(cond)(env)
      val (nCons, cEnv) = rewriteC(cons)(emptyEnv)
      val (nAlt, aEnv) = rewriteC(alt)(emptyEnv)
      CIf(
        expr,
        // Discard the environments because we can't percolate
        // bindings outside their scopes.
        CPar.smart(construct(cEnv)._1 :+ nCons),
        CPar.smart(construct(aEnv)._1 :+ nAlt),
      ) -> env1.copy(defines = cEnv.defines ++ aEnv.defines)
    }

    case (CWhile(cond, pipeline, body), env) => {
      val (expr, cEnv) = rewriteE(cond)(emptyEnv)
      val (nBody, bEnv) = rewriteC(body)(cEnv)
      // The body may "define" certain variables by assigning to them.
      // If there are memory accesses in the condition or the body that
      // use such variables, psuh them to the top of the body.
      val (bindings, rEnv) = construct(bEnv)
      CWhile(expr, pipeline, CPar.smart(bindings :+ nBody)) -> env.merge(rEnv)
    }

    case (c@CFor(range, _, body, _), env) => {
      val (nBody, bEnv) = rewriteC(body)(emptyEnv)
      // The iterator in the loop is defined.
      val (bindings, rEnv) = construct(bEnv.defineAdd(range.iter))
      c.copy(par = CPar.smart(bindings :+ nBody)) -> env.merge(env.merge(rEnv))
    }

    /*case (CReturn(expr), _) => {
      val (rewriteExpr, env) = rewriteE(expr)(emptyEnv)
      construct(CReturn(rewriteExpr), env)
    }*/

    /*case (CExpr(expr), _) => {
      val (rewrite, env) = rewriteE(expr)(emptyEnv)
      construct(CExpr(rewrite), env)
    }*/

    /*case (CUpdate(e @ EArrAccess(id, _), rhs), _) => {
      val (rhsRewrite, env) = rewriteE(rhs)(emptyEnv)
      val writeTmp = genName(s"${id}_write")
      val writeLet = CLet(writeTmp, e.typ, Some(rhsRewrite))
      construct(CUpdate(e, EVar(writeTmp)), env, writeLet)
    }*/

    // We naively rewrite memory reads in rhs without considering if the
    // lhs updates it. This is fine because if there LHS updated them,
    // we would have required the read to be moved up anyways.
    case (CUpdate(lhs, rhs), env) => {
      val (nR, env1) = rewriteE(rhs)(env)
      // Set of bindings that cannot be percolated up.
      val defined = lhs match {
        // All access expressions from this array cannot be percolated up.
        case EArrAccess(id, _) => id
        // All access expressions that use this variable cannot be percolated
        // up.
        case EVar(id) => id
        case _ => throw Impossible(s"Not an LHS: $lhs")
      }
      // Update the set of defines
      CUpdate(lhs, nR) -> env1.defineAdd(defined)
    }

    /*case (CReduce(rop, e @ EArrAccess(id, _), rhs), _) => {
      val writeTmp = genName(s"${id}_write")
      val (rewrite, env) = rewriteE(rhs)(emptyEnv)
      val writeLet = CLet(writeTmp, e.typ, Some(rewrite))
      construct(CReduce(rop, e, EVar(writeTmp)), env, writeLet)
    }*/

    case (_:CReduce, _) => ???
    case (_: CBlock, _) => ???
    /*{
      val (rewrite, env) = rewriteE(rhs)(emptyEnv)
      construct(CReduce(rop, e, rewrite), env)
    }*/
  }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)

  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)
}
