package fuselang.passes

import scala.{PartialFunction => PF}
import scala.collection.immutable.ListMap
import fuselang.common._
import Transformer._
import EnvHelpers._
import Syntax._

object HoistMemoryReads extends PartialTransformer {

  // Env for storing the assignments for reads to replace
  case class BufferEnv(
      map: Seq[(Id, ListMap[Expr, CLet])],
      top: ListMap[Expr, CLet]
  ) extends ScopeManager[BufferEnv] {

    def addIdx(idx: Id) = {
      BufferEnv(this.map :+ (idx -> ListMap()), ListMap())
    }

    // Given a set of loop idxs, add the expr -> let map to the shallowest scope, i.e., hoist the expression as far as possible
    def add(key: Expr, value: CLet) = {
      val idxs = key.all_vars.toSet & this.map.map(_._1).toSet
      // If the expression is not dependent on any loop idxs, add it to the top level
      if (idxs.isEmpty) {
        BufferEnv(this.map, this.top + (key -> value))
      } else {
        // Otherwise, add it to the shallowest scope
        val (out, _) = map.reverse
          .foldLeft[
            (Seq[(Id, ListMap[Expr, CLet])], Option[(Expr, CLet)])
          ](
            (Seq(), Some(key -> value))
          ) {
            case ((env, kv), (id, m)) =>
              if (kv.isDefined && idxs.contains(id)) {
                (env :+ (id, m + kv.get), None)
              } else {
                (env :+ (id, m), kv)
              }
          }
        BufferEnv(out.reverse, this.top)
      }
    }

    def get(e: Expr): Option[CLet] = {
      // Find the shallowest scope that contains the expression
      map.reverse.find(_._2.contains(e)).map(_._2(e)) match {
        case Some(let) => Some(let)
        case None => this.top.get(e)
      }
    }

    def merge(other: BufferEnv) = {
      val map = this.map
        .zip(other.map)
        .map({
          case ((id1, m1), (id2, m2)) =>
            assert(id1 == id2)
            id1 -> (m1 ++ m2)
        })
      BufferEnv(map, this.top ++ other.top)
    }

    // Pretty print map
    override def toString = {
      val fmt = (m: ListMap[Expr, CLet]) => {
        m.map({
            case (e, _) =>
              s"${Pretty.emitExpr(e)(false).pretty}"
          })
          .mkString(", ")
      }
      val m = map
        .map({ case (id, m) => s"$id -> [${fmt(m)}]" })
        .mkString(", ")
      s"TOP -> [${fmt(top)}], $m"
    }

  }

  type Env = BufferEnv
  val emptyEnv = BufferEnv(Seq(), ListMap())

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

  /** Constructs a (Command, Env) tuple from a commands in the last element of the sequence of the environment. */
  def construct(
      env: ListMap[Expr, CLet],
      last: Command,
      first: Command = CEmpty
  ): Command = {
    if (env.values.isEmpty && first == CEmpty) {
      last
    } else {
      CPar.smart(first +: env.values.toSeq :+ last)
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
          val read =
            CLet(readTmp, None, Some(EArrAccess(id, nexprs.toSeq))).withPos(e)
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
    case (CFor(range, pipe, par, combine), env) => {
      val (nPar, parEnv) = rewriteC(par)(env.addIdx(range.iter))
      System.err.println(s"PAR: ${parEnv.toString}")
      // Construct all expression bound in the body
      val (_, binds) = parEnv.map.last
      val rest = BufferEnv(parEnv.map.dropRight(1), parEnv.top)
      System.err.println(s"REST: ${rest.toString}")
      val body = construct(binds, nPar)
      // Expression in the combine statement use variables in outer scope.
      val (nCombine, combineEnv) = rewriteC(combine)(rest)
      System.err.println(s"COMB: ${parEnv.toString}")
      val nFor =
        CFor(range, pipe, body, nCombine).withPos(par)
      // If this is the outermost loop, add the top level bindings
      if (rest.map.isEmpty) {
        System.err.println(s"IDX: Index ${range.iter} is outermost")
        val binds = combineEnv.top ++ rest.top
        construct(binds, nFor) -> emptyEnv
      } else {
        nFor -> combineEnv
      }
    }

    case (c @ CUpdate(_, rhs), env) => {
      val (rewrite, nEnv) = rewriteE(rhs)(env)
      val nC = c.copy(rhs = rewrite).withPos(c)
      nC -> nEnv
    }

    case (c @ CReduce(_, _, rhs), env) => {
      val (rewrite, nEnv) = rewriteE(rhs)(env)
      val nC = c.copy(rhs = rewrite).withPos(c)
      nC -> nEnv
    }
  }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)

  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)
}
