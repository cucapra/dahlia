package fuselang.passes

import scala.{PartialFunction => PF}
import fuselang.common._
import Transformer._
import EnvHelpers._
import Syntax._
import CompilerError._

object LowerUnroll extends PartialTransformer {
  case class ForEnv(idxMap: Map[Id, Int])
      extends ScopeManager[ForEnv]
      with Tracker[Id, Int, ForEnv] {
    def merge(that: ForEnv) = {
      ForEnv(this.idxMap ++ that.idxMap)
    }

    def get(key: Id) = this.idxMap.get(key)

    def add(key: Id, bank: Int) = {
      ForEnv(this.idxMap + (key -> bank))
    }
  }

  type Env = ForEnv
  val emptyEnv = ForEnv(Map())

  def cartesianProduct[T](llst: Seq[Seq[T]]): Seq[Seq[T]] = {

    /**
      * Prepend single element to all lists of list
      * @param e single elemetn
      * @param ll list of list
      * @param a accumulator for tail recursive implementation
      * @return list of lists with prepended element e
      */
    def pel(e: T, ll: Seq[Seq[T]], a: Seq[Seq[T]] = Nil): Seq[Seq[T]] =
      ll match {
        case Nil => a.reverse
        case x +: xs => pel(e, xs, (e +: x) +: a)
      }

    llst match {
      case Nil => Nil
      case x +: Nil => x.map(Seq(_))
      case x +: _ =>
        x match {
          case Nil => Nil
          case _ =>
            llst
              .foldRight(Seq(x))((l, a) => l.flatMap(x => pel(x, a)))
              .map(_.dropRight(x.size))
        }
    }
  }

  def unbankedDecls(id: Id, ta: TArray): Seq[(Id, Type)] = {
    val TArray(typ, dims, ports) = ta
    val out = cartesianProduct(dims.map({
      case (size, banks) => (0 to banks - 1).map((size / banks, _))
    })).map(idxs => {
      val name = id.v + idxs.map(_._2).mkString("_")
      val dims = idxs.map({ case (s, _) => (s, 1) })
      (Id(name), TArray(typ, dims, ports))
    })
    println(out)
    out
  }

  override def rewriteDeclSeq(ds: Seq[Decl])(implicit env: Env) = {
    ds.flatMap(d =>
      d.typ match {
        case ta: TArray =>
          unbankedDecls(d.id, ta).map((x: (Id, Type)) => Decl(x._1, x._2))
        case _ => List(d)
      }
    ) -> env
  }

  def myRewriteC: PF[(Command, Env), (Command, Env)] = {
    case (CLet(id, Some(ta:TArray), None), env) => {
      unbankedDecls(id, ta).foldLeft[Command](CEmpty)({ case (acc, (i, t)) => {
        CPar(acc, CLet(i, Some(t), None))
      }}) -> env
    }
    // Handle case for initialized, unbanked memories.
    case (CLet(id, Some(ta:TArray), init), env) => {
      if (ta.dims.exists({ case (_, bank) => bank > 1 })) {
        throw NotImplemented("Banked local arrays with initial values")
      }
      CLet(id.copy(v = id.v + "0"), Some(ta), init) -> env
    }
    case (c @ CFor(range, pipeline, par, combine), env) => {

      if (range.u > 1 && combine != CEmpty) {
        throw NotImplemented("Unrolled for loops with combine blocks", c.pos)
      }

      if (range.u > 1 && range.s != 0) {
        throw NotImplemented("Unrolling loops with non-zero start idx", c.pos)
      }

      val cmd = (0 to range.u - 1).foldLeft[Command](CEmpty)({
        case (acc, idx) => {
          val nRange = range.copy(e = range.e / range.u, u = 1).copy()
          val nEnv = env.add(range.iter, idx)
          val (npar, _) = rewriteC(par)(nEnv)
          val (ncombine, _) = rewriteC(combine)(nEnv)
          CPar(acc, CFor(nRange, pipeline, npar, ncombine))
        }
      })

      // Refuse lowering without explicit type on iterator.
      cmd -> env
    }
  }

  def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
    case (EArrAccess(id, idxs), env) => {
      val banks: Seq[Int] = idxs.map(idx =>
        idx match {
          case EVar(id) => env.get(id).getOrElse(0)
          case _ => 0
        }
      )
      val arrName = id.v + banks.mkString("_")
      EArrAccess(Id(arrName), idxs) -> env
    }
  }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)

  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)

}
