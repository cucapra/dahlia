package fuselang

import Syntax._
import CodeGenHelpers._
import CompilerError._

import Utils.RichOption

/**
 * AST pass to rewrite views into simple array accesses. Should be used after
 * type checking.
 *
 * TODO(rachit): Update description.
 *
 * If `a` itself is a view, we keep rewriting it until we reach a true array.
 *
 * For information about the monadic implementation, refer to the docs for
 * [[fuselang.StateHelper.State]].
 */
object RewriteView {
  import StateHelper._

  /**
   * Represents a mapping from view ids to a function that takes all dimension
   * expressions returns a new array access expression
   */
  type Env = Map[Id, List[Expr] => Expr]

  private def rewriteExpr(e: Expr): State[Env, Expr] = e match {
    case EVar(_) | EInt(_, _) | EFloat(_) | EBool(_) | _:ERecAccess => State.unit(e)
    case ec@ECast(e, _) => for {
      en <- rewriteExpr(e)
    } yield ec.copy(e = en)
    case eb@EBinop(_, e1, e2) => for {
      e1n <- rewriteExpr(e1)
      e2n <- rewriteExpr(e2)
    } yield eb.copy(e1 = e1n, e2 = e2n)
    case eaa@EArrAccess(arrId, idxs) => for {
      idxsn <- State.mapList(rewriteExpr)(idxs)
      state <- State.get
      ret <- if (state.contains(arrId)) rewriteExpr(state(arrId)(idxsn))
             else State.unit[Env, Expr](eaa.copy(idxs = idxsn))
    } yield ret
    case app@EApp(_, args) => for {
      argsn <- State.mapList(rewriteExpr)(args)
    } yield app.copy(args = argsn)
    case ERecLiteral(fs) => for {
      fsn <- State.mapMap(rewriteExpr)(fs)
    } yield ERecLiteral(fsn)
    case EArrLiteral(idxs) => for {
      idxsn <- State.mapList(rewriteExpr)(idxs)
    } yield EArrLiteral(idxsn)
  }

  private def genViewAccessExpr(view: View, idx: Expr): Expr = view.suffix match {
    case Aligned(factor, e2) => (EInt(factor) * e2) + idx
    case Rotation(e) => e + idx
  }

  private def splitAccessExpr(i: Expr, j: Expr, arrBank: Int, viewBank: Int): Expr =
    (i * EInt(viewBank)) +
    ((j / EInt(viewBank)) * EInt(arrBank)) +
    (j % EInt(viewBank))

  private def rewriteC(c: Command): State[Env, Command] = c match {
    case CPar(c1, c2) => for {
      c1n <- rewriteC(c1)
      c2n <- rewriteC(c2)
    } yield CPar(c1n, c2n)
    case CSeq(c1, c2) => for {
      c1n <- rewriteC(c1)
      c2n <- rewriteC(c2)
    } yield CSeq(c1n, c2n)
    case l@CLet(_, _, e) => for {
      en <- State.mapOpt(rewriteExpr)(e)
    } yield l.copy(e = en)
    case CView(id, arrId, dims) => State { env =>
      val f = (es: List[Expr]) => EArrAccess(arrId, es.zip(dims).map({ case (idx, view) =>
        genViewAccessExpr(view, idx)
      }))
      (CEmpty, env + (id -> f))
    }
    case CSplit(id, arrId, factors) => State { env =>
      val arrBanks = arrId
        .typ
        .getOrThrow(Impossible(s"$arrId is missing type in $c")) match {
          case TArray(_, dims) => dims.map(_._2)
          case t => throw Impossible(s"Array has type $t in $c")
        }
      val f = (es: List[Expr]) => {
        val it = es.iterator
        // For each dimension, if it was split by more than 1, group the next
        // two accessors.
        val groups = factors.map({
          case factor => List(it.next, it.next) -> factor
        })
        val idxs = groups.zip(arrBanks).map({
          case ((List(i), _), _) => i
          case ((List(i, j), factor), arrBank) => splitAccessExpr(i, j, arrBank, arrBank / factor)
        })
        EArrAccess(arrId, idxs)
      }
      (CEmpty, env + (id -> f))
    }
    case CIf(e1, c1, c2) => for {
      e1n <- rewriteExpr(e1)
      c1n <- rewriteC(c1)
      c2n <- rewriteC(c2)
    } yield CIf(e1n, c1n, c2n)
    case cf@CFor(_, c1, c2) => for {
      c1n <- rewriteC(c1)
      c2n <- rewriteC(c2)
    } yield cf.copy(par = c1n, combine = c2n)
    case cw@CWhile(_, c) => for {
      cn <- rewriteC(c)
    } yield cw.copy(body = cn)
    case CUpdate(e1, e2) => for {
      e1n <- rewriteExpr(e1)
      e2n <- rewriteExpr(e2)
    } yield CUpdate(e1n, e2n)
    case cr@CReduce(_, e1, e2) => for {
      e1n <- rewriteExpr(e1)
      e2n <- rewriteExpr(e2)
    } yield cr.copy(lhs = e1n, rhs = e2n)
    case CExpr(exp) => for {
      e1n <- rewriteExpr(exp)
    } yield CExpr(e1n)
    case CEmpty => State.unit(c)
  }

  def rewriteProg(p: Prog): Prog = {
    val emptyEnv = Map[Id, List[Expr] => Expr]()
    val fs = p.defs.map(defi => defi match {
      case fdef@FuncDef(_, _, bOpt) => fdef.copy(bodyOpt = bOpt.map(b => rewriteC(b)(emptyEnv)._1))
      case _ => defi
    })
    val cmdn = rewriteC(p.cmd)(emptyEnv)._1
    p.copy(defs = fs, cmd = cmdn)
  }
}

private object StateHelper {

  /**
   * Monadic implementation of `State`. The type variable [[S]] refers to
   * an abstract state that is carried through the computations over [[A]].
   *
   * The parameter [[computation]] refers to the entire computation collected
   * upto this point. The functions [[map]] and [[flatMap]] allow us to build
   * up other computations over the current one.
   *
   * Extending AnyVal is an optimization to reduce the overhead of this case
   * class.
   *
   * For more funky examples, read this series of blog posts: http://eed3si9n.com/learning-scalaz/State.html
   *
   */
  case class State[S, A](computation: S => (A, S)) extends AnyVal {

    /**
     * Run this computation with the initial state [[initState]] and return
     * the answer with the final state.
     */
    def apply(initState: S): (A, S) = computation(initState)

    /**
     * Transform the result using the function [[f]]. When the this monad is
     * run, it will first run [[computation]] and apply [[f]] to the result.
     */
    def map[B](f: A => B) = State[S, B] { state =>
      val (value, newState) = computation(state)
      (f(value), newState)
    }

    /**
     * Merge another computation [[f]] into this one. This is done by first
     * running this computation and then passing the result and the state to
     * the next computation.
     */
    def flatMap[B](f: A => State[S, B]) = State[S, B] { state =>
      val (value, newState) = computation(state)
      f(value)(newState)
    }

  }

  object State {
    def unit[S, A](a: A): State[S, A] = State { state => (a, state) }

    /**
     * Type theory note: The code for foldLeft is repetitive. There might
     * be a generic notion of a `lift` function that can turn `map` on a
     * collection into a `map` over the monadic version of that collection.
     *
     * Formally:
     *
     * lift: (C[A] -> (A -> B) -> C[B]) -> (M[C[A]] -> (A -> B) -> M[C[B]])
     *
     * where `C` and `M` are monads (being a functor might suffice).
     * Unfortunately, implementing this in scala requires enabling the
     * `higherKinds` features.
     *
     * Instead, we just manually define these over List and Map which are the
     * two commonly used collections in our AST.
     *
     */
    def mapMap[S, K, V1, V2]
      (f: V1 => State[S, V2])
      (map: Map[K, V1]): State[S, Map[K, V2]] =
        mapList[S, (K, V1), (K, V2)]({
          case (k, v) => f(v).map(v2 => k -> v2)
        })(map.toList).map(_.toMap)

    def mapList[S, A, B](f: A => State[S, B])(es: List[A]): State[S, List[B]] =
      es match {
        case Nil => State.unit(Nil)
        case hd :: tl => for {
          hdn <- f(hd)
          tln <- mapList(f)(tl)
        } yield hdn :: tln
      }

    def mapOpt[S, A, B](f: A => State[S, B])(eOpt: Option[A]): State[S, Option[B]] =
      eOpt match {
        case None => State.unit(None)
        case Some(e) => for {
          en <- f(e)
        } yield Some(en)
      }

    /**
     * Return the current state as a value
     */
    def get[S] = State[S, S] { state => (state, state) }
  }

}
