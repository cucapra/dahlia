package fuselang

import Syntax._
import CodeGenHelpers._

/**
 * AST pass to rewrite views into simple array accesses. Should be used after
 * type checking.
 *
 * For `shrink` views, we rewrite them as:
 *
 * view v_a = shrink a[4 * i : 4];
 * v_a[k];
 * ==>
 * ; // remove the view decl
 * a[4*i + k]
 *
 * If `a` itself is a view, we keep rewriting it until we reach a true array.
 */
object RewriteView {
  import Utils.State

  type T = Map[Id, List[Expr] => Expr]

  def rewriteExpr(e: Expr): State[T, Expr] = e match {
    case EVar(_) | EInt(_, _) | EFloat(_) | EBool(_) | _:ERecAccess => State.unit(e)
    case eb@EBinop(_, e1, e2) => for {
      e1n <- rewriteExpr(e1)
      e2n <- rewriteExpr(e2)
    } yield eb.copy(e1 = e1n, e2 = e2n)
    case eaa@EArrAccess(arrId, idxs) => State { env =>
      val (idxsn, env1) = State.foldLeft(rewriteExpr)(idxs)(env)
      // If the array id is a view array rewrite it and recurr. The recursive
      // call is needed to handle views on views.
      if (env.contains(arrId)) {
        rewriteExpr(env(arrId)(idxsn))(env1)
      } else {
        eaa.copy(idxs = idxsn) -> env1
      }
    }
    case app@EApp(_, args) => for {
      argsn <- State.foldLeft(rewriteExpr)(args)
    } yield app.copy(args = argsn)
  }

  def rewriteC(c: Command): State[T, Command] = c match {
    case CPar(c1, c2) => for {
      c1n <- rewriteC(c1)
      c2n <- rewriteC(c2)
    } yield CPar(c1n, c2n)
    case CSeq(c1, c2) => for {
      c1n <- rewriteC(c1)
      c2n <- rewriteC(c2)
    } yield CSeq(c1n, c2n)
    case l@CLet(_, _, e) => for {
      en <- rewriteExpr(e)
    } yield l.copy(e = en)
    case CView(id, Shrink(arrId, dims)) => State { env =>
      val f = (es: List[Expr]) => EArrAccess(arrId, es.zip(dims).map({
        case (e, (idx, _, s)) => e + (idx * EInt(s))
      }))
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
      case fdef@FuncDef(_, _, b) => fdef.copy(body = rewriteC(b)(emptyEnv)._1)
      case _ => defi
    })
    val cmdn = rewriteC(p.cmd)(emptyEnv)._1
    p.copy(defs = fs, cmd = cmdn)
  }
}
