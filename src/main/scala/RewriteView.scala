package fuselang

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
  import Syntax._
  import CodeGenHelpers._

  // We can use a flat environment because the type checker makes sure things
  // in the right scope.
  type T = Map[Id, List[Expr] => Expr]

  def foldExprs(es: List[Expr])(implicit env: T): (List[Expr], T) =
      es.foldLeft((List[Expr](), env))({
        case ((idxsn, env), e) => {
          val (e1, env1) = rewriteExpr(e)(env)
          (e1 :: idxsn, env1)
        }
      })

  def rewriteExpr(e: Expr)(implicit env: T): (Expr, T) = e match {
    case EVar(_) | EInt(_) | EFloat(_) | EBool(_) => e -> env
    case eb@EBinop(_, e1, e2) => {
      val (e1n, env1) = rewriteExpr(e1)
      val (e2n, env2) = rewriteExpr(e2)(env1)
      eb.copy(e1 = e1n, e2 = e2n) -> env2
    }
    case eaa@EAA(arrId, idxs) => {
      val (idxsn, env1) = foldExprs(idxs)
      // If the array id is a view array rewrite it and recur
      if (env.contains(arrId)) {
        rewriteExpr(env(arrId)(idxsn))
      } else {
        eaa.copy(idxs = idxsn) -> env1
      }
    }
    case app@EApp(_, args) => {
      val (argsn, env1) = foldExprs(args)
      app.copy(args = argsn) -> env1
    }
  }

  def rewriteCommand(c: Command)(implicit env: T): (Command, T) = c match {
    case CPar(c1, c2) => {
      val (c1n, env1) = rewriteCommand(c1)
      val (c2n, env2) = rewriteCommand(c2)(env1)
      CPar(c1n, c2n) -> env2
    }
    case CSeq(c1, c2) => {
      val (c1n, env1) = rewriteCommand(c1)
      val (c2n, env2) = rewriteCommand(c2)(env1)
      CSeq(c1n, c2n) -> env2
    }
    case l@CLet(_, _, e) => {
      val (en, env1) = rewriteExpr(e)
      l.copy(e = en) -> env1
    }
    case CView(id, Shrink(arrId, dims)) => {
      val f = (es: List[Expr]) => EAA(arrId, es.zip(dims).map({
        case (e, (idx, _, s)) => e + (idx * EInt(s))
      }))
      (CEmpty, env + (id -> f))
    }
    case CIf(e1, c2) => {
      val (e1n, env1) = rewriteExpr(e1)
      val (c2n, env2) = rewriteCommand(c2)(env1)
      CIf(e1n, c2n) -> env2
    }
    case cf@CFor(_, c1, c2) => {
      val (c1n, e1) = rewriteCommand(c1)
      val (c2n, e2) = rewriteCommand(c2)(e1)
      cf.copy(par = c1n, combine = c2n) -> e2
    }
    case cw@CWhile(_, c) => {
      val (cn, e1) = rewriteCommand(c)
      cw.copy(body = cn) -> e1
    }
    case CUpdate(e1, e2) => {
      val (e1n, env1) = rewriteExpr(e1)
      val (e2n, env2) = rewriteExpr(e2)(env1)
      CUpdate(e1n, e2n) -> env2
    }
    case cr@CReduce(_, e1, e2) => {
      val (e1n, env1) = rewriteExpr(e1)
      val (e2n, env2) = rewriteExpr(e2)(env1)
      cr.copy(lhs = e1n, rhs = e2n) -> env2
    }
    case CExpr(exp) => {
      val (e1n, env1) = rewriteExpr(exp)
      CExpr(e1n) -> env1
    }
    case CEmpty => c -> env
  }

  def rewriteProg(p: Prog): Prog = {
    val emptyEnv = Map[Id, List[Expr] => Expr]()
    val fs = p.fdefs.map(fdef => fdef.copy(body = rewriteCommand(fdef.body)(emptyEnv)._1))
    val cmdn = rewriteCommand(p.cmd)(emptyEnv)._1
    p.copy(fdefs = fs, cmd = cmdn)
  }
}
