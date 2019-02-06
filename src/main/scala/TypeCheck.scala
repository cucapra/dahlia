package fuselang

import Syntax._
import Errors._

object TypeChecker {
  import TypeEnv._

  def typeCheck(p: Prog) = {
    val initEnv = p.decls.foldLeft(emptyEnv)({ case (env, Decl(id, typ)) =>
      id.typ = Some(typ);
      env.addBind(id -> Info(id, typ))
    })
    checkC(p.cmd)(initEnv, List[Id]())
  }

  private def checkB(t1: Type, t2: Type, op: Op2) = op match {
    case OpEq() | OpNeq() => {
      if (t1 :< t2 || t2 :< t1) TBool()
      else throw UnexpectedSubtype(op.pos, op.toString, t1, t2)
    }
    case OpLt() | OpLte() | OpGt() | OpGte() => (t1, t2) match {
      case ((TStaticInt(_) | TSizedInt(_)), (TStaticInt(_) | TSizedInt(_))) => TBool()
      case (_: TFloat, _: TFloat) => TFloat()
      case _ => throw BinopError(op, t1, t2)
    }
    case OpAdd() | OpTimes() | OpSub() | OpDiv() => (t1, t2) match {
      case ((TStaticInt(_) | TSizedInt(_)), (TStaticInt(_) | TSizedInt(_))) => t1.join(t2, op.toFun)
      case (_: TFloat, _: TFloat) => TFloat()
      case _ => throw BinopError(op, t1, t2)
    }

  }

  private def checkE(expr: Expr)(implicit env: Env, its: ItStack): (Type, Env) = expr match {
    case EFloat(_) => TFloat() -> env
    case EInt(v) => TStaticInt(v) -> env
    case EBool(_) => TBool() -> env
    case EVar(id) => {
      // Add type information to variable
      id.typ = Some(env(id).typ);
      env(id).typ -> env
    }
    case EBinop(op, e1, e2) => {
      val (t1, env1) = checkE(e1)
      val (t2, env2) = checkE(e2)(env1, its)
      checkB(t1, t2, op) -> env2
    }
    case EAA(id, idxs) => env(id).typ match {
      case TArray(typ, dims) => {
        if (dims.length != idxs.length) {
          throw IncorrectAccessDims(id, dims.length, idxs.length)
        }
        // update type for id
        id.typ = Some(env(id).typ);
        typ -> idxs.zipWithIndex.foldLeft(env)({case (env1, (e, i)) =>
          checkE(e)(env1, its) match {
            case (TIndex((s, e), _), env2) =>
              env2.updateBind(id -> env2(id).consumeDim(i, e - s))
            case (TStaticInt(v), env2) =>
              env2.updateBind(id -> env(id).consumeBank(i, v % dims(i)._2))
            case (t, _) => throw InvalidIndex(id, t)
          }
        })
      }
      case t => throw UnexpectedType(expr.pos, "array access", s"$t[]", t)
    }
  }

  private def checkC(cmd: Command)(implicit env: Env, its: ItStack): Env = cmd match {
    case CPar(c1, c2) => checkC(c2)(checkC(c1), its)
    case CIf(cond, cons) => {
      val (cTyp, e1) = checkE(cond)(env.addScope, its)
      if (cTyp != TBool()) {
        throw UnexpectedType(cond.pos, "if condition", TBool().toString, cTyp)
      } else {
        checkC(cons)(e1, its).endScope._1
      }
    }
    case CUpdate(lhs, rhs) => {
      val (t1, e1) = checkE(lhs)
      val (t2, e2) = checkE(rhs)(e1, its)
      // :< is a subtyping method we defined on the type trait.
      if (t2 :< t1) (t1, t2, lhs) match {
        // Reassignment of static ints upcasts to bit<32>
        case (TStaticInt(_), TStaticInt(_), EVar(id)) =>
          e2.updateBind(id -> Info(id, TSizedInt(32)))
        case (TStaticInt(_), TSizedInt(_), EVar(id)) =>
          e2.updateBind(id -> Info(id, t2))
        case _ => e2
      }
      else throw UnexpectedSubtype(rhs.pos, "assignment", t1, t2)
    }
    case CReduce(rop, l, r) => {
      val (t1, e1) = checkE(l)
      val (ta, e2) = checkE(r)(e1, its)
      (t1, ta) match {
        case (t1, TArray(t2, dims)) =>
          if (t2 :< t1 == false || dims.length != 1 || dims(0)._1 != dims(0)._2) {
            throw ReductionInvalidRHS(r.pos, rop, t1, ta)
          } else {
            e2
          }
        case _ =>
          throw ReductionInvalidRHS(r.pos, rop, t1, ta)
      }
    }
    case l@CLet(id, typ, exp) => {
      val (t, e1) = checkE(exp)
      typ match {
        case Some(t2) if t :< t2 => e1.addBind(id -> Info(id, t2))
        case Some(t2) => throw UnexpectedType(exp.pos, "let", t.toString, t2)
        case None => l.typ = Some(t); e1.addBind(id -> Info(id, t))
      }
    }
    case CFor(range, par, combine) => {
      val iter = range.iter
      // Add binding for iterator in a separate scope.
      val e1 = env.addScope.addBind(iter -> Info(iter, range.idxType)).addScope
      // Check for body and pop the scope.
      val (e2, binds) = checkC(par)(e1, iter :: its).endScope
      // Create scope where ids bound in the parallel scope map to fully banked arrays.
      val vecBinds = binds.map({ case (id, inf) =>
        id -> Info(id, TArray(inf.typ, List((range.u, range.u))))
      })
      // Remove the binding of the iterator and add the updated vector bindings.
      checkC(combine)(e2.endScope._1.addScope ++ vecBinds, its).endScope._1
    }
    case CExpr(e) => checkE(e)._2
    case CEmpty => env
    case CSeq(c1, c2) => {
      val _ = checkC(c1)
      val e2 = checkC(c2)(env, its)
      // FIXME(rachit): This should probably be intersection of e1 and e2
      e2
    }
  }
}
