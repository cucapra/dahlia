package fuselang

import Syntax._
import Errors._

object TypeChecker {
  import TypeEnv._

  def checkFuse(c: Command) = checkC(c)(emptyEnv)

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

  private def checkE(expr: Expr)(implicit env: Env): (Type, Env) = expr match {
    case EFloat(_) => TFloat() -> env
    case EInt(v) => TStaticInt(v) -> env
    case EBool(_) => TBool() -> env
    case EVar(id) => env(id).typ -> env
    case EBinop(op, e1, e2) => {
      val (t1, env1) = checkE(e1)
      val (t2, env2) = checkE(e2)(env1)
      checkB(t1, t2, op) -> env2
    }
    case EAA(id, idxs) => env(id).typ match {
      case TArray(typ, dims) => {
        if (dims.length != idxs.length) {
          throw IncorrectAccessDims(id, dims.length, idxs.length)
        }
        typ -> idxs.zipWithIndex.foldLeft(env)({case (env1, (e, i)) =>
          checkE(e)(env1) match {
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

  private def checkC(cmd: Command)(implicit env: Env): Env = cmd match {
    case CDecl(id, typ) => env.addBind(id -> Info(id, typ))
    case CSeq(c1, c2) => checkC(c2)(checkC(c1))
    case CIf(cond, cons) => {
      val (cTyp, e1) = checkE(cond)(env.addScope)
      if (cTyp != TBool()) {
        throw UnexpectedType(cond.pos, "if condition", TBool().toString, cTyp)
      } else {
        checkC(cons)(e1).endScope._1
      }
    }
    case CUpdate(lhs, rhs) => {
      val (t1, e1) = checkE(lhs)
      val (t2, e2) = checkE(rhs)(e1)
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
      val (ta, e2) = checkE(r)(e1)
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
      val (e2, binds) = checkC(par)(e1).endScope
      // Create scope where ids bound in the parallel scope map to fully banked arrays.
      val vecBinds = binds.map({ case (id, inf) =>
        id -> Info(id, TArray(inf.typ, List((range.u, range.u))))
      })
      // Remove the binding of the iterator and add the updated vector bindings.
      checkC(combine)(e2.endScope._1.addScope ++ vecBinds).endScope._1
    }
    case CExpr(e) => checkE(e)._2
    case CEmpty => env
    // XXX(rachit): @adrian this refreshes capabilities and banks in all the scopes globally.
    // Should this be instead limited to the current scope?
    // Intuitively, I would say no, since a logical time step refreshes things globally.
    case CRefreshBanks() => env.refreshBanks
  }
}
