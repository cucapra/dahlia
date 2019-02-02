package fuselang

import Syntax._
import Errors._

object TypeEnv {
  type Env = Map[Id, Info]

  def refreshBanks(env: Env): Env = env.map({ case (id, info) => id -> Info(id, info.typ) })

  implicit class RichCheck(checkRet: (Type, Env)) {
    def typRun(typ: Type, construct: String, f: Env => Env) = {
      if (checkRet._1 != typ) {
        throw UnexpectedType(construct, typ, checkRet._1)
      } else {
        f(checkRet._2)
      }
    }
  }

  case class Info(id: Id, typ: Type, avBanks: Map[Int, Set[Int]], conBanks: Map[Int, Set[Int]]) {
    def consumeBank(dim: Int, bank: Int): Info = avBanks.contains(dim) match {
      case true => if (avBanks(dim).contains(bank)) {
        Info(
          id,
          typ,
          avBanks + (dim -> (avBanks(dim) - bank)),
          conBanks + (dim -> (conBanks(dim) + bank)))
      } else if (conBanks(dim).contains(bank)){
        throw MsgError(s"Bank $bank in dimension $dim of $id already consumed.")
      } else {
        throw MsgError(s"Bank $bank does not exist for dimension $dim of $id.")
      }
      case false => throw UnknownDim(id, dim)
    }
    def consumeDim(dim: Int, unrollFactor: Int) = typ match {
      case TArray(_, dims) => dims.lift(dim) match {
        case Some((_, bank)) => {
          if (unrollFactor != bank) {
            throw BankUnrollInvalid(bank, unrollFactor)
          }
          val banks = 0.until(bank)
          banks.foldLeft(this)({ case (info, bank) => info.consumeBank(dim, bank) })
        }
        case None => throw UnknownDim(id, dim)
      }
      case _ => ??? // Cannot happen
    }
  }

  object Info {
    def apply(id: Id, typ: Type): Info = typ match {
      case TArray(_, dims) => {
        val banksWithIndex = dims.map({case (_, b) => b}).zipWithIndex
        Info(
          id,
          typ,
          banksWithIndex.map({case (banks, i) => i -> 0.until(banks).toSet}).toMap,
          banksWithIndex.map({case (_, i) => i -> Set[Int]()}).toMap)
      }
      case _ => Info(id, typ, Map(), Map())
    }
  }
}

object TypeChecker {
  import TypeEnv._

  def checkFuse(c: Command) = checkC(c)(Map[Id, Info]())

  private def checkB(t1: Type, t2: Type, op: Op2) = op match {
    case OpEq | OpNeq => {
      if (t1 :< t2 || t2 :< t1) TBool
      else throw UnexpectedSubtype(op.toString, t1, t2)
    }
    case OpLt | OpLte | OpGt | OpGte => (t1, t2) match {
      case ((TStaticInt(_) | TSizedInt(_)), (TStaticInt(_) | TSizedInt(_))) => TBool
      case (TFloat, TFloat) => TFloat
      case _ => throw BinopError(op, t1, t2)
    }
    case OpAdd | OpTimes | OpSub | OpDiv => (t1, t2) match {
      case ((TStaticInt(_) | TSizedInt(_)), (TStaticInt(_) | TSizedInt(_))) => t1.join(t2, op.toFun)
      case (TFloat, TFloat) => TFloat
      case _ => throw BinopError(op, t1, t2)
    }

  }

  private def checkE(expr: Expr)(implicit env: Env): (Type, Env) = expr match {
    case EFloat(_) => TFloat -> env
    case EInt(v) => TStaticInt(v) -> env
    case EBool(_) => TBool -> env
    case EVar(id) => env.get(id) match {
      case Some(inf) => inf.typ -> env
      case None => throw UnboundVar(id)
    }
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
            case (TIndex((s, e), _), env2) => env2 + (id -> env2(id).consumeDim(i, e - s))
            case (TStaticInt(v), env2) => env2 + (id -> env(id).consumeBank(i, v % dims(i)._2))
            case (t, _) => throw InvalidIndex(id, t)
          }
        })
      }
      case t => throw UnexpectedType("array access", TArray(t, List()), t)
    }
  }

  private def checkC(cmd: Command)(implicit env: Env): Env = cmd match {
    case CDecl(id, typ) => env + (id -> Info(id, typ))
    case CSeq(c1, c2) => checkC(c2)(checkC(c1))
    case CIf(cond, cons) => checkE(cond).typRun(TBool, "if condition", env => checkC(cons)(env))
    case CUpdate(lhs, rhs) => {
      val (t1, e1) = checkE(lhs)
      val (t2, e2) = checkE(rhs)(e1)
      if (t2 :< t1) (t1, t2, lhs) match {
        // Reassignment of static ints upcasts to bit<32>
        case (TStaticInt(_), TStaticInt(_), EVar(id)) => e2 + (id -> Info(id, TSizedInt(32)))
        case (TStaticInt(_), TSizedInt(_), EVar(id)) => e2 + (id -> Info(id, t2))
        case _ => e2
      }
      else throw UnexpectedSubtype("assignment", t1, t2)
    }
    case CLet(id, exp) => {
      val (t, e1) = checkE(exp)
      e1 + (id -> Info(id, t))
    }
    case CFor(iter, range, par, _) => {
      checkC(par)(env + (iter -> Info(iter, range.idxType)))
    }
    case CExpr(e) => checkE(e)._2
    case CEmpty => env
    case CRefreshBanks => refreshBanks(env)
  }
}
