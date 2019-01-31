package fuselang

import scala.math.{max,log10,ceil}

import Syntax._
import TypeErrors._

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
      case false => throw MsgError(s"$id does not have dimension $dim. $this")
    }
    def consumeBanks(dim: Int, bank: Seq[Int]): Info = {
      bank.foldLeft(this)({ case (info, bank) => info.consumeBank(dim, bank) })
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

  private def checkB(t1: Type, t2: Type, b: Op2) = b match {
    case OpEq => {
      if (t1 :< t2 || t2 :< t1) TBool
      else throw UnexpectedSubtype("=", t1, t2)
    }
    case OpAdd => (t1, t2) match {
      case (TSizedInt(s1), TSizedInt(s2)) => TSizedInt(max(s1, s2))
      case (TStaticInt(v1), TStaticInt(v2)) => TStaticInt(v1 + v2)
      case (TStaticInt(v), TSizedInt(s)) => {
        TSizedInt(max(s, ceil(log10(v)/log10(2)).toInt))
      }
      case (TSizedInt(s), TStaticInt(v)) => {
        TSizedInt(max(s, ceil(log10(v)/log10(2)).toInt))
      }
      case (TSizedInt(_), _) => throw UnexpectedSubtype("+", t1, t2)
      case (_, TSizedInt(_)) => throw UnexpectedSubtype("+", t2, t1)
      case (TStaticInt(_), _) => throw UnexpectedSubtype("+", t1, t2)
      case (_, TStaticInt(_)) => throw UnexpectedSubtype("+", t2, t1)
      case (_, _) => throw UnexpectedSubtype("+", TSizedInt(64), t1)
    }
  }

  private def checkE(expr: Expr)(implicit env: Env): (Type, Env) = expr match {
    case EInt(v) => TStaticInt(v) -> env
    case EBool(_) => TBool -> env
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
            case (TIndex((s, e), _), env2) => env2 + (id -> env2(id).consumeBanks(i, s.until(e)))
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
      if (t2 :< t1) lhs match {
        case EVar(id) => e2 + (id -> Info(id, t2))
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
