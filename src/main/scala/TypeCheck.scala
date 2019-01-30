package fuselang

import scala.math.max

import Syntax._
import TypeErrors._

object TypeEnv {
  type Env = Map[Id, Info]

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
    def consumeBank(dim: Int, bank: Int): Either[TypeError, Info] = avBanks.contains(dim) match {
      case true => if (avBanks(dim).contains(bank)) {
        Right(
          Info(
            id,
            typ,
            avBanks + (dim -> (avBanks(dim) - bank)),
            conBanks + (dim -> (conBanks(dim) + bank))))
      } else if (conBanks(dim).contains(bank)){
        Left(MsgError(s"Bank $bank in dimension $dim of $id already consumed."))
      } else {
        Left(MsgError(s"Bank $bank does not exist for dimension $dim of $id."))
      }
      case false => Left(MsgError(s"$id does not have dimension $dim."))
    }
  }

  object Info {
    def apply(id: Id, typ: Type): Info = typ match {
      case TArray(_, dims) => {
        val banks = dims.map({case (_, b) => b})
        Info(
          id,
          typ,
          banks.zipWithIndex.map({case (i, banks) => i -> 0.until(banks).toSet}).toMap,
          banks.zipWithIndex.map({case (i, _) => i -> Set[Int]()}).toMap)
      }
      case _ => Info(id, typ, Map(), Map())
    }
  }
}

object TypeChecker {
  import TypeEnv._

  def checkFuse(f: Fuse) = f.prog.foldLeft(Map[Id, Info]())({
    case (env, cmd) => checkC(cmd)(env)
  })

  private def checkB(t1: Type, t2: Type, b: Op2) = b match {
    case OpEq => {
      if (t1 :< t2 || t2 :< t1) TBool
      else throw UnexpectedSubtype("=", t1, t2)
    }
    case OpAdd => (t1, t2) match {
      case (TSizedInt(s1), TSizedInt(s2)) => TSizedInt(max(s1, s2))
      case (TSizedInt(_), _) => throw UnexpectedSubtype("+", t1, t2)
      case (_, TSizedInt(_)) => throw UnexpectedSubtype("+", t2, t1)
      case (_, _) => throw UnexpectedSubtype("+", TSizedInt(64), t1)
    }
  }

  private def checkE(expr: Expr)(implicit env: Env): (Type, Env) = expr match {
    case EInt(_) => TSizedInt(64) -> env
    case EBool(_) => TBool -> env
    case EVar(id) => env(id).typ -> env
    case EBinop(op, e1, e2) => {
      val (t1, env1) = checkE(e1)
      val (t2, env2) = checkE(e2)(env1)
      checkB(t1, t2, op) -> env2
    }
    case _ => TBool -> env
  }

  private def checkC(cmd: Command)(implicit env: Env): Env = cmd match {
    case CSeq(c1, c2) => checkC(c2)(checkC(c1))
    case CIf(cond, cons) => checkE(cond).typRun(TBool, "if condition", env => checkC(cons)(env))
    case CUpdate(lhs, rhs) => {
      val (t1, e1) = checkE(lhs)
      val (t2, e2) = checkE(rhs)(e1)
      if (t2 :< t1) e2
      else throw UnexpectedSubtype("assignment", t1, t2)
    }
    case CLet(id, exp) => {
      val (t, e1) = checkE(exp)
      e1 + (id -> Info(id, t))
    }
    case CFor(iter, range, par) => {
      checkC(par)(env + (iter -> Info(iter, range.idxType)))
    }
    case CExpr(e) => checkE(e)._2
    case CEmpty => env
  }
}
