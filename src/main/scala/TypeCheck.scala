package fuselang

import Syntax._

object TypeErrors {

  sealed trait TypeError extends RuntimeException
  case class MsgError(msg: String) extends TypeError
  case class UnexpectedType(construct: String, exp: Type, actual: Type) extends TypeError {
    override def toString = s"Expected type $exp in $construct, received: $actual."
  }
  case class UnexpectedSubtype(construct: String, exp: Type, actual: Type) extends TypeError {
    override def toString = s"Expected subtype of $exp in $construct, received: $actual."
  }
}

object TypeChecker {
  import TypeErrors._

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

  class Info(id: Id, typ: Type, avBanks: Map[Int, Set[Int]], conBanks: Map[Int, Set[Int]]) {
    def consumeBank(dim: Int, bank: Int): Either[TypeError, Info] = avBanks.contains(dim) match {
      case true => if (avBanks(dim).contains(bank)) {
        Right(
          new Info(
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
    def apply(id: Id, typ: Type) = typ match {
      case TArray(_, dims) => {
        val banks = dims.map({case (_, b) => b})
        new Info(
          id,
          typ,
          banks.zipWithIndex.map({case (i, banks) => i -> 0.until(banks).toSet}).toMap,
          banks.zipWithIndex.map({case (i, _) => i -> Set[Int]()}).toMap)
      }
      case _ => new Info(id, typ, Map(), Map())
    }
  }

  def checkE(expr: Expr)(implicit env: Env): (Type, Env) = expr match {
    case EInt(_) => TSizedInt(64) -> env
    case _ => TBool -> env
  }

  def checkC(cmd: Command)(implicit env: Env): Env = cmd match {
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
    case CExpr(e) => checkE(e)._2
    case CEmpty => env
  }

}
