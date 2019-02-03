package fuselang

import Syntax._
import Errors._

object TypeEnv {

  type Env = Map[Id, Info]

  implicit class RichMap(m: Env) {
    def addBind(bind: (Id, Info)) = m.get(bind._1) match {
      case Some(_) => throw AlreadyBound(bind._1)
      case None => m + bind
    }
    def getBind(id: Id) = m.get(id) match {
      case Some(info) => info
      case None => throw UnboundVar(id)
    }
    def refreshBanks = m.map({ case (id, info) => id -> Info(id, info.typ) })
  }

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

