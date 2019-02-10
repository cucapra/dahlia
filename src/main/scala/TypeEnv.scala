package fuselang

import Syntax._
import Errors._

object TypeEnv {

  type Stack[T] = List[T]
  type Scope = Map[Id, Info]
  type CapScope = Map[Expr, Capability]

  // Product of all unroll factors enclosing the current context.
  type ReqResources = Int

  // capabilities for read/write
  sealed trait Capability
  case object Read extends Capability
  case object Write extends Capability

  val emptyEnv: Env = Env(List(Map()), List(Map()))

  case class Env(e: Stack[Scope], caps: Stack[CapScope]) {
    def addScope = Env(Map[Id, Info]() :: e, Map[Expr, Capability]() :: caps)
    def endScope = (Env(e.tail, caps.tail), e.head, caps.head)
    def apply(id: Id): Info = find(e, id) match {
      case Some(info) => info
      case None => throw UnboundVar(id)
    }

    def getCap(expr: Expr): Option[Capability] =
      caps.find(c => c.get(expr).isDefined).map(c => c(expr))

    def addCap(expr: Expr, cap: Capability) =
      Env(e, caps.head + (expr -> cap) :: caps.tail)

    private def find(e: Stack[Scope], id: Id): Option[Info] =
      e.find(m => m.get(id).isDefined) match {
        case None => None
        case Some(map) => Some(map(id))
      }

    def add(bind: (Id, Info)) = find(e, bind._1) match {
      case Some(_) => throw AlreadyBound(bind._1)
      case None => Env(e.head + bind :: e.tail, caps)
    }
    def update(bind: (Id, Info)) = find(e, bind._1) match {
      case None => throw UnboundVar(bind._1)
      case Some(_) => {
        val scope = e.indexWhere(m => m.get(bind._1).isDefined)
        Env(e.updated(scope, e(scope) + bind), caps)
      }
    }
    def ++(binds: Scope): Env =
      binds.foldLeft(this)({ case (e, b) => e.add(b) })

  }

  case class Info(
    id: Id,
    typ: Type,
    avBanks: Map[Int, Set[Int]],
    conBanks: Map[Int, Set[Int]]) {
    def consumeBank(dim: Int, bank: Int): Info = avBanks.contains(dim) match {
      case true => if (avBanks(dim).contains(bank)) {
        Info(
          id,
          typ,
          avBanks + (dim -> (avBanks(dim) - bank)),
          conBanks + (dim -> (conBanks(dim) + bank)))
      } else if (conBanks(dim).contains(bank)){
        throw AlreadyConsumed(id, dim, bank)
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
    override def toString = s"{$typ, $avBanks, $conBanks}"
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
