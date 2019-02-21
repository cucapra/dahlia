package fuselang

object TypeInfo {
  import Syntax._
  import Errors._

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
      case t => throw Impossible(s"consumeDim called on non-array type $t")
    }
    def consumeAll = typ match {
      case TArray(_, dims) => dims.zipWithIndex.foldLeft(this)({
        case (info, ((_, bank), dim)) => info.consumeDim(dim, bank)
      })
      case _ => throw Impossible("consumeAll called on non-array")
    }

    /**
     * Return a new Info such that for each dimension:
     * - conBanks is the union of this.conBanks and that.conBanks
     * - avBanks is the intersection of this.conBanks and that.conBanks
     */
    def merge(that: Info): Info = {
      val conBanks = this.conBanks.map({
        case (dim, bankSet) => dim -> (that.conBanks(dim) union bankSet)
      })
      val avBanks = this.avBanks.map({
        case (dim, bankSet) => dim -> (that.avBanks(dim) intersect bankSet)
      })
      Info(id, typ, avBanks, conBanks)
    }

    override def toString = s"{$typ, $avBanks, $conBanks}"
  }

  // Companion object to allow for easier construction of Info.
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
