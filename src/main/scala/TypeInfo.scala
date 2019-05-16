package fuselang

import scala.util.parsing.input.Position

import Syntax._
import Errors._

object TypeInfo {

  case class ArrayInfo(
    id: Id,
    avBanks: Map[Int, Set[Int]],
    conBanks: Map[Int, Set[Int]],
    conLocs: Map[(Int, Int), Position] = Map()) {

    def consumeBank(dim: Int, bank: Int)
                   (implicit pos: Position) = avBanks.contains(dim) match {
      case true =>
        if (avBanks(dim).contains(bank) == false) {
          throw UnknownBank(id, bank)
        } else if (conBanks(dim).contains(bank)){
          throw AlreadyConsumed(id, dim, bank, conLocs((dim, bank)))
        } else {
          this.copy(
            conBanks = conBanks + (dim -> (conBanks(dim) + bank)),
            conLocs = conLocs + ((dim, bank) -> pos))
        }
      case false => throw UnknownDim(id, dim)
    }

    def merge(that: ArrayInfo) = {
      val conBanks = this.conBanks.map({
        case (dim, bankSet) => dim -> (that.conBanks(dim) union bankSet)
      })
      this.copy(conBanks = conBanks, conLocs = this.conLocs ++ that.conLocs)
    }

    override def toString = s"{$avBanks, $conBanks}"
  }

  object ArrayInfo {
    def apply(id: Id, banks: Vector[Int]): ArrayInfo = {
      val banksWithIndex = banks.zipWithIndex
      ArrayInfo(
        id,
        banksWithIndex.map({case (banks, i) => i -> 0.until(banks).toSet}).toMap,
        banksWithIndex.map({case (_, i) => i -> Set[Int]()}).toMap)
    }
  }
}
