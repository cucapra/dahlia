package fuselang.typechecker

import scala.util.parsing.input.Position

import fuselang.common._
import Syntax._
import Errors._
import fuselang.Utils._

object Info {

  case class ArrayInfo(
    id: Id,
    avBanks: Map[Int, Set[Int]],
    conBanks: Map[Int, Set[Int]],
    conLocs: Map[(Int, Int), Position] = Map()) {

    private def consumeDim(dim: Int, resources: Set[Int])
                          (implicit pos: Position) = {
      assertOrThrow(avBanks.contains(dim), UnknownDim(id, dim))
      val (av, con) = (avBanks(dim), conBanks(dim))

      // Make sure banks exist.
      val diff = resources diff av
      assertOrThrow(diff.isEmpty, UnknownBank(id, diff.toSeq(0), dim))

      // Make sure banks are not already consumed.
      val alreadyCon = con intersect resources
      if (alreadyCon.isEmpty == false) {
        val bank = alreadyCon.toSeq(0)
        throw AlreadyConsumed(id, dim, bank, conLocs((dim, bank)))
      }

      this.copy(
        conBanks = conBanks + (dim -> con.union(resources)),
        conLocs = conLocs ++ resources.map((dim, _) -> pos))
    }

    def consumeResources(resources: Iterable[Iterable[Int]])
                        (implicit pos: List[Position]) = {
      resources.zipWithIndex.zip(pos).foldLeft(this) {
        case (info, ((resource, dim), pos)) => info.consumeDim(dim, resource.toSet)(pos)
      }
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
    def apply(id: Id, banks: Iterable[Int]): ArrayInfo = {
      val banksWithIndex = banks.zipWithIndex
      ArrayInfo(
        id,
        banksWithIndex.map({case (banks, i) => i -> 0.until(banks).toSet}).toMap,
        banksWithIndex.map({case (_, i) => i -> Set[Int]()}).toMap)
    }
  }
}
