package fuselang.typechecker

import scala.util.parsing.input.Position

import fuselang.Utils._

import fuselang.common._
import Syntax._
import Errors._
import MultiSet._

object Info {

  case class ArrayInfo(
    id: Id,
    // List of starting bank resources.
    avBanks: Map[Int, MultiSet[Int]],
    // Bank resources that haven't been consumed yet.
    remBanks: Map[Int, MultiSet[Int]],
    // Source code locations that consumed a bank.
    conLocs: Map[(Int, Int), Position] = Map()) {

    private def consumeDim(dim: Int, resources: Seq[Int])
                          (implicit pos: Position) = {

      assertOrThrow(avBanks.contains(dim), UnknownDim(id, dim))
      val (av, rem) = (avBanks(dim), remBanks(dim))

      // Make sure banks exist.
      val missingBank = resources.find(!av.containsAtLeast(_, 1))
      scribe.debug((resources, av, missingBank).toString)
      assertOrThrow(missingBank.isEmpty, UnknownBank(id, missingBank.get, dim))

      // Check if required resources are available
      val resourceMS = fromSeq(resources)
      val afterConsume = rem.diff(resourceMS)
      val hasRequired = afterConsume.forall({case (_, v) => v >= 0 })
      //scribe.debug((remBanks, resources, hasRequired).toString)
      if (hasRequired == false) {
        val bank = afterConsume.find({ case (_, v) => v < 0 }).get._1
        throw AlreadyConsumed(id, dim, bank, conLocs.get((dim, bank)))
      }

      this.copy(
        remBanks = remBanks + (dim -> afterConsume),
        conLocs = conLocs ++ resources.map((dim, _) -> pos))
    }

    def consumeResources(resources: Seq[Seq[Int]])
                        (implicit pos: List[Position]) = {
      resources.zipWithIndex.zip(pos).foldLeft(this) {
        case (info, ((resource, dim), pos)) => info.consumeDim(dim, resource)(pos)
      }
    }

    def merge(that: ArrayInfo) = {
      val remBanks = this.remBanks.map({
        case (dim, bankMSet) => dim -> (that.remBanks(dim).zipWith(bankMSet, Math.min))
      })
      this.copy(remBanks = remBanks, conLocs = this.conLocs ++ that.conLocs)
    }

    override def toString = s"{$avBanks, $remBanks}"
  }

  object ArrayInfo {
    def apply(id: Id, banks: Iterable[Int], ports: Int): ArrayInfo = {
      // Generate resources for 0..i banks with `ports` copy of each resource.
      val startResources =
        banks
          .zipWithIndex
          .map({ case (banks, i) =>
            i ->
            fromSeq(List.tabulate(banks)(b => List.tabulate(ports)(_ => b)).flatten)
          }).toMap

      ArrayInfo(id, startResources, startResources)
    }
  }
}
