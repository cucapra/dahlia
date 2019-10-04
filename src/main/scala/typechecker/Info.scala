package fuselang.typechecker

import scala.util.parsing.input.Position

import fuselang.common._
import Syntax._
import Errors._
import fuselang.Utils._

object Info {

  case class ArrayInfo(
    id: Id,
    // List of starting bank resources.
    avBanks: Map[Int, Seq[Int]],
    // Bank resources that haven't been consumed yet.
    remBanks: Map[Int, Seq[Int]],
    // Source code locations that consumed a bank.
    conLocs: Map[(Int, Int), Position] = Map()) {

    private def consumeDim(dim: Int, resources: Seq[Int])
                          (implicit pos: Position) = {
      assertOrThrow(avBanks.contains(dim), UnknownDim(id, dim))
      val (av, rem) = (avBanks(dim), remBanks(dim))

      // Make sure banks exist.
      val diff = resources diff av
      assertOrThrow(diff.isEmpty, UnknownBank(id, diff.toSeq(0), dim))

      // Make sure banks are not already consumed.
      val hasRequired = resources.forall(rem.contains(_))
      scribe.debug((remBanks, resources, hasRequired).toString)
      if (hasRequired == false) {
        val bank = resources.diff(rem).toSeq(0)
        throw AlreadyConsumed(id, dim, bank, conLocs((dim, bank)))
      }

      this.copy(
        remBanks = remBanks + (dim -> rem.diff(resources)),
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
        case (dim, bankSet) => dim -> (that.remBanks(dim).intersect(bankSet))
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
            List.tabulate(banks)(b => List.tabulate(ports)(_ => b)).flatten
          }).toMap

      ArrayInfo(id, startResources, startResources)
    }
  }
}
