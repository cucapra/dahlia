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
    avBanks: MultiSet[Int],
    // Bank resources that haven't been consumed yet.
    remBanks: MultiSet[Int],
    // Source code locations that consumed a bank.
    conLocs: Map[Int, MultiSet[Position]] = Map()
  )(implicit ctxName: String){

    override def toString = remBanks.toString

    def consumeResources(resources: Seq[Int])
                        (implicit pos: Position, trace: List[String]) = {

      // Make sure banks exist.
      val missingBank = resources.find(!avBanks.containsAtLeast(_, 1))
      assertOrThrow(missingBank.isEmpty, UnknownBank(id, missingBank.head))

      val newConLocs =
        conLocs ++ resources.foldLeft(conLocs){
          case (newConLocs, res) =>
            newConLocs + (res -> newConLocs.getOrElse(res, emptyMultiSet[Position]).add(pos))
        }


      // Calculate multi-set difference b/w required resource and available
      // resources.
      val resourceMS = fromSeq(resources)
      val afterConsume = remBanks.diff(resourceMS)
      val hasRequired = afterConsume.forall({case (_, v) => v >= 0 })
      if (hasRequired == false) {
        val bank = afterConsume.find({ case (_, v) => v < 0 }).get._1
        throw AlreadyConsumed(id, bank, avBanks.getCount(bank), newConLocs(bank))
      }

      this.copy(remBanks = afterConsume, conLocs = newConLocs)
    }

    // Return a copy of the physical resource with all the resources available.
    def toFresh = this.copy(remBanks = avBanks, conLocs = Map())

    def merge(that: ArrayInfo) = {
      val remBanks = this.remBanks.zipWith(that.remBanks, Math.min)
      this.copy(remBanks = remBanks, conLocs = this.conLocs ++ that.conLocs)
    }
  }

  object ArrayInfo {
    private def cross[A](acc: List[List[A]], l: List[A]): List[List[A]] =
      for { a <- acc; el <- l } yield a :+ el

    private def hyperBankToBank(maxBanks: Iterable[Int])(hyperBank: Seq[Int]) =
      hyperBank.zip(maxBanks).foldLeft(0)({
        case (acc, (hb, maxBank)) => acc * maxBank + hb
      })

    def apply(id: Id, banks: Iterable[Int], ports: Int)(implicit ctxName: String): ArrayInfo = {
      val startResources: MultiSet[Int] = fromSeq(
        banks
          .map(b => List.tabulate(b)(identity))
          .foldLeft(List(List[Int]()))({
            case (acc, b) => cross(acc, b)
          })
          .map(hyperBankToBank(banks))
          .map(b => List.tabulate(ports)(_ => b))
          .flatten)

      ArrayInfo(id, startResources, startResources)
    }
  }
}
