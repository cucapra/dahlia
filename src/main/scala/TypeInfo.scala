package fuselang

import scala.util.parsing.input.Position

import Syntax._
import Errors._
import CompilerError._
import Utils._

object TypeInfo {

  sealed trait Info {
    val id: Id
    val typ: Type

    /** Consume the given bank from the Info object */
    def consumeBank(dim: Int, bank: Int)(implicit pos: Position): Info

    /**
     * Return a new Info such that for each dimension:
     * - conBanks is the union of this.conBanks and that.conBanks
     * - avBanks is the intersection of this.conBanks and that.conBanks
     */
    def merge(that: Info): Info = {
      assertOrThrow(this.id == that.id,
        Impossible(s"Tried to merge ${that.id} and ${this.id}"))
      assertOrThrow(this.typ == that.typ,
        Impossible(s"Tried to merge types ${that.typ} and ${this.typ} for ${this.id}"))

      (this, that) match {
        case (me:ArrayInfo, that:ArrayInfo) => {
          val conBanks = me.conBanks.map({
            case (dim, bankSet) => dim -> (that.conBanks(dim) union bankSet)
          })
          me.copy(conBanks = conBanks, conLocs = me.conLocs ++ that.conLocs)
        }
        case (me:SimpleInfo, _:SimpleInfo) => me
        case _ => throw Impossible(s"Tried to merge $this and $that.")
      }
    }
  }

  private case class SimpleInfo(id: Id, typ: Type) extends Info {
    def consumeBank(d: Int, b: Int)(implicit p: Position) =
      throw Impossible(s"Tried to consume bank for type $typ")
  }

  private case class ArrayInfo(
    id: Id,
    typ: Type,
    avBanks: Map[Int, Set[Int]],
    conBanks: Map[Int, Set[Int]],
    conLocs: Map[(Int, Int), Position] = Map()) extends Info {

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

    override def toString = s"{$typ, $avBanks, $conBanks}"
  }

  // Companion object to allow for easier construction of Info.
  object Info {
    def apply(id: Id, typ: Type): Info = typ match {
      case TArray(_, dims) => {
        val banksWithIndex = dims.map({case (_, b) => b}).zipWithIndex
        ArrayInfo(
          id,
          typ,
          banksWithIndex.map({case (banks, i) => i -> 0.until(banks).toSet}).toMap,
          banksWithIndex.map({case (_, i) => i -> Set[Int]()}).toMap)
      }
      case _ => SimpleInfo(id, typ)
    }
  }
}
