package fuselang

import scala.util.parsing.input.Position

import Syntax._
import Errors._
import CompilerError._

object TypeInfo {

  sealed trait Info {
    val id: Id
    val typ: Type

    def consumeBank(dim: Int, bank: Int)(implicit pos: Position): Info

    /**
     * Return a new Info such that for each dimension:
     * - conBanks is the union of this.conBanks and that.conBanks
     * - avBanks is the intersection of this.conBanks and that.conBanks
     */
    def merge(that: Info): Info = {
      if (this.id != that.id)
        Impossible("merge", s"Tried to merge ${that.id} and ${this.id}")
      if (this.typ != that.typ)
        Impossible("merge", s"Tried to merge types ${that.typ} and ${this.typ} for ${this.id}")

      (this, that) match {
        case (me:ArrayInfo, that:ArrayInfo) => {
          val conBanks = me.conBanks.map({
            case (dim, bankSet) => dim -> (that.conBanks(dim) union bankSet)
          })
          me.copy(conBanks = conBanks)
        }
        case (me:SimpleInfo, _:SimpleInfo) => me
        case _ =>
          throw Impossible("merge", s"Tried to merge $this and $that.")
      }
    }
  }

  private case class SimpleInfo(id: Id, typ: Type) extends Info {
    def consumeBank(d: Int, b: Int)(implicit p: Position) =
      throw Impossible("consumeBank", s"Tried to consume bank for type $typ")
  }

  private case class ArrayInfo(
    id: Id,
    typ: Type,
    avBanks: Map[Int, Set[Int]],
    conBanks: Map[Int, Set[Int]]) extends Info {

    def consumeBank(dim: Int, bank: Int)
                   (implicit pos: Position) = avBanks.contains(dim) match {
      case true =>
        if (avBanks(dim).contains(bank) == false) {
          throw UnknownBank(id, bank)
        } else if (conBanks(dim).contains(bank)){
          throw AlreadyConsumed(id, dim, bank)
        } else {
          this.copy(conBanks = conBanks + (dim -> (conBanks(dim) + bank)))
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
