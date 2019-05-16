package fuselang

import Syntax._
import Errors._
import scala.util.parsing.input.Position

object Gadgets {

  /**
   * A gadget is a piece of hardware that consumes some part of a physical
   * memory. Gadgets need to define what parts of memories should be
   * consumed when they are accessed using a:
   *
   * (1) Static Value
   * (2) Index Type
   * (3) Dynamic Value
   *
   * Each such function returns a "Summary" of things that need to be consumed.
   * A summary is simply the name of the underlying memories associated to
   * a vector that consumes banks for each dimensions. Summaries are generated
   * using the list of indices.
   */
  sealed trait Gadget {
    val dims: List[(Int, Int)]

    val underlying: Id

    def getSummary(typ: List[Type])(implicit pos: Position): Vector[Vector[Int]]
  }

  /**
   * Default gadgets are generated for all `decl` and `let` bounde arrays.
   */
  case class DefaultGadget(id: Id, underlying: Id, dims: List[(Int, Int)]) extends Gadget {

    def getSimpleSummary(typ: Type, dim: Int)
                        (implicit pos: Position): Vector[Int] = typ match {
      case TIndex((s, e), _) => {
        if (dims(dim)._2 != e - s)
          throw BankUnrollInvalid(id, dims(dim)._2, e - s)
        else
          Range(s, e).toVector
      }
      case TStaticInt(v) => Vector(v % dims(dim)._2)
      case TSizedInt(_) => {
        if (dims(dim)._2 != 1) throw InvalidDynamicIndex(id, dims(dim)._2)
        else Vector(0)
      }
      case t => {
        // TODO(rachit): Add position information here.
        throw UnexpectedType(null, "array indexing", "integer type", t)
      }
    }

    def getSummary(typs: List[Type])(implicit pos: Position) =
      typs.zipWithIndex.map({ case (typ, dim) => getSimpleSummary(typ, dim) }).toVector
  }

  case class ViewGadget(id: Id, underlying: Id, dims: List[(Int, Int)]) extends Gadget {

    /** Views always fully consume the underlying memory.
     *  This can be made to be more fine grained in the future. Specifically,
     *  we can compute more exact banks for split views and simple views
     *  created using compile time constants.
     */
    def getSimpleSummary(typ: Type, dim: Int)(implicit pos: Position): Vector[Int] = typ match {
      case _: IntType => Vector(dims(dim)._2)
      case t =>
        // TODO(rachit): Add position information here.
        throw UnexpectedType(pos, "array indexing", "integer type", t)
    }

    def getSummary(typs: List[Type])(implicit pos: Position) =
      typs.zipWithIndex.map({ case (typ, dim) => getSimpleSummary(typ, dim) }).toVector
  }
}
