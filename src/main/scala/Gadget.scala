package fuselang

import Syntax._
import Errors._
import scala.util.parsing.input.Position

object Gadgets {

  type ConsumeList = Iterable[Iterable[Int]]

  trait Gadget {
    def getSummary(consume: ConsumeList): (Id, ConsumeList)
  }

  case class BaseGadget(resource: Id) extends Gadget {
    def getSummary(consume: ConsumeList) = resource -> consume
  }

  case class ViewGadget(
    underlying: Gadget,
    transformer: ConsumeList => ConsumeList) extends Gadget {
      def getSummary(consume: ConsumeList) = underlying.getSummary(transformer(consume))
    }

  object ViewGadget {

    /**
     * Creates a conservative simple view that fully consumes the array
     * regardless of how fine grain the accessors were. It is possible
     * to write a more fine grained transformer when the view is static
     * i.e. an aligned view is being used.
     */
    def apply(underlying: Gadget, dims: List[(Int, Int)]): ViewGadget = {
      val transformer = (_: ConsumeList) => dims.map(_._2).map(0 until _)
      ViewGadget(underlying, transformer)
    }

    /**
     * Creates logic for a split view. A split view always has an even number
     * of dimensions which are grouped. For now, the implementation simply
     * consumes the entire underlying array. It is possible to refine this
     * when static accessors are used.
     */
    def apply(
      underlying: Gadget,
      arrayDims: List[(Int, Int)],
      @deprecated("Not used", "0.0.1") splitDims: List[(Int, Int)]): ViewGadget = {
        ViewGadget(underlying, arrayDims)
    }
  }
}

object Gadget2 {
  /**
   * A gadget is a piece of hardware that consumes some part of a physical
   * memory. Gadgets need to define what parts of memories should be
   * consumed when they are accessed using a:
   *
   * (1) Static Value
   * (2) Index Type
   * (3) Dynamic Value
   * (4) Used as an argument to a function.
   *
   * Each such function returns a "Summary" of things that need to be consumed.
   * A summary is simply the name of the underlying memories associated to
   * a vector that consumes banks for each dimensions. Summaries are generated
   * using the list of indices.
   */
  sealed trait Gadget {
    def getSummary(typ: List[Type])
                  (implicit pos: Position): (Id, Iterable[Iterable[Int]])

    def getSummaryAsArg: (Id, Iterable[Iterable[Int]])
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
        throw UnexpectedType(pos, "array indexing", "integer type", t)
      }
    }

    def getSummary(typs: List[Type])(implicit pos: Position) =
      (underlying,
        typs
          .zipWithIndex
          .map({ case (typ, dim) => getSimpleSummary(typ, dim) }))

    /**
     * Fully consumes the underlying array when used as an arg.
     */
    def getSummaryAsArg = underlying -> dims.map(dim => 0.until(dim._2))
  }

  case class ViewGadget(id: Id, underlying: Id, dims: List[(Int, Int)]) extends Gadget {

    /** Views always fully consume the underlying memory.
     *  This can be made to be more fine grained in the future. Specifically,
     *  we can compute more exact banks for split views and simple views
     *  created using compile time constants.
     */
    def getSimpleSummary(typ: Type, dim: Int)
                        (implicit pos: Position): Iterable[Int] = typ match {
      case _: IntType => 0.until(dims(dim)._2)
      case t => throw UnexpectedType(pos, "array indexing", "integer type", t)
    }

    def getSummary(typs: List[Type])(implicit pos: Position) =
      underlying ->
      typs.zipWithIndex.map({ case (typ, dim) => getSimpleSummary(typ, dim) }).toVector

    /**
     * Fully consumes the underlying array when used as an arg.
     */
    def getSummaryAsArg = underlying -> dims.map(dim => 0.until(dim._2))
  }
}
