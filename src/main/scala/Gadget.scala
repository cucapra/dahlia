package fuselang

import Syntax._

/**
 * A _gadget_ represents a unit of hardware that adapts memory indices. Program
 * subscript gadgets to index them, and gadgets produce accesses (both reads
 * and writes) to the underlying gadget. A physical memory has a _base gadget_
 * on top of which all gadget hierarchies are built. During type checking,
 * gadgets are responsible for summarizing their resource consumption --
 * specifically, they transform source resource demands. When the program
 * asks for resources from a gadget, the gadget determines which resources
 * it requires from the underlying gadget.
 */
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
     * when static accessors are used. For now, we ignore the [[splitDims]]
     * parameter completely.
     */
    def apply(
      underlying: Gadget,
      arrayDims: List[(Int, Int)],
      @deprecated("Not used", "0.0.1") splitDims: List[(Int, Int)]): ViewGadget = {
        ViewGadget(underlying, arrayDims)
    }
  }
}
