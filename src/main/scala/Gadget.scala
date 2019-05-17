package fuselang

import Syntax._

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
