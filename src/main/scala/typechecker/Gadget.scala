package fuselang.typechecker

import fuselang.common._
import Syntax._
import CompilerError._

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
object Gadgets:

  type ConsumeList = Seq[Seq[Int]]

  def clString(cl: Seq[Seq[Int]]): String =
    cl.map(els => els.mkString("{", ",", "}")).mkString("[", "][", "]")

  trait Gadget:
    // Return the name of the resource, the list of banks to be consumed,
    // and a trace of transformations done on the original resource.
    def getSummary(consume: ConsumeList): (Id, Seq[Int], Seq[String])

  case class ResourceGadget(resource: Id, banks: Seq[Int]) extends Gadget:
    private def cross[A](acc: Seq[Seq[A]], l: Seq[A]): Seq[Seq[A]] =
      for  a <- acc; el <- l  yield a :+ el

    override def toString = resource.toString

    private def hyperBankToBank(hyperBanks: Seq[Int]) =
      if hyperBanks.length != banks.length then
        throw Impossible("hyperbank size is different from original banking")

      hyperBanks
        .zip(banks)
        .foldLeft(0)({
          case (acc, (hb, b)) => b * acc + hb
        })

    /**
      * The root for all gadgets. Maps a multidimensional consume list to
      * corresponding one dimensional banks.
      */
    def getSummary(consume: ConsumeList) =
      // Transform consumelist into a Seq[Seq[A]] where the inner list
      // represents a sequence of banks for the dimension. These are
      // latter transformed to 1D banks.
      val hyperBanks: Seq[Seq[Int]] =
        consume.tail.foldLeft(consume.head.map(Seq(_)))({
          case (acc, banks) => cross(acc, banks)
        })

      val outRes = hyperBanks.map(hyperBankToBank)

      (resource, outRes, Seq(clString(Seq(outRes))))

  case class MultiDimGadget(underlying: Gadget, dim: Seq[DimSpec])
      extends Gadget:

    /**
      * A base physical memory with `k` banks redirects access from bank `b` to
      * to `b % k`.
      */
    def getSummary(consume: ConsumeList) =
      val resourceTransform = consume
        .zip(dim)
        .map({ case (resources, (_, banks)) => resources.map(_ % banks) })

      val (res, sum, trace) = underlying.getSummary(resourceTransform)
      (res, sum, clString(resourceTransform) +: trace)

  case class ViewGadget(
      underlying: Gadget,
      transformer: ConsumeList => ConsumeList
  ) extends Gadget:
    def getSummary(consume: ConsumeList) =
      val outRes = transformer(consume)
      val (res, sum, trace) = underlying.getSummary(outRes)
      (res, sum, clString(outRes) +: trace)

  /**
    * Creates a conservative simple view that fully consumes the array
    * regardless of how fine grain the accessors were. It is possible
    * to write a more fine grained transformer when the view is static
    * i.e. an aligned view is being used.
    */
  def viewGadget(
      underlying: Gadget,
      shrinks: Seq[Int],
      arrDims: Seq[DimSpec]
  ): ViewGadget =
    // Multiply the resource requirements by the origBanking / shrink.
    // This simulates that shrinking "connects" multiple banks into a
    // single one.
    val resourceMultipliers: Seq[Int] = shrinks
      .zip(arrDims)
      .map({
        case (shrink, (_, oldBank)) => oldBank / shrink
      })

    val transformer: ConsumeList => ConsumeList = (cl: ConsumeList) => {
      // For a shrink view, require each resource implied by one bank.
      // If a view shrinks by a factor of `k`, then the requiring bank `b`
      // is transformed into requiring `b + 0`, `b + 1`, ... `b + k - 1`.
      val reqs = cl
        .zip(resourceMultipliers)
        .map({
          case (req, mul) =>
            req.map(b => 0.until(mul).map(k => mul * b + k)).flatten
        })

      // Consume at least all of the banks.
      val allBanks = arrDims.map(_._2).map(0 until _)
      reqs
        .zip(allBanks)
        .map({
          // Remove all the common elements and consume at least the entire array.
          // This handles the case when the consume list is larger than all.
          case (cl, all) => cl.diff(all).appendedAll(all)
        })
    }
    ViewGadget(underlying, transformer)

  /**
    * Creates logic for a split view. A split view always has an even number
    * of dimensions which are grouped. For now, the implementation simply
    * consumes the entire underlying array. It is possible to refine this
    * when static accessors are used. For now, we ignore the [[splitDims]]
    * parameter completely.
    */
  def splitGadget(
      underlying: Gadget,
      arrayDims: Seq[DimSpec],
      @deprecated("Not used", "0.0.1") splitDims: Seq[DimSpec]
  ): ViewGadget =
    viewGadget(underlying, arrayDims.map(_._2), arrayDims)
