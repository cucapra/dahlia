package fuselang.backend.calyx

import scala.math.{max, BigInt}
import BigInt.int2bigInt
import scala.util.parsing.input.{Position}

import fuselang.common._
import Syntax._
import CompilerError._

object Helpers {

  val slowBinops: List[String] = List("*", "/", "%")

  /** Given a binary string, returns the negated two's complement
    * representation.
    */
  def negateTwosComplement(bitString: String): String = {
    if (bitString.forall(_ == '0')) {
      bitString
    } else {
      val t = bitString
        .replaceAll("0", "_")
        .replaceAll("1", "0")
        .replaceAll("_", "1")
      (BigInt(t, 2) + 1).toString(2)
    }
  }

  /** Given an integer, returns the corresponding zero-padded string of size
    * `width`.
    */
  def binaryString(value: BigInt, width: Int): String = {
    val s = value.toString(2)
    "0" * max(width - s.length(), 0) + s
  }

  /** Extracts the bits needed from an optional type annotation. Returns (total
    * size, Option[integral]) bits for the computation.
    */
  def bitsForType(t: Option[Type], pos: Position): (Int, Option[Int]) = {
    t match {
      case Some(TSizedInt(width, _)) => (width, None)
      case Some(TFixed(t, i, _)) => (t, Some(i))
      case Some(_: TBool) => (1, None)
      case Some(_: TVoid) => (0, None)
      case Some(x) =>
        throw NotImplemented(
          s"Calyx cannot infer bitwidth for type $x. Please manually annotate it using a cast expression.",
          pos
        )
      case None =>
        throw Impossible(
          s"Explicit type missing. Try running with `--lower` or report an error with a reproducible program."
        )
    }
  }

  /** Returns true if the given int or fixed point is signed
    */
  def signed(typ: Option[Type]): Boolean = {
    typ match {
      case Some(TSizedInt(_, un)) => un == false
      case Some(TFixed(_, _, un)) => un == false
      case _ => false
    }
  }

}
