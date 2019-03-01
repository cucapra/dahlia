package fuselang

import scala.math.{max,log10,ceil}
import Syntax._
import Errors.NoJoin

/**
 * Subtyping relations are only defined over the number hierarchy. Read 't1 < t2'
 * as t1 is subtype of t2. The subtyping hierarchy is:
 *
 * sized(n) > sized(n - 1) ... > sized(1) > idx > static
 *
 * Note that all idx types and static types are equal to each other.
 *
 * Subtyping and joins:
 *
 *      a1
 *      |        b1
 *      a2       |
 *      |        b2
 *      a3       |
 *      |        |
 *      +---+----+
 *          |
 *          c1
 *          |
 *          c2
 *
 *  If a, b, c are types, then we have the definitions:
 *
 *  Subtyping: t1 < t2 iff there is a path from t1 to t2 such that the links
 *  only go down.
 *  Example: a1 < a2 == true, a1 < c1 == true, a1 < b1 == false
 *
 *  For subtyping, a types and b types are called "incomparable".
 *
 *  Joins: t1 and t2 have a join T if t1 < T and t2 < T. Informally, this is
 *  described as descending down the trees of t1 and t2 to find a common ancestor.
 *  Canonically, we call this type the "least upper bound"
 */
object Subtyping {
  def bitsNeeded(n: Int) = n match {
    case 0 => 1
    case n => ceil(log10(n)/log10(2)).toInt + 1
  }

  def areEqual(t1: Type, t2: Type) = (t1, t2) match {
    case (TStaticInt(v1), TStaticInt(v2)) => v1 == v2
    case (_:TIndex, _:TIndex) => true
    case _ => t1 == t2
  }

  def isSubtype(sub: Type, sup: Type): Boolean = (sub, sup) match {
    case (TSizedInt(v1), TSizedInt(v2)) => v1 <= v2
    case (_:IntType, _:TSizedInt) => true
    case (_:TStaticInt, _:TIndex) => true
    case (TArray(tsub, _), TArray(tsup, _)) => {
      // Arrays are mutable so we are conservative and disallow subtyping.
      areEqual(tsup, tsub)
    }
    case _ => areEqual(sub, sup)
  }

  def hasJoin(t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (_:IntType, _:IntType) => true
    case _ => false
  }

  def joinOf(t1: Type, t2: Type, op: (Int, Int) => Int) = (t1, t2) match {
    case (TStaticInt(v1), TStaticInt(v2)) => TStaticInt(op(v1, v2))
    case (TSizedInt(s1), TSizedInt(s2)) => TSizedInt(max(s1, s2))
    case (TStaticInt(v), TSizedInt(s)) => {
      TSizedInt(max(s, bitsNeeded(v)))
    }
    case (TSizedInt(s), TStaticInt(v)) => {
      TSizedInt(max(s, bitsNeeded(v)))
    }
    case (idx@TIndex(_, _), TStaticInt(v)) =>
      TSizedInt(max(idx.maxVal, bitsNeeded(v)))
    case (TStaticInt(v), idx@TIndex(_, _)) =>
      TSizedInt(max(idx.maxVal, bitsNeeded(v)))
    case (_: TIndex, t2@TSizedInt(_)) => t2
    case (t2@TSizedInt(_), _:TIndex) => t2
    case (t1, t2) => throw NoJoin(t1, t2)
  }
}
