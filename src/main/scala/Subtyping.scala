package fuselang

import scala.math.{max,log10,ceil}
import Syntax._

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
 *  Canonically, we call this type an "upper bound" and a "least upper bound"
 *  if there is no type T' statisfying T' < T and is an upper bound for t1 and
 *  t2.
 */
object Subtyping {
  def bitsNeeded(n: Int) = n match {
    case 0 => 1
    case n => ceil(log10(n)/log10(2)).toInt + 1
  }

  def areEqual(t1: Type, t2: Type) = (t1, t2) match {
    case (TStaticInt(v1), TStaticInt(v2)) => v1 == v2
    case (_:TIndex, _:TIndex) => true
    case (_:TFloat, _:TFloat) => true
    case _ => t1 == t2
  }

  def isSubtype(sub: Type, sup: Type): Boolean = (sub, sup) match {
    case (TSizedInt(v1), TSizedInt(v2)) => v1 <= v2
    case (_:IntType, _:TSizedInt) => true
    case (_:TStaticInt, _:TIndex) => true
    case (TArray(tsub, subDims), TArray(tsup, supDims)) => {
      // Arrays are invariant
      areEqual(tsup, tsub) && subDims == supDims
    }
    case _ => areEqual(sub, sup)
  }

  def joinOf(t1: Type, t2: Type, op: BOp): Option[Type] = (t1, t2) match {
    case (TStaticInt(v1), TStaticInt(v2)) => op.toFun match {
      case Some(fun) => Some(TStaticInt(fun(v1, v2)))
      case None => Some(TSizedInt(max(bitsNeeded(v1), bitsNeeded(v2))))
    }
    case (TSizedInt(s1), TSizedInt(s2)) =>
      Some(TSizedInt(max(s1, s2)))
    case (TStaticInt(v), TSizedInt(s)) => {
      Some(TSizedInt(max(s, bitsNeeded(v))))
    }
    case (TSizedInt(s), TStaticInt(v)) => {
      Some(TSizedInt(max(s, bitsNeeded(v))))
    }
    case (idx:TIndex, st:TStaticInt) =>
      Some(TSizedInt(bitsNeeded(max(idx.maxVal, st.v))))
    case (st:TStaticInt, idx:TIndex) =>
      Some(TSizedInt(bitsNeeded(max(idx.maxVal, st.v))))
    case (_: TIndex, t2@TSizedInt(_)) => Some(t2)
    case (t2@TSizedInt(_), _:TIndex) => Some(t2)
    case (_:TFloat, _:TFloat) => Some(TFloat())
    case (ti1:TIndex, ti2:TIndex) =>
      Some(TSizedInt(max(bitsNeeded(ti1.maxVal), bitsNeeded(ti2.maxVal))))
    case (t1, t2) => if (t1 == t2) Some(t1) else None
  }
}
