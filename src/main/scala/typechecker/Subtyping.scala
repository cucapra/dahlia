package fuselang.typechecker

import scala.math.max
import fuselang.common.Syntax._
import fuselang.Utils.bitsNeeded

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
  def areEqual(t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (TStaticInt(v1), TStaticInt(v2)) => v1 == v2
    case (_: TIndex, _: TIndex) => true
    case (_: TFloat, _: TFloat) => true
    case _ => t1 == t2
  }

  def isSubtype(sub: Type, sup: Type): Boolean = (sub, sup) match {
    case (TSizedInt(v1, un1), TSizedInt(v2, un2)) => un1 == un2 && v1 <= v2
    case (TStaticInt(v1), TSizedInt(v2, un2)) =>
      ((v1 < 0 && un2 == false) || (v1 >= 0)) && bitsNeeded(v1) <= v2
    case (_: TIndex, _: TSizedInt) => true
    case (_: TStaticInt, _: TIndex) => true
    case (TArray(tsub, subDims, p1), TArray(tsup, supDims, p2)) => {
      // Arrays are invariant
      areEqual(tsup, tsub) && subDims == supDims && p1 == p2
    }
    case (_: TFloat, _: TDouble) => true
    case (_: TRational, _: TFloat) => true
    case (_: TRational, _: TDouble) => true
    case (TRational(v1), TFixed(_, i2, un2)) =>
      ((v1.toDouble < 0 && un2 == false) || (v1.toDouble >= 0)) && bitsNeeded(
        v1.toDouble.toInt
      ) <= i2
    case (TFixed(t1, i1, un1), TFixed(t2, i2, un2)) =>
      (un1 == un2 && i1 <= i2 && (t1 - i1) <= (t2 - i2))
    case _ => areEqual(sub, sup)
  }

  private def joinOfHelper(t1: Type, t2: Type, op: BOp): Option[Type] =
    (t1, t2) match {
      //XXX(Zhijing): what happens for multiplication? Overflow?
      case (TStaticInt(v1), TStaticInt(v2)) =>
        op.toFun match {
          case Some(fun) =>
            Some(TStaticInt(fun(v1.toDouble, v2.toDouble).toInt))
          case None =>
            Some(TSizedInt(max(bitsNeeded(v1), bitsNeeded(v2)), false))
        }
      case (TRational(v1), TRational(v2)) =>
        op.toFun match {
          //XXX(Zhijing):deprecated
          case Some(fun) =>
            Some(TRational(fun(v1.toDouble, v2.toDouble).toString))
          case None =>
            if (bitsNeeded(v1.toDouble.toInt) > bitsNeeded(v2.toDouble.toInt))
              Some(TRational(v1))
            else Some(TRational(v2))
        }
      case (TSizedInt(s1, un1), TSizedInt(s2, un2)) =>
        if (un1 == un2) Some(TSizedInt(max(s1, s2), un1))
        else None
      case (TSizedInt(s, un), TStaticInt(v)) =>
        Some(TSizedInt(max(s, bitsNeeded(v)), un))
      case (st: TStaticInt, idx: TIndex) =>
        // Infer unsigned
        Some(TSizedInt(bitsNeeded(max(idx.maxVal, st.v)), false))
      case (t2: TSizedInt, _: TIndex) => Some(t2)
      case (_: TFloat, _: TDouble) => Some(TDouble())
      case (_: TRational, _: TFloat) => Some(TFloat())
      case (_: TRational, _: TDouble) => Some(TDouble())
      case (TRational(v1), TFixed(t2, i2, un2)) =>
        Some(
          TFixed(
            max(i2, bitsNeeded(v1.toDouble.toInt)) + t2 - i2,
            max(i2, bitsNeeded(v1.toDouble.toInt)),
            un2
          )
        )
      case (TFixed(t1, i1, un1), TFixed(t2, i2, un2)) =>
        if (un1 == un2)
          Some(TFixed(max(t1 - i1, t2 - i2) + max(i1, i2), max(i1, i2), un1))
        else None
      case (ti1: TIndex, ti2: TIndex) =>
        Some(
          TSizedInt(max(bitsNeeded(ti1.maxVal), bitsNeeded(ti2.maxVal)), false)
        )
      case (t1, t2) => if (t1 == t2) Some(t1) else None
    }

  /**
    * Try finding the join of either ordering and use the result.
    */
  def joinOf(t1: Type, t2: Type, op: BOp): Option[Type] = {
    val j1 = joinOfHelper(t1, t2, op)
    if (j1.isDefined) j1
    else joinOfHelper(t2, t1, op)
  }

  def safeCast(originalType: Type, castType: Type): Boolean =
    (originalType, castType) match {
      case (t1: IntType, t2: TSizedInt) => isSubtype(t1, t2)
      case (_: TFloat, _: TSizedInt) => false
      case (_: TDouble, _: TSizedInt) => false
      case (_: TRational, _: TSizedInt) => false

      case (_: IntType, _: TFloat) => true
      case (_: TFloat, _: TDouble) => true
      case (_: TRational, _: TDouble) => true
      case (TSizedInt(i1, un1), TFixed(_, i2, un2)) => (un1 == un2 && i1 <= i2)
      case (t1, t2) => areEqual(t1, t2)
    }
}
