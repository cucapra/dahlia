package fuselang.typechecker

import scala.math.{max,log10,ceil,abs,pow,round}
import fuselang.common.Syntax._

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
  def bitsNeeded(n: Int):Int = n match {
    case 0 => 1
    case n if n > 0 => ceil(log10(n + 1)/log10(2)).toInt
    case n if n < 0 => bitsNeeded(abs(n)) + 1
  }
  def doubleBitsNeeded(n: Double, l:Int , i:Int):Boolean = (n,l,i) match {
    case (n,l,i) => round((n - n.toInt)*pow(2,l-i))==(n - n.toInt)*pow(2,l-i) && bitsNeeded(n.toInt) < i 
  }

  def areEqual(t1: Type, t2: Type) = (t1, t2) match {
    case (TStaticInt(v1), TStaticInt(v2)) => v1 == v2
    case (_:TIndex, _:TIndex) => true
    case (_:TFloat, _:TFloat) => true
    case _ => t1 == t2
  }

  def isSubtype(sub: Type, sup: Type): Boolean = (sub, sup) match {
    case (TSizedInt(v1, un1), TSizedInt(v2, un2)) => un1 == un2 && v1 <= v2
    case (TStaticInt(v1), TSizedInt(v2,un2)) => ( (v1<0 && un2==false) | (v1>=0) ) && bitsNeeded(v1)<=v2   
    case (TSizedInt(v1, un1), TSizedDouble(_,v2, un2)) => un1 == un2 && v1 <= v2
    case (TSizedDouble(v1, i1, un1), TSizedDouble(v2, i2, un2)) => un1 == un2 && v1 <= v2 && i1 <= i2
    case (TStaticInt(v1), TSizedDouble(_, i2, un2)) => ( (v1<0 && un2==false) | (v1>=0) ) && bitsNeeded(v1)<=i2  
    case (TStaticDouble(v1), TSizedDouble(v2, i2, un2) ) => {println(v1,v2,i2,un2); ( (v1<0 && un2==false) | (v1>=0) ) && doubleBitsNeeded(v1,v2,i2)}
    case (_:TStaticInt, _:TIndex) => true
    case (TArray(tsub, subDims), TArray(tsup, supDims)) => {
      // Arrays are invariant
      areEqual(tsup, tsub) && subDims == supDims
    }
    case (_:TFloat, _:TDouble) => true
    case _ => areEqual(sub, sup)
  }

  private def joinOfHelper(t1: Type, t2: Type, op: BOp): Option[Type] = (t1, t2) match {
    case (TStaticInt(v1), TStaticInt(v2)) => op.toFun match {
      case Some(fun) => Some(TStaticInt(fun(v1, v2)))
      case None => Some(TSizedInt(max(bitsNeeded(v1), bitsNeeded(v2)), false))
    }
    case (TSizedInt(s1, un1), TSizedInt(s2, un2)) =>
      if (un1 == un2) Some(TSizedInt(max(s1, s2), un1))
      else None
    case (TSizedInt(s, un), TStaticInt(v)) =>
      Some(TSizedInt(max(s, bitsNeeded(v)), un))
    case (TStaticInt(v), TSizedInt(s, un)) =>
      Some(TSizedInt(max(s, bitsNeeded(v)), un))
    case (st:TStaticInt, idx:TIndex) =>
      // Infer unsigned
      Some(TSizedInt(bitsNeeded(max(idx.maxVal, st.v)), false))
    case (t2:TSizedInt, _:TIndex) => Some(t2)
    case (_:TFloat, _:TFloat) => Some(TFloat())
    case (_:TFloat, _:TDouble) => Some(TDouble())
    case (ti1:TIndex, ti2:TIndex) =>
      Some(TSizedInt(max(bitsNeeded(ti1.maxVal), bitsNeeded(ti2.maxVal)), false))
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

  def safeCast(originalType: Type, castType: Type) = (originalType, castType) match {
    case (t1:IntType, t2:TSizedInt) =>  isSubtype(t1, t2)
    case (t1:TStaticDouble, t2:TSizedDouble) =>  isSubtype(t1, t2)
    case (_:TFloat, _:TSizedInt) => false
    case (_:IntType, _:TFloat) => true
    case (_:TFloat, _:TDouble) => true
    case (t1, t2) => areEqual(t1, t2)
  }
}
