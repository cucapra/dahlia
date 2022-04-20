package fuselang.common

import scala.collection.immutable.Map

object MultiSet:

  def emptyMultiSet[K](): MultiSet[K] = MultiSet[K](Map[K, Int]())

  def fromSeq[K](seq: Seq[K]): MultiSet[K] =
    MultiSet(seq.foldLeft(Map[K, Int]())({
      case (ms, v) =>
        if ms.contains(v) then ms + (v -> (ms(v) + 1))
        else ms + (v -> 1)
    }))

  case class MultiSet[K](val setMap: Map[K, Int]) extends AnyVal:

    /**
      * Contains at least [[num]] copies of [[element]]
      */
    def containsAtLeast(element: K, num: Int): Boolean =
      setMap.get(element).map(_ >= num).getOrElse(false)

    override def toString: String =
      setMap.toList
        .map({ case (v, n) => List.tabulate(n)(_ => v) })
        .flatten
        .mkString("{", ", ", "}")

    /**
      * Apply [[op]] on the values associated with the same key in [[this]] and [[that]].
      */
    def zipWith(that: MultiSet[K], op: (Int, Int) => Int): MultiSet[K] =
      val thatMap = that.setMap
      val (thisKeys, thatKeys) = (setMap.keys.toSet, thatMap.keys.toSet)
      if thisKeys != thatKeys then
        throw new NoSuchElementException(
          s"Element ${thisKeys.diff(thatKeys).head} not in both multisets.\nThis: ${setMap}\nThat: ${thatMap}."
        )
      MultiSet(setMap.map({ case (k, v) => k -> op(v, thatMap(k)) }))

    /** Calculate multiset difference */
    def diff(that: MultiSet[K]): MultiSet[K] =
      MultiSet(setMap.map({
        case (k, v) => {
          k -> (if that.setMap.contains(k) then (v - that.setMap(k)) else v)
        }
      }))

    def add(key: K): MultiSet[K] = MultiSet(setMap + (key -> (setMap.getOrElse(key, 0) + 1)))

    def remove(key: K): MultiSet[K] =
      MultiSet(setMap + (key -> (setMap.getOrElse(key, 0) - 1)))

    def keys = setMap.keys

    def forall: (((K, Int)) => Boolean) => Boolean = setMap.forall _

    def find: (((K, Int)) => Boolean) => Option[(K, Int)] = setMap.find _

    def getCount(k: K): Int = setMap(k)


