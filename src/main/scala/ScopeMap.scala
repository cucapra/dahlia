package fuselang

object ScopeMap {

  /**
   * A map that undestands scopes. A ScopedMap is a chain of maps from
   * [[K]] to [[V]].
   */
  case class ScopedMap[K, V](private val mapList: List[Map[K, V]] = List(Map[K, V]())) {

    def get(key: K): Option[V] =
      mapList.find(map => map.get(key).isDefined).map(c => c(key))

    /**
     * Add key -> value binding to the topmost scope.
     * @returns None if the value is already bound in the scope chain, otherwise
     *          a new [[ScopedMap]] with the binding in the top most scope.
     */
    def add(key: K, value: V): Option[ScopedMap[K, V]] = get(key) match {
      case Some(_) => None
      case None => Some(this.copy(mapList = mapList.head + (key -> value) :: mapList.tail))
    }

    /**
     * Update the binding for [[key]] to [[value]]. The update method walks
     * the scope chain to find where the implementation is bound and update it.
     * @returns None if [[key]] is not already bound in the scope chains, otherwise
     *          returns a new [[ScopedMap]] with the key bound to value
     */
    def update(key: K, value: V): Option[ScopedMap[K, V]] = get(key) match {
      case None => None
      case Some(_) => {
        val scope = mapList.indexWhere(m => m.get(key).isDefined)
        val newMapList = mapList.updated(scope, mapList(scope) + (key -> value))
        Some(this.copy(mapList = newMapList))
      }
    }

    /** Methods to manage scopes. */
    def addScope: ScopedMap[K, V] = ScopedMap(Map[K, V]() :: mapList)

    def endScope: Option[(Map[K, V], ScopedMap[K, V])] = mapList match {
      case Nil => None
      case hd :: tl => Some((hd, this.copy(mapList = tl)))
    }

    /** Return the set of all keys. */
    def keys = mapList.flatMap(m => m.keys).toSet

    // Convinience methods
    def apply(k: K) = get(k)

    def +(bind: (K, V)) = add(bind._1, bind._2)
  }
}
