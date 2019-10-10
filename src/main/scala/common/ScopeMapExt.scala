package fuselang.common

import CompilerError._
import fuselang.Utils._

object ScopeMapExt {

  /**
   * A map that undestands scopes. A ScopedMap is a chain of maps from
   * [[K]] to [[V]].
   */
  case class ScopedMapExt[K, V]
                      (val mapList: List[Map[K, V]] = List(Map[K, V]()))
                      extends AnyVal {

    override def toString =
      mapList
        .map(map => s"<${map.map({case (k, v) => s"$k -> $v"}).mkString(", ")}>")
        .mkString(" => ")

    def isEmpty: Boolean = mapList.isEmpty
    def get(key: K): Option[V] = mapList.head.get(key)
      //mapList.find(map => map.get(key).isDefined).map(c => c(key))

    /**
     * Add key -> value binding to the topmost scope.
     */
    def add(key: K, value: V): ScopedMapExt[K, V] = {
      this.copy(mapList = mapList.head + (key -> value) :: mapList.tail)
    }
    /*
    get(key) match {
      case Some(_) => this
      case None => this.copy(mapList = mapList.head + (key -> value) :: mapList.tail)
    }*/

    /**
     * Update the binding for [[key]] to [[value]]. The update method walks
     * the scope chain to find where the implementation is bound and update it.
     * @returns a new [[ScopedMap]] with the key bound to value
     * @throw [[Errors.Unbound]] If the key is not found.
    
    def update(key: K, value: V): ScopedMapExt[K, V] = {
      val scope = mapList.indexWhere(m => m.get(key).isDefined)
      assertOrThrow(scope > -1, Impossible(s"$key was not found."))
      val newMapList = mapList.updated(scope, mapList(scope) + (key -> value))
      this.copy(mapList = newMapList)
    }*/
    
    /** Methods to manage scopes. */
    def addScope: ScopedMapExt[K, V] = ScopedMapExt(Map[K, V]() :: mapList)

    def endScope: Option[(Map[K, V], ScopedMapExt[K, V])] = mapList match {
      case Nil => None
      case hd :: tl => Some( (hd, this.copy(mapList = tl)) )
    }

    /** Return the set of keys at head. */
    def keys = mapList.head.keySet

    // Convinience methods
    def apply(k: K) = get(k).getOrThrow(Impossible(s"$k was not found in $this."))

    def +(bind: (K, V)) = add(bind._1, bind._2)
  }
}
