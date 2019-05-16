package fuselang

/**
 * Set that provides support for Scopes.
 */
case class ScopedSet[V]
                    (val setList: List[Set[V]] = List(Set[V]()))
                    extends AnyVal {

  override def toString =
    setList.map(set => s"{${set.mkString(", ")}}").mkString("->")

  def contains(v: V): Boolean = setList.exists(set => set.contains(v))

  def add(v: V): ScopedSet[V] =
    this.copy(setList = setList.head + v :: setList.tail)

  /** Managing Scopes */
  def addScope: ScopedSet[V] = this.copy(setList = Set[V]() :: setList)

  def endScope: Option[(Set[V], ScopedSet[V])] = setList match {
    case Nil => None
    case hd :: tl => Some((hd, this.copy(setList = tl)))
  }
}
