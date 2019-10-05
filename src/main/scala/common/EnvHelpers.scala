package fuselang.common

object EnvHelpers {

  trait ScopeManager[T <: ScopeManager[_]] {

    /**
     * Open a new scope and run commands in it. When the scope ends, the
     * modified environment is returned.
     *
     * @param inScope Commands executed inside a new Scope level.
     */
    def withScope(inScope: T => T): T

    /**
     * Merge this environment with [[that]] for some abstract merge function.
     * @assumes: this.getBoundIds == that.getBoundIds
     */
    def merge(that: T): T
  }

  /**
   * An environment that keeps track of mapping of resource [[V]] using keys
   * of type [[V]].
   */
  trait Tracker[K, V, T <: Tracker[_, _, _]] {

    /**
     * Add the resource to the current environment.
     */
    def add(k: K, v: V): T

    /**
     * Get the resource associated with key if it is present.
     */
    def get(k: K): Option[V]
  }

}
