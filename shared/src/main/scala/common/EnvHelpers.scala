package fuselang.common

object EnvHelpers:

  trait ScopeManager[T <: ScopeManager[_]] { this: T =>

    /**
      * Open a new scope and run commands in it. When the scope ends, the
      * modified environment is returned.
      *
      * @param inScope Commands executed inside a new Scope level.
      */
    def withScope(inScope: T => T): T =
      inScope(this)

    /**
      * Open a new scope and run commands in it. When the scope ends, the
      * modified environment and the value produced by the function are
      * returned.
      *
      * @param inScope Commands executed inside a new Scope level.
      */
    def withScopeAndRet[V](inScope: T => (V, T)): (V, T) =
      inScope(this)

    /**
      * Merge this environment with [[that]] for some abstract merge function.
      * @assumes: this.getBoundIds == that.getBoundIds
      */
    def merge(that: T): T
  }

  /**
    * An environment that keeps track of mapping of resource [[V]] using keys
    * of type [[K]].
    */
  trait Tracker[K, V, T <: Tracker[_, _, _]] { this: T =>

    /**
      * Add the resource to the current environment.
      */
    def add(k: K, v: V): T

    /**
      * Get the resource associated with key if it is present.
      */
    def get(k: K): Option[V]
  }

  /**
    * Definition of a trivial environment that doesn't track any
    * information.
    */
  final case class UnitEnv() extends ScopeManager[UnitEnv]:
    def merge(that: UnitEnv): UnitEnv = this

