package fuselang

import Syntax._
import Errors._
import ScopeMap._
import TypeInfo._

object TypeEnv {
  // Capabilities for read/write
  sealed trait Capability
  case object Read extends Capability
  case object Write extends Capability

  val emptyEnv: Environment = Env()(1)

  // Product of all unroll factors enclosing the current context.
  type ReqResources = Int

  /**
   * An environment keep tracks of information for type checking:
   * - The typedefs bound at the start of the program.
   * - The association of Identifiers to corresponding types.
   * - The association of Expressions to the capabilities acquired.
   * - Number of resources nested unrolled contexts imply.
   * The environment structure is built using a chain of scope over the
   * last two associations. A scope is a logical grouping of assoication
   * corresponding to lexical scope in programs.
   */
  trait Environment {

    // A Scope in the environment. A scope is a collection of bindings in the
    // current lexical scope.
    private type Scope[K, V] = Map[K, V]

    /**
     * Methods to manipulate type defs. Since the typedefs are top level,
     * they don't need to use scopes.
     */
    /**
     * Creates a new alias for the [[typ]].
     * @throws [[AlreadyBoundType]] if the type definition is already bound.
     */
    def addType(alias: Id, typ: Type): Environment

    /**
     * Returns the type [[alias]] is associated to.
     * @throws [[UnboundType]] if the alias has no binding for types.
     */
    def getType(alias: Id): Type

    /**
     * Returns a type with recursively resolved TAlias.
     */
    def resolveType(typ: Type): Type

    /**
     * Methods to manipulate the scopes with the environment.
     * INVARIANT: There is at least one scope in the environment.
     */
    /** Create a new binding scope in the environment. */
    def addScope: Environment
    /**
     *  Remove the topmost scope from the environment. The bindings and capabilities
     *  bound in this scope are removed from the context.
     *  @returns A new environment without the topmost scope, A Scope containing
     *           all bindings in the topmost scope, A scope containing capability mappings.
     */
    def endScope: (Environment, Scope[Id, Info], Scope[Expr, Capability])

    /**
     * Type binding manipulation
     */
    /**
     * Use application syntax to get a type bindings for [[id]].
     * @param id The id whose binding is needed
     * @returns Information associated with the id
     * @throws [[UnboundVar]] If there is no bindings for id
     */
    def apply(id: Id): Info

    /**
     * Add a new type binding in the current scope. If the type being added is
     * an alias type, instead add the resolved underlying type.
     * @param id The identifier to which the type is mapped.
     * @param typ The [[Info]] associated with the identifier.
     * @return A new environment which contains the mapping.
     * @throws [[AlreadyBound]] if a bindings for Id already exists.
     */
    def add(id: Id, typ: Type): Environment

    /**
     * Update bindings associated with Id. Method traverses the entire scope chain.
     * @param id The identifier whose type is being updated.
     * @param typ The new type associated with the identifier.
     * @return A new environment where Id is bound to this Type.
     * @throw [[UnboundVar]] If the Id doesn't already have a binding
     */
    def update(id: Id, typ: Info): Environment

    /**
     * Capability manipulation
     */
    /** Get the capability associated with [[e]].
     *  @param e The Expr to get the capability for.
     *  @returns The Capability associated with e. Returns None if no association is found.
     */
    def getCap(e: Expr): Option[Capability]
    /**
     * Associate the capabilitie [[cap]] to the expressions [[e]].
     * @param e The param with which the capability is associated
     * @param cap The capability the expression has.
     * @return A new environment which contains the capability mapping.
     */
    def addCap(e: Expr, cap: Capability): Environment

    /**
     * Create a new Environment with all the bindings in [[binds]] added to the
     * current scope.
     * @param binds A scope with bindings to be added to the environment.
     * @returns A new environment with all the bindings in the environment.
     */
    def ++(binds: Scope[Id, Info]): Environment =
      binds.foldLeft[Environment](this)({ case (e, b) => e.add(b._1, b._2.typ) })

    /**
     * Merge this environment with [[that]] to create e' such that for each
     * identifier id, e'.banks(id) <= this.banks(id) and e'.banks(id) <=
     * that.banks(id).
     * @assumes: this.getBoundIds == that.getBoundIds
     */
    def merge(that: Environment): Environment = {
      // Sanity check: The same set of ids are bound by both environments
      if (this.getBoundIds != that.getBoundIds) {
        throw Impossible(
          s"Trying to merge two environments which bind different sets of ids." +
          " Intersection of bind set: ${this.getBoundIds & that.getBoundIds}")
      }

      // For each bound id, set consumed banks to the union of consumed bank sets
      // from both environments
      this.getBoundIds.foldLeft[Environment](this)({ case (env, id) => {
        env.update(id, env(id) merge that(id))
      }})
    }

    /**
     * @returns a set with all the bound Ids in this environment
     */
    def getBoundIds: Set[Id]

  }

  private case class Env(
    typeMap: ScopedMap[Id, Info] = ScopedMap(),
    capMap: ScopedMap[Expr, Capability] = ScopedMap(),
    typeDefMap: Map[Id, Type] = Map())
    (implicit val rres: Int) extends Environment {

    type TypeScope = Map[Id, Info]
    type CapScope = Map[Expr, Capability]

    /** Scope management */
    def addScope = Env(typeMap.addScope, capMap.addScope, typeDefMap)
    def endScope = {
      val scopes = for {
        (tScope, tMap) <- typeMap.endScope;
        (cScope, cMap) <- capMap.endScope
      } yield (Env(tMap, cMap, typeDefMap), tScope, cScope)

      scopes match {
        case None => throw Impossible("Removed topmost scope")
        case Some(res) => res
      }
    }

    /** Capability methods */
    def getCap(expr: Expr): Option[Capability] = capMap(expr)
    def addCap(expr: Expr, cap: Capability): Environment = capMap.add(expr, cap) match {
      case Some(cMap) => this.copy(capMap = cMap)
      case None => throw Impossible(s"Capability for $expr already exists.")
    }

    /** Type defintions */
    def resolveType(typ: Type): Type = typ match {
      case TAlias(n) => getType(n)
      case TFun(args) => TFun(args.map(resolveType(_)))
      case arr@TArray(t, _) => arr.copy(typ = resolveType(t))
      case t => t
    }

    def addType(alias: Id, typ: Type) = typeDefMap.get(alias) match {
      case Some(_) => throw AlreadyBoundType(alias)
      case None => this.copy(typeDefMap = typeDefMap + (alias -> typ))
    }
    def getType(alias: Id) = typeDefMap.get(alias) match {
      case Some(t) => t
      case None => throw UnboundType(alias)
    }

    /** Type binding methods */
    def apply(id: Id): Info = typeMap(id) match {
      case Some(info) => info
      case None => throw UnboundVar(id)
    }

    def add(id: Id, typ: Type) = typeMap.add(id, Info(id, resolveType(typ))) match {
      case None => throw AlreadyBound(id)
      case Some(tMap) => this.copy(typeMap = tMap)
    }

    def update(id: Id, typ: Info) = typeMap.update(id, typ) match {
      case None => throw UnboundVar(id)
      case Some(tMap) => this.copy(typeMap = tMap)
    }

    lazy val getBoundIds = typeMap.keys
  }
}
