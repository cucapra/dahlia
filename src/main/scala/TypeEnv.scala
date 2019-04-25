package fuselang

import scala.util.parsing.input.Position

import Syntax._
import Errors._
import TypeInfo._
import Utils.RichOption

object TypeEnv {

  /**
   * An Environment that keeps tracks of Lexical Scopes.
   */
  trait ScopeManager[T <: ScopeManager[T]] {
    /**
     * A Scope in the environment. A scope is a collection of bindings in the
     * current lexical scope.
     */
    type Scope[K, V] = Map[K, V]

    /**
     * Open a new scope and run commands in it. When the scope ends, the
     * bindings and capabilities bound in this scope are returned
     *
     * @param inScope Commands executed with the inner scope.
     * @param resources Amount of resources required inside new scope.
     * @returns A new environment without the topmost scope and a Scope
     *          containing all bindings in the topmost scope.
     */
    def withScope(resources: Int)(inScope: T => T): (T, Scope[Id, Type]) =
      inScope(this.addScope(resources)).endScope(resources)

    /**
     * Add a new scope which consumes [[resources]].
     * @param resources Amount of resources the new Scope consumes
     */
    def addScope(resources: Int): T

    /**
     *  Remove the topmost scope from the environment. The bindings and capabilities
     *  bound in this scope are removed from the context.
     *  This method is only used for interface implementation and should not be
     *  used directly.
     *  @param resources Number of resources to be removed from the scope.
     *  @returns A new environment without the topmost scope and a Scope
     *           containing all bindings in the topmost scope.
     */
    def endScope(resources: Int): (T, Scope[Id, Type])

    /**
     * @returns The total amount of resources required by the environment
     */
    def getResources: Int
  }

  trait Mergable[T <: Mergable[T]] {

    val getBoundIds: Set[Id]

    def getInfo(id: Id): Info

    def update(id: Id, info: Info): T

    /**
     * Merge this environment with [[that]] to create e' such that for each
     * identifier id, e'.banks(id) <= this.banks(id) and e'.banks(id) <=
     * that.banks(id).
     * @assumes: this.getBoundIds == that.getBoundIds
     */
    def merge(that: T): T = {
      // Sanity check: The same set of ids are bound by both environments
      if (this.getBoundIds != that.getBoundIds) {
        throw Impossible(
          "Trying to merge two environments which bind different sets of ids." +
          s" Intersection of bind set: ${this.getBoundIds & that.getBoundIds}")
      }

      // For each bound id, set consumed banks to the union of consumed bank sets
      // from both environments
      this.getBoundIds.foldLeft[T](that)({ case (env, id) => {
        env.update(id, env.getInfo(id) merge this.getInfo(id))
      }})
    }
  }

  /**
   * An environment keep tracks of information for type checking:
   * - The typedefs bound at the start of the program.
   * - The association of Identifiers to corresponding types.
   * - The association of Expressions to the capabilities acquired.
   * - Number of resources nested unrolled contexts imply.
   *
   * The environment structure is built using a chain of scope over the
   * last two associations. A scope is a logical grouping of assoication
   * corresponding to lexical scope in programs.
   */
  trait Environment
    extends ScopeManager[Environment]
    with Mergable[Environment] {

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
     * Type binding manipulation
     */
    /**
     * Use application syntax to get a type bindings for [[id]].
     * @param id The id whose binding is needed
     * @returns The [[Type]] associated to the id
     * @throws [[UnboundVar]] If there is no bindings for id
     */
    def apply(id: Id): Type

    /**
     * Add a new type binding in the current scope. If the type being added is
     * an alias type, instead add the resolved underlying type.
     * @param id The identifier to which the type is mapped.
     * @param typ The [[Type]] associated with the identifier.
     * @return A new environment which contains the mapping.
     * @throws [[AlreadyBound]] if a bindings for Id already exists.
     */
    def add(id: Id, typ: Type): Environment

    /**
     * Returns a new Environment where the [[bank]] in [[dim]] is consumed.
     * @param id The name of the TArray that is being updated
     * @param dim The dimension of the of array.
     * @param bank The bank of the array to be consumed.
     * @throws [[AlreadyConsumed]] If the [[bank]] in [[dim]] of [[id]] has
     *                             already been consumed.
     * @throws [[UnknownDim]] If the dimension for the array doesn't exist.
     */
    def consumeBank(id: Id, dim: Int, bank: Int)(implicit pos: Position): Environment

    /**
     * Convinience methods for fully consuming dimensions or arrays.
     */
    def consumeDim(id: Id, dim: Int, unrollFactor: Int)
                  (implicit pos: Position): Environment =
      this(id).matchOrError(pos, "array access", "array type"){ case TArray(_, dims) =>
        val (_, bank) = dims.lift(dim).getOrThrow(UnknownDim(id, dim))
        // TODO(rachit): This check should be in TypeCheck
        if (unrollFactor != bank) {
          throw BankUnrollInvalid(bank, unrollFactor)
        }
        val banks = 0.until(bank)

        banks.foldLeft(this)({ case (env, bank) => env.consumeBank(id, dim, bank)})
      }

    def consumeAll(id: Id)(implicit pos: Position): Environment =
      this(id).matchOrError(pos, "array access", "array type"){ case TArray(_, dims) =>
        dims.zipWithIndex.foldLeft(this)({ case (env, ((_, bank), dim)) => env.consumeDim(id, dim, bank) })
      }

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
    def ++(binds: Scope[Id, Type]): Environment =
      binds.foldLeft[Environment](this)({ case (e, b) => e.add(b._1, b._2) })
  }

}
