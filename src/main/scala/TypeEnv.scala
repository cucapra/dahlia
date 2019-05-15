package fuselang

import scala.util.parsing.input.Position

import Syntax._
import Errors._
import TypeInfo._
import ScopeMap._
import CompilerError._

import Utils.RichOption

object TypeEnv {

  val emptyEnv: Environment = Env()(1)

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
  sealed trait Environment {

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
     * Returns a type with all nested TAliases resolved to a concrete type.
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
    def consumeDim(id: Id, dim: Int)
                  (implicit pos: Position): Environment =
      this(id).matchOrError(pos, "array access", "array type"){ case TArray(_, dims) =>
        val (_, bank) = dims.lift(dim).getOrThrow(UnknownDim(id, dim))
        val banks = 0.until(bank)

        banks.foldLeft(this)({ case (env, bank) => env.consumeBank(id, dim, bank)})
      }

    def consumeAll(id: Id)(implicit pos: Position): Environment =
      this(id).matchOrError(pos, "array access", "array type"){ case TArray(_, dims) =>
        dims.zipWithIndex.foldLeft(this)({ case (env, (_, dim)) => env.consumeDim(id, dim) })
      }

    /**
     * Create a new Environment with all the bindings in [[binds]] added to the
     * current scope.
     * @param binds A scope with bindings to be added to the environment.
     * @returns A new environment with all the bindings in the environment.
     */
    def ++(binds: Map[Id, Type]): Environment =
      binds.foldLeft[Environment](this)({ case (e, b) => e.add(b._1, b._2) })

    /**
     * Merge this environment with [[that]] to create e' such that for each
     * identifier id, e'.banks(id) <= this.banks(id) and e'.banks(id) <=
     * that.banks(id).
     * @assumes: this.getBoundIds == that.getBoundIds
     */
    def merge(that: Environment): Environment

    /**
     * Open a new scope and run commands in it. When the scope ends, the
     * bindings and capabilities bound in this scope are returned
     *
     * @param inScope Commands executed with the inner scope.
     * @param resources Amount of resources required inside new scope.
     * @returns A new environment without the topmost scope and a Scope
     *          containing all bindings in the topmost scope.
     */
    def withScope(resources: Int)
                 (inScope: Environment => Environment): (Environment, Map[Id, Type])

    /**
     * @returns The total amount of resources required by the environment
     */
    def getResources: Int
  }

  private case class Env(
    typeMap: ScopedMap[Id, Info] = ScopedMap(),
    typeDefMap: Map[Id, Type] = Map())
  (implicit val res: Int) extends Environment {

    type TypeScope = Map[Id, Info]
    type CapScope = Map[Expr, Capability]

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
    def apply(id: Id) = getInfo(id).typ
    def add(id: Id, typ: Type) = typeMap.add(id, Info(id, resolveType(typ))) match {
      case None => throw AlreadyBound(id)
      case Some(tMap) => this.copy(typeMap = tMap)
    }
    def update(id: Id, typ: Info) = typeMap.update(id, typ) match {
      case None => throw UnboundVar(id)
      case Some(tMap) => this.copy(typeMap = tMap)
    }
    def consumeBank(id: Id, dim: Int, bank: Int)
    (implicit pos: Position): Environment = {
      val tMap = typeMap
        .update(id, this.getInfo(id).consumeBank(dim, bank))
        .getOrThrow(UnboundVar(id))
        this.copy(typeMap = tMap)
    }

    val getResources = res

    /** Helper functions for Mergable[Env] */
    def getInfo(id: Id): Info = typeMap(id) match {
      case Some(info) => info
      case None => throw UnboundVar(id)
    }
    lazy val getBoundIds = typeMap.keys
    def merge(env: Environment): Environment = env match {
      case that:Env =>
        // Sanity check: The same set of ids are bound by both environments
        if (this.getBoundIds != that.getBoundIds) {
          throw Impossible(
            "Trying to merge two environments which bind different sets of ids." +
            s" Intersection of bind set: ${this.getBoundIds & that.getBoundIds}")
        }

        // For each bound id, set consumed banks to the union of consumed bank sets
        // from both environments
        this.getBoundIds.foldLeft[Env](that)({ case (env, id) => {
          env.update(id, env.getInfo(id) merge this.getInfo(id))
        }})
    }

    /** Helper functions for ScopeManager */
    def addScope(resources: Int) = {
      Env(typeMap.addScope, typeDefMap)(res * resources)
    }
    def endScope(resources: Int) = {
      val scopes = for {
        (tScope, tMap) <- typeMap.endScope
      } yield (Env(tMap, typeDefMap)(res / resources), tScope)

      scopes match {
        case None => throw Impossible("Removed topmost scope")
        case Some((env, map)) => env -> map.map({ case (id, info) => id -> info.typ })
      }
    }
    def withScope(resources: Int)(inScope: Environment => Environment) =
      inScope(this.addScope(resources)) match {
        case env:Env => env.endScope(resources)
      }
  }
}

