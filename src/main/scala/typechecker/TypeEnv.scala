package fuselang.typechecker

//import scala.util.parsing.input.Position

import fuselang.Utils._

import fuselang.common._
import ScopeMap._
import Syntax._
import Errors._
import CompilerError._
import EnvHelpers._

object TypeEnv:

  val emptyEnv: Environment = Env()

  /**
    * An environment keep tracks of information for type checking:
    * - The typedefs bound at the start of the program.
    * - The association of Identifiers to corresponding **concrete** types. Any
    *   TAlias in the type are resolved before they are added to the environment.
    * - The return type expected by the current environment.
    *
    * The environment structure is built using a chain of scope over the
    * last two associations. A scope is a logical grouping of assoication
    * corresponding to lexical scope in programs.
    */
  sealed trait Environment extends Tracker[Id, Type, Environment]:

    /**
      * Type binding manipulation
      */
    /**
      * Use application syntax to get a type bindings for [[id]].
      * @param id The id whose binding is needed
      * @returns The [[Type]] associated to the id
      * @throws [[Unbound]] If there is no bindings for id
      */
    def apply(id: Id): Type = this.get(id).getOrThrow(Unbound(id))

    /**
      * Methods to manipulate type defs. Since the typedefs are top level,
      * they don't need to use scopes.
      */
    /**
      * Creates a new alias for the [[typ]].
      * @throws [[AlreadyBound]] if the type definition is already bound.
      */
    def addType(alias: Id, typ: Type): Environment

    /**
      * Returns the concrete type for a type with TAliases in it.
      */
    def resolveType(typ: Type): Type

    /**
      * Create a new Environment with all the bindings in [[binds]] added to the
      * current scope.
      * @param binds A scope with bindings to be added to the environment.
      * @returns A new environment with all the bindings in the environment.
      */
    def ++(binds: Map[Id, Type]): Environment =
      binds.foldLeft[Environment](this)({ case (e, b) => e.add(b._1, b._2) })

    /**
      * Open a new scope and run commands in it. When the scope ends, new the
      * bindings bound in this scope are returned
      *
      * @param inScope Commands executed with the inner scope.
      * @returns A new environment without the topmost scope and a Scope
      *          containing all bindings in the topmost scope.
      */
    def withScope(inScope: Environment => Environment): Environment

    /**
      * For function return types: if we're in a function, we keep track of
      * it's return type. This is None when we're outside of any function.
      */
    def getReturn: Option[Type]
    def withReturn(typ: Type): Environment


  private case class Env(
      typeMap: ScopedMap[Id, Type] = ScopedMap(),
      typeDefMap: Map[Id, Type] = Map(),
      retType: Option[Type] = None
  ) extends Environment:

    /** Type definitions */
    def addType(alias: Id, typ: Type) = typeDefMap.get(alias) match
      case Some(_) => throw AlreadyBound(alias)
      case None => this.copy(typeDefMap = typeDefMap + (alias -> typ))
    def getType(alias: Id) = typeDefMap.get(alias).getOrThrow(Unbound(alias))

    def resolveType(typ: Type): Type = typ match
      case TAlias(n) => getType(n)
      case TFun(args, ret) => TFun(args.map(resolveType(_)), resolveType(ret))
      case arr @ TArray(t, _, _) => arr.copy(typ = resolveType(t))
      case t => t

    /** Type binding methods */
    def get(id: Id) = typeMap.get(id)
    def add(id: Id, typ: Type) =
      this.copy(typeMap =
        typeMap.add(id, resolveType(typ)).getOrThrow(AlreadyBound(id))
      )

    /** Helper functions for ScopeManager */
    def addScope =
      Env(typeMap.addScope, typeDefMap, retType)
    def endScope =
      val scopes = for
        (_, tMap) <- typeMap.endScope
      yield Env(tMap, typeDefMap, retType)

      scopes.getOrThrow(Impossible("Removed topmost scope"))
    def withScope(inScope: Environment => Environment) =
      inScope(this.addScope) match
        case env: Env => env.endScope

    def getReturn = retType
    def withReturn(typ: Type) = this.copy(retType = Some(typ))
