package fuselang

import scala.util.parsing.input.Position

import Syntax._
import Errors._
import Info._
import ScopeMap._
import Gadgets._
import CompilerError._

import Utils._

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
     * @throws [[AlreadyBound]] if the type definition is already bound.
     */
    def addType(alias: Id, typ: Type): Environment

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
     * @throws [[Unbound]] If there is no bindings for id
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
     * Associate a gadget to the name of the physical resource it consumes.
     * Note that this DOES NOT associate it to the exact list of resources
     * consumed.
     * @param gadget Name of the gadget that consumes the resource.
     * @param resource The name of the physical resource being consumed.
     *
     * For example, the call addGadget("V_A", "A") associates "V_A" to the
     * physical resource "A".
     */
    def addGadget(gadget: Id, resource: Gadget): Environment

    /**
     * Associate a gadget to the name of the physical resource it consumes.
     * Note that this DOES NOT associate it to the exact list of resources
     * consumed.
     * @param gadget Name of the gadget that consumes the resource.
     *
     */
    def getGadget(gadget: Id): Gadget

    /**
     * Add physical resources to the environment.
     * @param id Name of the physical resource.
     * @param banks Number of banks in each dimension of the physical resource.
     *
     * For example, addResources("A", List(2, 2)) adds the set of resources
     * {A00, A01, A10, A11} to the environment.
     */
    def addResource(name: Id, resources: Iterable[Int]): Environment

    /**
     * Consume the physical resources implied by the access of this gadget.
     * Uses the mapping created by addGadget to find the underlying physical
     * resource and consumed the banks associated with it.
     *
     * @param gadget Name of the gadget causing the consume.
     * @param banks Banks to be consumed for each dimension
     */
    def consumeResource(name: Id, resources: Iterable[Iterable[Int]])
                       (implicit pos: List[Position]): Environment

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
     * physical resource id, e'.banks(id) <= this.banks(id) and e'.banks(id) <=
     * that.banks(id).
     * @requires: this.physicalResources == that.physicalResources
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

    /** Convinience Methods */
    def consumeWithGadget(gadget: Id, consumeList: ConsumeList)
                         (implicit pos: List[Position]) = {
      val (resName, resources) = this.getGadget(gadget).getSummary(consumeList)
      this.consumeResource(resName, resources)
    }

  }

  private case class Env(
    typeMap: ScopedMap[Id, Type] = ScopedMap(),
    typeDefMap: Map[Id, Type] = Map(),
    phyRes: ScopedMap[Id, ArrayInfo] = ScopedMap(),
    gadgetMap: ScopedMap[Id, Gadget] = ScopedMap())(implicit val res: Int) extends Environment {

    /** Type defintions */
    def resolveType(typ: Type): Type = typ match {
      case TAlias(n) => getType(n)
      case TFun(args) => TFun(args.map(resolveType(_)))
      case arr@TArray(t, _) => arr.copy(typ = resolveType(t))
      case t => t
    }
    def addType(alias: Id, typ: Type) = typeDefMap.get(alias) match {
      case Some(_) => throw AlreadyBound(alias)
      case None => this.copy(typeDefMap = typeDefMap + (alias -> typ))
    }
    def getType(alias: Id) = typeDefMap.get(alias).getOrThrow(Unbound(alias))

    /** Type binding methods */
    def apply(id: Id) = typeMap.get(id).getOrThrow(Unbound(id))
    def add(id: Id, typ: Type) =
      this.copy(typeMap =
        typeMap.add(id, resolveType(typ)).getOrThrow(AlreadyBound(id)))

    /** Tracking bound gadgets */
    def addGadget(id: Id, resource: Gadget) =
      this.copy(gadgetMap = gadgetMap.add(id, resource).getOrThrow(AlreadyBound(id)))
    def getGadget(id: Id) = gadgetMap.get(id).getOrThrow(Unbound(id))

    /** Managing physical resources */
    def addResource(id: Id, banks: Iterable[Int]) = {
      val pRes = phyRes.add(id, ArrayInfo(id, banks)).getOrThrow(AlreadyBound(id))
      this.copy(phyRes = pRes)
    }
    def consumeResource(name: Id, resources: Iterable[Iterable[Int]])
                       (implicit pos: List[Position]): Environment = {
      phyRes.get(name) match {
        case None =>
          throw Impossible(s"No physical resource named $name.")
        case Some(info) =>
          this.copy(phyRes = phyRes.update(name, info.consumeResources(resources)))
      }
    }


    /** Helper functions for Mergable[Env] */
    def merge(env: Environment): Environment = env match {
      case that:Env =>
        val (myResources, thatResources) = (this.phyRes.keys, that.phyRes.keys)

        // Sanity check: The same set of ids are bound by both environments
        if (myResources != thatResources) {
          throw Impossible(
            "Trying to merge two environments which bind different sets of ids." +
            s" Intersection of bind set: ${myResources & thatResources}")
        }

        /**
         * For each bound id, set consumed banks to the union of consumed bank
         * sets from both environments.
         */
        myResources.foldLeft[Env](that)({ case (env, id) =>
          env.copy(phyRes = env.phyRes.update(id, env.phyRes(id) merge this.phyRes(id)))
        })
    }

    /** Helper functions for ScopeManager */
    def addScope(resources: Int) = {
      Env(typeMap.addScope, typeDefMap, phyRes.addScope, gadgetMap.addScope)(res * resources)
    }
    def endScope(resources: Int) = {
      val scopes = for {
        (tScope, tMap) <- typeMap.endScope
        (_, pMap) <- phyRes.endScope
        (_, gMap) <- gadgetMap.endScope
      } yield (Env(tMap, typeDefMap, pMap, gMap)(res / resources), tScope)

      scopes.getOrThrow(Impossible("Removed topmost scope"))
    }
    def withScope(resources: Int)(inScope: Environment => Environment) = {
      inScope(this.addScope(resources)) match {
        case env:Env => env.endScope(resources)
      }
    }

    val getResources = res
  }
}

