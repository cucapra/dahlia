package fuselang.typechecker

import scala.util.parsing.input.Position

import fuselang.Utils._
import Info._
import Gadgets._

import fuselang.common._
import ScopeMap._
import Syntax._
import Errors._
import CompilerError._
import EnvHelpers._

object AffineEnv {

  val emptyEnv: Environment = Env()(1)

  /**
   * An environment keep tracks of information for type checking:
   * - The typedefs bound at the start of the program.
   * - The association of Identifiers to corresponding types.
   * - Number of resources nested unrolled contexts imply.
   *
   * The environment structure is built using a chain of scope over the
   * last two associations. A scope is a logical grouping of assoication
   * corresponding to lexical scope in programs.
   */
  sealed trait Environment
    extends ScopeManager[Environment]
    with Tracker[Id, Gadget, Environment] {

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
    def add(gadget: Id, resource: Gadget): Environment

    /**
     * Associate a gadget to the name of the physical resource it consumes.
     * Note that this DOES NOT associate it to the exact list of resources
     * consumed.
     * @param gadget Name of the gadget that consumes the resource.
     *
     */
    def apply(gadget: Id): Gadget

    /**
     * Create a new Environment with all the bindings in [[binds]] added to the
     * current scope.
     * @param binds A scope with bindings to be added to the environment.
     * @returns A new environment with all the bindings in the environment.
     */
    def ++(binds: Map[Id, Gadget]): Environment =
      binds.foldLeft[Environment](this)({ case (e, b) => e.add(b._1, b._2) })

    /**
     * Add physical resources to the environment.
     * @param id Name of the physical resource.
     * @param banks Number of banks in each dimension of the physical resource.
     *
     * For example, addResources("A", List(2, 2)) adds the set of resources
     * {A00, A01, A10, A11} to the environment.
     */
    def addResource(name: Id, info: ArrayInfo): Environment

    /**
     * Consume the physical resources implied by the access of this gadget.
     * Uses the mapping created by addGadget to find the underlying physical
     * resource and consumed the banks associated with it.
     *
     * @param gadget Name of the gadget causing the consume.
     * @param banks Banks to be consumed for each dimension
     */
    def consumeResource(name: Id, resources: List[Int])
                       (implicit pos: Position, trace: List[String]): Environment

    /**
     * Merge this environment with [[that]] to create e' such that for each
     * physical resource id, e'.banks(id) <= this.banks(id) and e'.banks(id) <=
     * that.banks(id).
     * @requires: this.physicalResources == that.physicalResources
     */
    def merge(that: Environment): Environment

    /**
     * Open a new scope and run commands in it. When the scope ends, new the
     * bindings bound in this scope are returned
     *
     * @param inScope Commands executed with the inner scope.
     * @param resources Amount of resources required inside new scope.
     * @returns A new environment without the topmost scope and a Scope
     *          containing all bindings in the topmost scope.
     */
    def withScope(resources: Int)
                 (inScope: Environment => Environment):
                 (Environment, Map[Id, ArrayInfo], Map[Id, Gadget])


    /**
     * @returns The total amount of resources required by the environment
     */
    def getResources: Int

    /** Convenience Methods */
    def consumeWithGadget(gadget: Id, consumeList: ConsumeList)
                         (implicit pos: Position) = {
      val (resName, resources, trace) = this(gadget).getSummary(consumeList)
      implicit val t = trace
      this.consumeResource(resName, resources)
    }

  }

  private case class Env(
    phyRes: ScopedMap[Id, ArrayInfo] = ScopedMap(),
    gadgetMap: ScopedMap[Id, Gadget] = ScopedMap())(implicit val res: Int) extends Environment {

    override def toString = {
      val lst =
        for { (ps, gs) <- phyRes.iterator.zip(gadgetMap.iterator) }
        yield (
          ps.toList.map({case (k, v) => s"$k -> $v"}).mkString(", "),
          gs.keys.mkString("{", ", ", "}")
        )

      lst.mkString(" ==> ")
    }

    /** Tracking bound gadgets */
    def add(id: Id, resource: Gadget) =
      this.copy(gadgetMap = gadgetMap.add(id, resource).getOrThrow(AlreadyBound(id)))
    def apply(id: Id) = gadgetMap.get(id).getOrThrow(Unbound(id))
    def get(id: Id) = gadgetMap.get(id)

    /** Managing physical resources */
    def addResource(id: Id, info: ArrayInfo) = {
      val pRes = phyRes.add(id, info).getOrThrow(AlreadyBound(id))
      this.copy(phyRes = pRes)
    }
    def consumeResource(name: Id, resources: List[Int])
                       (implicit pos: Position, trace: List[String]): Environment = {
      phyRes.get(name) match {
        case None =>
          throw Impossible(s"No physical resource named $name.")
        case Some(info) => {
          this.copy(phyRes = phyRes.update(name, info.consumeResources(resources)))
        }
      }
    }

    /** Helper functions for Mergable[Env] */
    def merge(env: Environment): Environment = env match {
      case that:Env =>
        val (myResources, thatResources) = (this.phyRes.keys, that.phyRes.keys)
        val (myGads, thatGads) = (this.gadgetMap.keys, that.gadgetMap.keys)

        // Sanity check: The same set of ids are bound by both environments
        if (myResources != thatResources) {
          throw Impossible(
            "Trying to merge two environments which bind different sets of ids." +
            s"\nEnv 1: ${myResources}" +
            s"\nEnv 2: ${thatResources}")
        }
        if (myGads != thatGads) {
          throw Impossible(
            "Trying to merge two environments which bind different sets of ids." +
            s"\nEnv 1: ${myGads}" +
            s"\nEnv 2: ${thatGads}")
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
      Env(phyRes.addScope, gadgetMap.addScope)(res * resources)
    }
    def endScope(resources: Int) = {
      val scopes = for {
        (pDefs, pMap) <- phyRes.endScope
        (gDefs, gMap) <- gadgetMap.endScope
      } yield (Env(pMap, gMap)(res / resources), pDefs, gDefs)

      scopes.getOrThrow(Impossible("Removed topmost scope"))
    }
    def withScope(resources: Int)(inScope: Environment => Environment) = {
      inScope(this.addScope(resources)) match {
        case env:Env => env.endScope(resources)
      }
    }

    def withScope(inScope: Environment => Environment): Environment = {
      this.withScope(1)(inScope)._1
    }

    val getResources = res
  }
}

