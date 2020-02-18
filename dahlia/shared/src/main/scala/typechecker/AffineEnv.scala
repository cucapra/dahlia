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

  private implicit val ctxName = "Affine Check"

  /**
   * An Affine Env tracks:
   * - The physical resources bound in this context.
   * - Gadget definitions bound in this context
   * - Number of parallel resources required by this context.
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
     * Get the gadget associated with identifier.
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
     * @param banks Number of banks available to this resource. All resources
     * are single dimensional. Multi-dimensional banks are implemented using
     * [[Gadget.MultiDimGadget]]
     */
    def addResource(name: Id, info: ArrayInfo): Environment

    /**
     * Merge this environment with [[next]] to create e' such that for each
     * physical resource id, e'.banks(id) <= this.banks(id) and e'.banks(id) <=
     * that.banks(id).
     * @requires: this.physicalResources subsetOf next.physicalResources
     * @requires: this.gadgetDefs subsetOf next.gadgetDefs
     */
    def merge(next: Environment): Environment

    /**
     * Open a new scope and run commands in it. When the scope ends, new the
     * bindings bound in this scope are returned
     *
     * @param inScope Commands executed with the inner scope.
     * @param resources Amount of resources required inside new scope.
     * @returns A new environment without the topmost scope and scopes
     *          containing bindings for physical resource and gadgets.
     */
    def withScope(resources: Int)
                 (inScope: Environment => Environment):
                 (Environment, Map[Id, ArrayInfo], Map[Id, Gadget])


    /**
     * @returns The total amount of resources required by the environment
     */
    def getResources: Int

    /**
     * Consume resources through [[gadget]]. The given [[consumeList]] is
     * transformed by the gadget into a resource requirement for the underlying
     * physical resource.
     */
    def consumeWithGadget(gadget: Id, consumeList: ConsumeList)
                         (implicit pos: Position): Environment

  }

  private case class Env(
    phyRes: ScopedMap[Id, ArrayInfo] = ScopedMap(),
    gadgetMap: ScopedMap[Id, Gadget] = ScopedMap())(implicit val res: Int) extends Environment {

    def consumeWithGadget(gadget: Id, consumeList: ConsumeList)
                         (implicit pos: Position) = {
      val (resName, resources, trace) = this(gadget).getSummary(consumeList)
      implicit val t = trace
      this.consumeResource(resName, resources)
    }

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
      case next:Env =>
        val (oldRes, nextRes) = (this.phyRes.keys, next.phyRes.keys)
        val (oldGads, nextGads) = (this.gadgetMap.keys, next.gadgetMap.keys)

        // The next environment should bind all resources in this env.
        if (oldRes.subsetOf(nextRes) == false) {
          throw Impossible(
            "New environment is missing resources bound in old env." +
            s"\n\nOld Env: ${oldRes}" +
            s"\n\nNew Env: ${nextRes}" +
            s"\n\nMissing: ${oldRes diff nextRes}")
        }
        if (oldRes.subsetOf(nextRes) == false) {
          throw Impossible(
            "New environment is missing gadgets bound in old env." +
            s"\n\nOld Env: ${oldGads}" +
            s"\n\nNew Env: ${nextGads}"+
            s"\n\nMissing: ${oldGads diff nextGads}.\n")
        }

        /**
         * For each bound id, set consumed banks to the union of consumed bank
         * sets from both environments.
         */
        oldRes.foldLeft[Env](next)({ case (env, id) =>
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

