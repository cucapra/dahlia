package fuselang

import Syntax._
import ScopeMap._
import CompilerError._
import Errors._
import Utils._

object GadgetEnv {

  val emptyEnv: GadgetEnvironment = GadgetEnv()

  sealed trait GadgetEnvironment {

    /**
     * Add a physical resource that can be consumed in the current scope.
     * @param resource A physical affine resource that can be consumed by a
     *                 gadget.
     * @throws [[Errors.AlreadyBound]] If the resource name is already bound in this scope.
     */
    def addResource(resource: Id): GadgetEnvironment

    /** Consume the physical resource associated with the gadget.
     *  @param gadget The gadget which is consuming the resource.
     *  @returns Returns a new environment where the resource has been consumed.
     *  @throws [[AlreadyConsumed]] if the physical resource has already been consumed.
     */
    def consumeGadget(gadget: Id): GadgetEnvironment

    /**
     * Associate a new gadget to a physical resource.
     * @param gadget The gadget.
     * @param cap The physical resource associated to the gadget.
     * @return A new environment where the gadget is associated to the gadget.
     * @throws [[UnknownResource]] if the resource name is not bound.
     */
    def addGadget(gadget: Id, resource: Id): GadgetEnvironment

    def withScope(inScope: GadgetEnvironment => GadgetEnvironment): GadgetEnvironment

    def merge(that: GadgetEnvironment): GadgetEnvironment

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
    def addCap(e: Expr, cap: Capability): GadgetEnvironment
  }

  /**
   * [[gadgetMap]] stores the mapping from gadget name to the corresponding physical
   * resource.
   *
   * [[resources]] is a mapping of all known physical resources. Mapping for a
   * resource is [[true]] if and only if it has not been consumed.
   */
  private case class GadgetEnv (
    gadgetMap: ScopedMap[Id, Id] = ScopedMap(),
    resources: ScopedMap[Id, Boolean] = ScopedMap(),
    capMap: ScopedMap[Expr, Capability] = ScopedMap()
  ) extends GadgetEnvironment {

    def consumeGadget(gadget: Id): GadgetEnv = {
      val res = gadgetMap(gadget).getOrThrow(UnboundVar(gadget))
      val notConsumed = resources(res)
        .getOrThrow(Impossible("gadget was mapped to unknown resource"))

      assert(notConsumed, Impossible("Physical resource was already consumed"))
      this.copy(resources = resources.update(res, false).get)
    }

    def addGadget(gadget: Id, resource: Id) = gadgetMap.add(gadget, resource) match {
      case Some(gMap) => this.copy(gadgetMap = gMap)
      case None => throw AlreadyBound(gadget)
    }

    def addResource(resource: Id) = resources.add(resource, true) match {
      case Some(res) => this.copy(resources = res)
      case None => throw AlreadyBound(resource)
    }

    private def endScope = {
      val scopes = for {
        (_, gMap) <- gadgetMap.endScope;
        (_, res) <- resources.endScope
        (_, cMap) <- capMap.endScope
      } yield GadgetEnv(gMap, res, cMap)

      scopes.getOrThrow(Impossible("Removed topmost scope"))
    }

    def withScope(inScope: GadgetEnvironment => GadgetEnvironment) = {
      val newScope = GadgetEnv(gadgetMap.addScope, resources.addScope, capMap.addScope)
      inScope(newScope) match {
        case env: GadgetEnv => env.endScope
      }
    }

    def merge(that: GadgetEnvironment) = that match {
      case other:GadgetEnv => {
        assertOrThrow(this.resources.keys == other.resources.keys,
          Impossible("Merging gadget environments with different resources." +
                     s"Intersection of resource ${this.resources.keys & other.resources.keys}"))

        val mergeRes = resources.keys.foldLeft(other.resources)({ case (res, id) =>
          res.update(id, res(id).get && resources(id).get).get
        })

        this.copy(resources = mergeRes)
      }
    }

    def getCap(expr: Expr): Option[Capability] = capMap(expr)
    def addCap(expr: Expr, cap: Capability) = capMap.add(expr, cap) match {
      case Some(cMap) => this.copy(capMap = cMap)
      case None => throw Impossible(s"Capability for $expr already exists.")
    }
  }
}

