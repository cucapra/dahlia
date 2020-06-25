package fuselang.typechecker

import scala.{PartialFunction => PF}

import Gadgets._
import Info._

import fuselang.common._
import Syntax._
import Errors._
import Checker._
import CompilerError._

/*
 * Apart from normal typechecking, such
 * as ensuring condition in `if` is a boolean, it does following.
 * - Affine properties of banks in memories.
 * - Checking combine blocks correctly use bound variables.
 * - Generate subtype joins for binary operators.
 * - Checks read/write capabilites for array accesses
 *
 * It also MUTATES and ANNOTATES the input AST:
 * - CLet with an explicit type get the correct type
 * - Binders for id update the `typ` var defined on the Id class.
 *
 * Type checking array accesses enforces the various affine restrictions. A
 * lot of machinery works together to implement gadget checking.
 *
 * We make a distinction between physical memories ("memories") and
 * the hardware that accesses them ("gadgets"). In short, the type checker
 * creates affine resources corresponding to memories and makes sure that
 * different gadgets with the same underlying memory don't try to consume
 * more resources than a memory provides.
 *
 * '''Memory Instantiation'''
 *
 * The type checker starts tracking memory resource after they are created.
 * There are two ways of creating memories:
 *
 * 1. `decl`s with array types.
 * 2. `let` bindings with array types.
 *
 * When the type checker encounters these, it adds two things to the context:
 *
 * 1. The physical affine resources implied by the type. For example, the
 *    type bit<32>[10 bank 5][4 bank 4] implies that the memory has 5 affine
 *    resources in the first dimension and 4 in the second.
 * 2. A default gadget for this memory ([[Gadgets.ResourceGadget]]). All possible
 *    compositions of gadgets have a [[Gadgets.ResourceGadget]] at their root.
 *
 * '''Gadget Creation'''
 *
 * Gadgets are created in two places:
 *
 * 1. Default ([[Gadgets.ResourceGadget]]) when a memory definition is reached.
 * 2. View ([[Gadgets.ViewGadget]]) when a view is created. A view gadget is
 *    built on top of another gadget itself which might come from a memory
 *    or another gadget itself. This creates a hierarchy of gadgets.
 *
 * '''Access checking'''
 *
 * Access checking is done in four steps:
 *
 * 1. If the memory was marked with [[Syntax.Annotations.SkipConsume]],
 *    we do the well formedness check and move on.
 * 2. Otherwise, we generate resource consumptions implied by the indices for
 *    each dimension of the array.
 * 3. We pass on this resource consumption list to the gadget for the access
 *    which transforms the resources into one for the underlying resource.
 * 4. Finally, we consume the resources required by the transformed consumption
 *    list.
 *
 */

object AffineChecker {

  def check(p: Prog) = AffineChecker.check(p)

  private final case object AffineChecker extends PartialChecker {

    type Env = AffineEnv.Environment

    val emptyEnv = AffineEnv.emptyEnv

    override def check(p: Prog): Unit = {
      val Prog(_, defs, _, decls, cmd) = p

      val topFunc = FuncDef(Id(""), decls, TVoid(), Some(cmd))
      (defs ++ List(topFunc)).foldLeft(emptyEnv) {
        case (e, d) => checkDef(d)(e)
      }
      ()
    }

    override def checkDef(defi: Definition)(implicit env: Env) = defi match {
      case FuncDef(_, args, _, bodyOpt) => {
        val (env2, _, _) = env.withScope(1) { newScope =>
          // Add physical resources corresponding to array decls
          val envWithResources = args
            .collect({ case Decl(id, t: TArray) => id -> t })
            .foldLeft(newScope)({
              case (env, (id, t)) =>
                addPhysicalResource(id, t, env)
            })

          bodyOpt
            .map(body => checkC(body)(envWithResources))
            .getOrElse(envWithResources)
        }
        env2
      }
      case _: RecordDef => env
    }

    /**
      * Add physical resources and default accessor gadget corresponding to a new
      * array. This is used for `decl` with arrays and new `let` bound arrays.
      */
    private def addPhysicalResource(id: Id, typ: TArray, env: Env) = {
      val banks = typ.dims.map(_._2)
      env
        .addResource(id, ArrayInfo(id, banks, typ.ports))
        .add(id, MultiDimGadget(ResourceGadget(id, banks), typ.dims))
    }

    /**
      * Generate a ConsumeList corresponding to the underlying memory type and
      * the index accessors.
      */
    private def getConsumeList(idxs: List[Expr], dims: List[DimSpec])(
        implicit arrId: Id
    ) = {

      val (bres, consume) = idxs.zipWithIndex.foldLeft(
        (1, IndexedSeq[Seq[Int]]())
      )({
        case ((bres, consume), (idx, dim)) =>
          idx.typ.get match {
            // Index is an index type.
            case TIndex((s, e), _) =>
              if ((e - s) % dims(dim)._2 != 0)
                throw BankUnrollInvalid(arrId, dims(dim)._2, e - s)(idx.pos)
              else
                (bres * (e - s), Range(s, e) +: consume)
            // Index is a statically known number.
            case TStaticInt(v) =>
              (bres * 1, Vector(v % dims(dim)._2) +: consume)
            // Index is a dynamic number.
            case _: TSizedInt =>
              if (dims(dim)._2 != 1)
                throw InvalidDynamicIndex(arrId, dims(dim)._2)
              else (bres * 1, Vector(0) +: consume)

            case t =>
              throw UnexpectedType(idx.pos, "array indexing", "integer type", t)
          }
      })

      // Reverse the types list to match the order with idxs.
      (bres, consume.reverse)
    }

    /**
      * Checks a given simple view and returns the dimensions for the view,
      * shrink factors, and an updated environment.
      */
    /*private def checkView(view: View, arrDim: DimSpec): (Int, DimSpec) = {

      val View(suf, pre, shrink) = view
      val (len, bank) = arrDim

      // Shrinking factor must be a factor of banking for the dimension
      if (shrink.isDefined && (shrink.get > bank || bank % shrink.get != 0)) {
        throw InvalidShrinkWidth(view.pos, bank, shrink.get)
      }

      val newBank = shrink.getOrElse(bank)

      (newBank, (pre.getOrElse(len) -> newBank))
    }*/

    override def checkLVal(e: Expr)(implicit env: Env) = e match {
      case acc @ EArrAccess(id, idxs) => {
        // This only triggers for l-values.
        val TArray(_, dims, _) = id.typ.get
        acc.consumable match {
          case Some(Annotations.ShouldConsume) => {
            val (bres, consumeList) = getConsumeList(idxs, dims)(id)
            // Check if the accessors generated enough copies for the context.
            if (bres != env.getResources)
              throw InsufficientResourcesInUnrollContext(
                env.getResources,
                bres,
                e
              )
            // Consume the resources required by this gadget.
            env.consumeWithGadget(id, consumeList)(acc.pos)
          }
          case con =>
            throw Impossible(s"$acc in write position has $con annotation")
        }
      }
      case _ => checkE(e)
    }

    override def myCheckC: PF[(Command, Env), Env] = {
      case (CLet(id, Some(ta @ TArray(_, _, _)), _), env) => {
        addPhysicalResource(id, ta, env)
      }
      case (CSeq(c1, c2), env) => {
        // Abuse withScope to capture bindings created in this scope.
        val (nEnv, pDefs, gDefs) = env.withScope(1) { newScope =>
          checkC(c1)(newScope)
        }
        // Recreate the resource usage patterns in this scope. Note that
        // any physical resource affected from lower parts of the scope
        // chain don't need to be changed.
        val env1 = pDefs.toList.foldLeft[Env](nEnv ++ gDefs)({
          case (e, (id, resource)) => e.addResource(id, resource)
        })

        // Create a new environment that has the same gadget definations and
        // physical resources as env1 but hasn't consumed anything.
        val nextEnv = pDefs.toList.foldLeft[Env](env ++ gDefs)({
          case (e, (id, resource)) => e.addResource(id, resource.toFresh)
        })

        val env2 = checkC(c2)(nextEnv)
        env1 merge env2
      }
      case (CPar(c1, c2), env) => checkC(c2)(checkC(c1)(env))
      case (CFor(range, _, par, combine), env) => {
        val (e1, _, _) = env.withScope(range.u) { newScope =>
          checkC(par)(newScope)
        }

        val (e2, _, _) = env.withScope(1) { newScope =>
          checkC(combine)(newScope)
        }

        e1 merge e2
      }
      case (CView(id, arrId, _), env) => {
        // Add gadget for the view and add missing well formedness checks
        // from new type checker
        val TArray(_, adims, _) = arrId.typ.get
        val TArray(_, vdims, _) = id.typ.get
        val shrinks = vdims.map(_._2)
        env.add(id, viewGadget(env(arrId), shrinks, adims))
      }
      case (CSplit(id, arrId, _), env) => {
        val TArray(_, adims, _) = arrId.typ.get
        val TArray(_, vdims, _) = id.typ.get
        env.add(id, splitGadget(env(arrId), adims, vdims))
      }
    }

    override def myCheckE: PF[(Expr, Env), Env] = {
      case (EApp(_, args), env) => {
        args.foldLeft(env)({
          case (e, argExpr) => {
            // If an array id is used as a parameter, consume it completely.
            // This works correctly with capabilities.
            (argExpr.typ.get, argExpr) match {
              case (ta: TArray, EVar(gadget)) => {
                val consumeList = ta.dims.map(dim => 0.until(dim._2))
                e.consumeWithGadget(gadget, consumeList)(argExpr.pos)
              }
              case (_: TArray, expr) => {
                throw Impossible(s"Type of $expr is ${argExpr.typ.get}")
              }
              case _ => e
            }
          }
        })
      }
      case (expr @ EArrAccess(id, idxs), env) => {
        val TArray(_, dims, _) = id.typ.get
        expr.consumable match {
          case None =>
            throw Impossible(
              s"$expr in read position has no consumable annotation"
            )
          case Some(Annotations.SkipConsume) => env
          case Some(Annotations.ShouldConsume) => {
            val (_, consumeList) = getConsumeList(idxs, dims)(id)
            // Consume the resources required by this gadget.
            env.consumeWithGadget(id, consumeList)(expr.pos)
          }
        }
      }
      case (_: EPhysAccess, _) => {
        throw NotImplemented("Affine checking for physical accesses.")
      }
    }
  }
}
