package fuselang.typechecker

import scala.{PartialFunction => PF}

import Gadgets._
import Info._

import fuselang._
import Utils._

import fuselang.common._
import Syntax._
import Errors._
import Checker._
import CompilerError._
import Logger.PositionalLoggable

object AffineChecker {

  private final case object AffineChecker extends PartialChecker {

    type Env = AffineEnv.Environment

    val emptyEnv = AffineEnv.emptyEnv

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
    private def getConsumeList(idxs: List[Expr], dims: List[DimSpec])
                              (implicit arrId: Id) = {

      val (bres, consume) = idxs.zipWithIndex.foldLeft((1, IndexedSeq[Seq[Int]]()))({
        case ((bres, consume), (idx, dim)) => idx.typ.get match {
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
          case _:TSizedInt =>
            if (dims(dim)._2 != 1) throw InvalidDynamicIndex(arrId, dims(dim)._2)
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
    private def checkView(view: View, arrDim: DimSpec): (Int, DimSpec) = {

      val View(suf, pre, shrink) = view
      val (len, bank) = arrDim

      // Shrinking factor must be a factor of banking for the dimension
      if (shrink.isDefined && (shrink.get > bank || bank % shrink.get != 0)) {
        throw InvalidShrinkWidth(view.pos, bank, shrink.get)
      }

      val newBank = shrink.getOrElse(bank)

      (newBank, (pre.getOrElse(len) -> newBank))
    }

    override def checkLVal(e: Expr)(implicit env: Env) = e match {
      case acc@EArrAccess(id, idxs) => {
        // This only triggers for l-values.
        val TArray(typ, dims, _) = id.typ.get
        acc.consumable match {
          case Some(Annotations.ShouldConsume) => {
            val (bres, consumeList) = getConsumeList(idxs, dims)(id)
            // Check if the accessors generated enough copies for the context.
            if (bres != env.getResources)
              throw InsufficientResourcesInUnrollContext(env.getResources, bres, e)
            // Consume the resources required by this gadget.
          env.consumeWithGadget(id, consumeList)(acc.pos)
          }
          case con => throw Impossible(s"$acc in write position has $con annotation")
        }
      }
      case _ => checkE(e)
    }

    override def myCheckC: PF[(Command, Env), Env] = {
      case (CLet(id, _, _), env) => {
        val ta@TArray(_, _, _) = id.typ.get
        addPhysicalResource(id, ta, env)
      }
      case (CReduce(_, _, rhs), env) => {
        val EVar(gadget) = rhs
        val TArray(_, dims, _) = rhs.typ.get
        val consumeList = dims.map(dim => 0.until(dim._2))
        env.consumeWithGadget(gadget, consumeList)(rhs.pos)
      }
      case (CSeq(c1, c2), env) => {
        val (env1, pDefs, gDefs) = env.withScope(1) { newScope =>
          checkC(c1)(newScope)
        }
        // Create a new environment that has the same gadget definations and
        // physical resources as env1 but hasn't consumed anything.
        pDefs.foldLeft[Env](env ++ gDefs)({ case (e, b) =>
          e.addResource(b._1, b._2)
        })

        val env2 = checkC(c2)(env ++ gDefs)
        env2 merge env1
      }
      case (CPar(c1, c2), env) => checkC(c2)(checkC(c1)(env))
      case (_:CFor, env) => {
        // Increase resources and handle combine
        ???
      }
      case (_:CView, env) => {
        // Add gadget for the view and add missing well formedness checks
        // from new type checker
        ???
      }
      case (_:CSplit, env) => {
        // Same as above
        ???
      }
    }

    override def myCheckE: PF[(Expr, Env), Env] = {
      case (EApp(f, args), env) => {
        args.foldLeft(env)({ case (e, argExpr) => {
          // If an array id is used as a parameter, consume it completely.
          // This works correctly with capabilities.
          (argExpr.typ.get, argExpr) match {
            case (ta:TArray, EVar(gadget)) => {
              val consumeList = ta.dims.map(dim => 0.until(dim._2))
              e.consumeWithGadget(gadget, consumeList)(argExpr.pos)
            }
            case (_:TArray, expr) => {
              throw Impossible(s"Type of $expr is ${argExpr.typ.get}")
            }
            case _ => e
          }
        }})
      }
      case (expr@EArrAccess(id, idxs), env) => {
        val TArray(_, dims, _) = id.typ.get
        expr.consumable match {
          case None => throw Impossible(s"$expr in read position has no consumable annotation")
          case Some(Annotations.SkipConsume) => env
          case Some(Annotations.ShouldConsume) => {
            val (_, consumeList) = getConsumeList(idxs, dims)(id)
            // Consume the resources required by this gadget.
            env.consumeWithGadget(id, consumeList)(expr.pos)
          }
        }
      }
    }
  }
}
