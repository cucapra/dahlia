package fuselang

import Syntax._
import Syntax.Annotations._
import GadgetEnv._
import Errors._

/**
 * Gadget checking is the first mechanism for enforcing the affine-ness of
 * physical memories. The pass reasons about the physical affine resources
 * (i.e. the underlying RAMs) and how they are consumed by various `gadgets'.
 * A gadget is simply a piece of hardware that consumes some subset of an
 * affine memory.
 *
 * Many gadgets can require the same underlying memory. This pass makes sure
 * that gadgets don't conflict with each other when acquiring resources.
 *
 * //TODO(rachit): More words about capability checking
 */
object GadgetChecker {

  type Environment = GadgetEnvironment

  def check(p: Prog) = {
    val Prog(_, defs, decls, cmd) = p

    defs.collect({ case FuncDef(_, args, bodyOpt) =>
      bodyOpt.map(checkC(_)(addDecls(args, emptyEnv))) })

    checkC(cmd)(addDecls(decls, emptyEnv))
  }

  /**
   * Add all physical resources corresponding to the decl and the default accessor.
   */
  private def addDecls(decls: List[Decl], env: Environment) =
    decls
      .collect({ case Decl(id, _:TArray) => id })
      .foldLeft(env)({ case (env, id) => env.addResource(id).addGadget(id, id)})

  private def checkESeq(exprs: Iterable[Expr])(implicit env: Environment): Environment =
    exprs.foldLeft(env)({ case (env, expr) => checkE(expr)(env) })

  private def checkCSeq(exprs: Iterable[Command])(implicit env: Environment): Environment =
    exprs.foldLeft(env)({ case (env, cmd) => checkC(cmd)(env) })

  private def checkE(expr: Expr)(implicit env: Environment): Environment = expr match {
    case _:EFloat | _:EInt | _:EBool | _:EVar => env
    case ERecLiteral(fields) => checkESeq(fields.map(_._2))
    case EArrLiteral(idxs) => checkESeq(idxs)
    case EBinop(_, e1, e2) => checkESeq(Vector(e1, e2))
    case EApp(_, args) => checkESeq(args)
    case ECast(e, _) => checkE(e)
    case ERecAccess(rec, _) => checkE(rec)
    case acc@EArrAccess(id, idxs) => {
      val (nEnv, consumableAnn, cap) = env.getCap(expr) match {
        case Some(Write) => throw InvalidCap(expr, Read, Write)
        case Some(Read) => (env, SkipConsume, Read)
        case None => {
          checkESeq(idxs)
          // Try to acquire physical resource for the read gadget
          (env.consumeGadget(id), ShouldConsume, Read)
        }
      }

      acc.consumable = Some(consumableAnn)
      nEnv.addCap(expr, cap)
    }
  }

  private def checkLVal(e: Expr)(implicit env: Environment) = e match {
    case acc@EArrAccess(id, idxs) => {
      val (nEnv, consumableAnn, cap) = env.getCap(e) match {
        case Some(Write) => throw AlreadyWrite(e)
        case Some(Read) => throw InvalidCap(e, Write, Read)
        case None => {
          checkESeq(idxs)
          // Try to acquire physical resource for write gadget
          (env.consumeGadget(id), ShouldConsume, Write)
        }
      }

      acc.consumable = Some(consumableAnn)
      nEnv.addCap(e, cap)
    }
    case _ => checkE(e)
  }

  private def checkC(cmd: Command)(implicit env: Environment): Environment = cmd match {
    case CEmpty => env
    case CPar(c1, c2) => checkC(c2)(checkC(c1))
    case CSeq(c1, c2) => checkCSeq(Vector(c1, c2))
    case CUpdate(lhs, rhs) => checkE(rhs)(checkLVal(lhs))
    case CReduce(_, lhs, rhs) => checkE(rhs)(checkLVal(lhs))
    case CLet(id, Some(_:TArray), eOpt) =>
      eOpt.map(checkE(_)).getOrElse(env).addResource(id).addGadget(id, id)
    case CLet(_, _, eOpt) => eOpt.map(checkE).getOrElse(env)
    case CExpr(e) => checkE(e)
    case CIf(cond, c1, c2) => {
      val nEnv = checkE(cond)
      val e1 = nEnv.withScope(checkC(c1)(_))
      val e2 = nEnv.withScope(checkC(c2)(_))
      e1 merge e2
    }
    case CFor(_, par, combine) => {
      val e1 = env.withScope(checkC(par)(_))
      checkC(combine)(e1)
    }
    case CWhile(cond, body) => {
      checkE(cond).withScope(checkC(body)(_))
    }
    case CSplit(id, arrId, _) => env.addGadget(id, arrId)
    case CView(id, arrId, _) => env.addGadget(id, arrId)
  }
}
