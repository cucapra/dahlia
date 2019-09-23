package fuselang.typechecker

import Subtyping._
import TypeEnv._
import Gadgets._
import fuselang.Utils._

import fuselang.common._
import Syntax._
import Errors._
import CompilerError._
import Logger.PositionalLoggable

/**
 * Type checker implementation for Fuse. Apart from normal typechecking, such as
 * ensuring condition in `if` is a boolean, it does following.
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
 * 2. A default gadget for this memory ([[Gadgets.BaseGadget]]). All possible
 *    compositions of gadgets have a [[Gadgets.BaseGadget]] at their root.
 *
 * '''Gadget Creation'''
 *
 * Gadgets are created in two places:
 *
 * 1. Default ([[Gadgets.BaseGadget]]) when a memory definition is reached.
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
object TypeChecker {

  /* A program consists of a list of function or type definitions, a list of
   * variable declarations and then a command. We build up an environment with
   * all the declarations and definitions, then check the command in that environment
   * (`checkC`).
   */
  def typeCheck(p: Prog) = {
    val Prog(includes, defs, _, decls, cmd) = p

    val allDefs = includes.flatMap(_.defs) ++ defs
    val topFunc = FuncDef(Id(""), decls, TVoid(), Some(cmd))

    (allDefs ++ List(topFunc)).foldLeft(emptyEnv) {
      case (e, d) => checkDef(d, e)
    }
  }

  private def checkDef(defi: Definition, env: Environment) = defi match {
    case FuncDef(id, args, ret, bodyOpt) => {
      val (env2, _) = env.withScope(1) { newScope =>

        // Bind all declarations to the body.
        val envWithArgs = args.foldLeft(newScope)({ case (env, Decl(id, typ)) =>
          val rTyp = env.resolveType(typ)
          id.typ = Some(rTyp);
          env.add(id, rTyp)
        })

        // Add physical resources corresponding to array decls
        val envWithResources = args
          .collect({ case Decl(id, t:TArray) => id -> t})
          .foldLeft(envWithArgs)({ case (env, (id, t)) => addPhysicalResource(id, t)(env)})

        // Add the return type
        val envWithRet = envWithResources.withReturn(ret)

        bodyOpt
          .map(body => checkC(body)(envWithRet))
          .getOrElse(envWithResources)
      }
      env2.add(id, TFun(args.map(_.typ), ret))
    }
    case RecordDef(name, fields) => {
      val rFields = fields.map({case (k, t) => k -> env.resolveType(t)})
      env.addType(name, TRecType(name, rFields))
    }
  }

  /**
   * Add physical resources and default accessor gadget corresponding to a new
   * array. This is used for `decl` with arrays and new `let` bound arrays.
   */
  private def addPhysicalResource(id: Id, typ: TArray)
                                 (implicit env: Environment) = {
    env
      .addResource(id, typ.dims.map(_._2))
      .addGadget(id, BaseGadget(id))
  }

  /**
   * Generate a ConsumeList corresponding to the underlying memory type and
   * the index accessors.
   */
  private def getConsumeList(idxs: List[Expr], dims: List[(Int, Int)])
                            (implicit arrId: Id, env: Environment) = {
    val (nEnv, bres, consume) = idxs.zipWithIndex.foldLeft((env, 1, IndexedSeq[Iterable[Int]]()))({
      case ((env1, bres, consume), (idx, dim)) => checkE(idx)(env1) match {
        // Index is an index type.
        case (TIndex((s, e), _), env2) =>
          if (dims(dim)._2 != e - s)
            throw BankUnrollInvalid(arrId, dims(dim)._2, e - s)(idx.pos)
          else
            (env2, bres * (e - s), Range(s, e) +: consume)
        // Index is a statically known number.
        case (TStaticInt(v), env2) =>
          (env2, bres * 1, Vector(v % dims(dim)._2) +: consume)
        // Index is a dynamic number.
        case (_:TSizedInt, env2) =>
          if (dims(dim)._2 != 1) throw InvalidDynamicIndex(arrId, dims(dim)._2)
          else (env2, bres * 1, Vector(0) +: consume)

        case (t, _) =>
          throw UnexpectedType(idx.pos, "array indexing", "integer type", t)
      }
    })

    // Reverse the types list to match the order with idxs.
    (nEnv, bres, consume.reverse)
  }

  private def checkLVal(e: Expr)(implicit env: Environment) = e match {
    case acc@EArrAccess(id, idxs) => env(id).matchOrError(e.pos, "array access", s"array") {
      // This only triggers for r-values. l-values are checked in checkLVal
      case TArray(typ, dims) => {
        if (dims.length != idxs.length) {
          throw IncorrectAccessDims(id, dims.length, idxs.length)
        }
        // Bind the type of to Id
        id.typ = Some(env(id));
        // Consumption check
        acc.consumable match {
          case Some(Annotations.ShouldConsume) => {
            val (e1, bres, consumeList) = getConsumeList(idxs, dims)(id, env)
            // Check if the accessors generated enough copies for the context.
            if (bres != env.getResources)
              throw InsufficientResourcesInUnrollContext(env.getResources, bres, e)
            // Consume the resources required by this gadget.
            typ -> e1.consumeWithGadget(id, consumeList)(idxs.map(_.pos))
          }
          case con => throw Impossible(s"$acc in write position has $con annotation")
        }
      }
    }
    case _ => checkE(e)
  }

  private def checkB(t1: Type, t2: Type, op: BOp) = op match {
    case _:EqOp => {
      if (t1.isInstanceOf[TArray])
        throw UnexpectedType(op.pos, op.toString, "primitive types", t1)
      else if (joinOf(t1, t2, op).isDefined)
        TBool()
      else
        throw NoJoin(op.pos, op.toString, t1, t2)
    }
    case _:BoolOp => (t1, t2) match {
      case (TBool(), TBool()) => TBool()
      case _ => throw BinopError(op, "booleans", t1, t2)
    }
    case _:CmpOp => (t1, t2) match {
      case (_:IntType, _:IntType) => TBool()
      case (_:TFloat, _:TFloat) => TBool()
      case (_:TDouble, _:TDouble) => TBool()
      case _ => throw BinopError(op, "float, integer, or double", t1, t2)
    }
    case _:NumOp =>
      joinOf(t1, t2, op).getOrThrow(NoJoin(op.pos, op.toString, t1, t2))
    case _:BitOp => (t1, t2) match {
      case (_:TSizedInt, _:IntType) => t1
      case (TStaticInt(v), _:IntType) => TSizedInt(bitsNeeded(v), false)
      case (tidx@TIndex(_, _), _:IntType) =>
        TSizedInt(bitsNeeded(tidx.maxVal), false)
      case _ => throw BinopError(op, "integer type", t1, t2)
    }
  }

  /**
   * Wrapper for checkE that annotates each expression with it's full type.
   * Type checking code is in _checkE.
   */
  private def checkE(e: Expr)
                    (implicit env: Environment): (Type, Environment) = {
    val (typ, nEnv) = _checkE(e)
    if (e.typ.isDefined) {
      throw Impossible(s"$e was type checked multiple times.")
    }
    e.typ = Some(typ)
    typ -> nEnv
  }

  // Implicit parameters can be elided when a recursive call is reusing the
  // same env and its. See EBinop case for an example.
  private def _checkE
    (expr: Expr)
    (implicit env: Environment): (Type, Environment) = expr match {
    case EDouble(v) => TStaticDouble(v) -> env
    case EInt(v, _) => TStaticInt(v) -> env
    case EBool(_) => TBool() -> env
    case ERecLiteral(_) => throw NotInBinder(expr.pos, "Record Literal")
    case EArrLiteral(_) => throw NotInBinder(expr.pos, "Array Literal")
    case ECast(e, castType) => {
      val (typ, nEnv) = checkE(e)
      if (safeCast(typ, castType) == false) {
        scribe.warn {
          (s"Casting $typ to $castType which may lose precision.", expr)
        }
      }
      castType -> nEnv
    }
    case EVar(id) => {
      // Add type information to variable
      id.typ = Some(env(id));
      env(id) -> env
    }
    case EBinop(op, e1, e2) => {
      val (t1, env1) = checkE(e1)
      val (t2, env2) = checkE(e2)(env1)
      checkB(t1, t2, op) -> env2
    }
    case EApp(f, args) => env(f) match {
      case TFun(argTypes, retType) => {
        if (argTypes.length != args.length) {
          throw ArgLengthMismatch(expr.pos, argTypes.length, args.length)
        }

        retType -> args.zip(argTypes).foldLeft(env)({ case (e, (arg, expectedTyp)) => {
          val (typ, e1) = checkE(arg)(e);
          if (isSubtype(typ, expectedTyp) == false) {
            throw UnexpectedSubtype(arg.pos, "parameter", expectedTyp, typ)
          }
          // If an array id is used as a parameter, consume it completely.
          // This works correctly with capabilities.
          (typ, arg) match {
            case (ta:TArray, EVar(gadget)) => {
              val consumeList = ta.dims.map(dim => 0.until(dim._2))
              e1.consumeWithGadget(gadget, consumeList)(ta.dims.map(_ => gadget.pos))
            }
            case (_:TArray, expr) => throw Impossible(s"Type of $expr is $typ")
            case _ => e1
          }
        }})
      }
      case t => throw UnexpectedType(expr.pos, "application", "function", t)
    }
    case ERecAccess(rec, field) => checkE(rec) match {
      case (TRecType(name, fields), env1) => fields.get(field) match {
        case Some(typ) => typ -> env1
        case None => throw UnknownRecordField(expr.pos, name, field)
      }
      case (t, _) => throw UnexpectedType(expr.pos, "record access", "record type", t)
    }
    case acc@EArrAccess(id, idxs) => env(id).matchOrError(expr.pos, "array access", s"array type"){
      // This only triggers for r-values. l-values are checked in checkLVal
      case TArray(typ, dims) => {
        if (dims.length != idxs.length) {
          throw IncorrectAccessDims(id, dims.length, idxs.length)
        }
        // Bind the type of to Id
        id.typ = Some(env(id));
        // Consumption check
        acc.consumable match {
          case None => throw Impossible(s"$acc in read position has no consumable annotation")
          case Some(Annotations.SkipConsume) => typ -> env
          case Some(Annotations.ShouldConsume) => {
            val (e1, _, consumeList) = getConsumeList(idxs, dims)(id, env)
            // Consume the resources required by this gadget.
            typ -> e1.consumeWithGadget(id, consumeList)(idxs.map(_.pos))
          }
        }
      }
    }
  }

  /**
   * Checks a given simple view and returns the dimensions for the view along
   * with an updated environment.
   */
  private def checkView(view: View, arrDim: (Int, Int))
                       (implicit env: Environment): (Environment, (Int, Int)) = {

    val View(suf, pre, shrink) = view
    val (len, bank) = arrDim

    // Shrinking factor must be a factor of banking for the dimension
    if (shrink.isDefined && (shrink.get > bank || bank % shrink.get != 0)) {
      throw InvalidShrinkWidth(view.pos, bank, shrink.get)
    }

    val newBank = shrink.getOrElse(bank)

    val idx = suf match {
      case Aligned(fac, idx) => if (newBank > fac || fac % newBank != 0) {
        throw InvalidAlignFactor(suf.pos, fac, newBank)
      } else {
        idx
      }
      case Rotation(idx) => idx
    }

    // Check the index is non-index int type
    val (typ, nEnv) = checkE(idx)(env)
    typ.matchOrError(idx.pos, "view", "integer type") {
      case _:IntType => () // IntTypes are valid
    }

    nEnv -> (pre.getOrElse(len) -> newBank)
  }

  private def checkPipeline(enabled: Boolean, loop: Command, body: Command) = {
    // Only loops without sequencing may be pipelined.
    body match {
      case _: CSeq => if (enabled) {
        throw PipelineError(loop.pos)
      }
      case _ => {}
    }
  }

  private def checkC(cmd: Command)
                    (implicit env:Environment): Environment = cmd match {
    case CPar(c1, c2) => checkC(c2)(checkC(c1))
    case CIf(cond, cons, alt) => {
      val (cTyp, e1) = checkE(cond)(env)
      cTyp.matchOrError(cond.pos, "if condition", "bool"){ case _:TBool => () }
      val (e2, _) = e1.withScope(1)(e => checkC(cons)(e))
      val (e3, _) = e1.withScope(1)(e => checkC(alt)(e))
      e2 merge e3
    }
    case CWhile(cond, pipeline, body) => {
      checkPipeline(pipeline, cmd, body)
      val (cTyp, e1) = checkE(cond)(env)
      if (cTyp != TBool()) {
        throw UnexpectedType(cond.pos, "while condition", TBool().toString, cTyp)
      }
      e1.withScope(1)(e => checkC(body)(e))._1
    }
    case CUpdate(lhs, rhs) => {
      val (t1, e1) = checkLVal(lhs)
      val (t2, e2) = checkE(rhs)(e1)
      if (isSubtype(t2, t1)) e2
      else throw UnexpectedSubtype(rhs.pos, "assignment", t1, t2)
    }
    case CReduce(rop, l, r) => {
      val (t1, e1) = checkLVal(l)
      val (ta, e2) = checkE(r)(e1)
      (t1, ta) match {
        case (t1, TArray(t2, dims)) =>
          if (isSubtype(t2, t1) == false ||
              dims.length != 1 ||
              dims(0)._1 != dims(0)._2) {
            throw UnexpectedType(r.pos, s"reduction operator $rop", "fully banked array", ta)
          } else {
            e2
          }
        case _ =>
          throw UnexpectedType(r.pos, s"reduction operator $rop", "fully banked array", ta)
      }
    }
    case l@CLet(id, typ, Some(EArrLiteral(idxs))) => {
      val expTyp = typ.getOrThrow(ExplicitTypeMissing(l.pos, "Array literal", id))

      env
        .resolveType(expTyp)
        .matchOrError(l.pos, "Let bound array literal", "array type") {
          case ta@TArray(elemTyp, dims) => {
            assertOrThrow(dims.length == 1,
              Unsupported(l.pos, "Multidimensional array literals"))

            assertOrThrow(dims(0)._1 == idxs.length,
              LiteralLengthMismatch(l.pos, dims(0)._1, idxs.length))

            val nEnv = idxs.foldLeft(env)({ case (e, idx) =>
              val (idxTyp, nEnv) = checkE(idx)(e)
              assertOrThrow(isSubtype(idxTyp, elemTyp),
                UnexpectedSubtype(idx.pos, "array literal", elemTyp, idxTyp))
              nEnv
            })

            id.typ = typ

            // Add the type binding, physical resource, and the accessor.
            addPhysicalResource(id, ta)(nEnv).add(id, expTyp)
          }
        }
    }
    case l@CLet(id, typ, Some(exp@ERecLiteral(fs))) => {
      val expTyp = typ.getOrThrow(ExplicitTypeMissing(l.pos, "Record literal", id))
      env.resolveType(expTyp) match {
        case recTyp@TRecType(name, expTypes) => {
          // Typecheck expressions in the literal and generate a new id to type map.
          val (env1, actualTypes) = fs.foldLeft((env, Map[Id, Type]()))({
            case ((env, map), (id, exp)) => {
              val (t, e1) = checkE(exp)(env)
              (e1, map + (id -> t))
            }
          })

          // Check all fields have the expected type and the literal has all
          // required fields.
          expTypes.keys.foreach(field => {
            val (eTyp, acTyp) = (expTypes(field), actualTypes.get(field))
            if (acTyp.isDefined == false) {
              throw MissingField(exp.pos, name, field)
            }
            if (isSubtype(acTyp.get, eTyp) == false) {
              throw UnexpectedType(
                fs(field).pos,
                "record literal",
                expTypes(field).toString,
                acTyp.get)
            }
          })

          // Check there are no extra fields in the literal
          val extraFields = (actualTypes.keys.toSet diff expTypes.keys.toSet)
          extraFields.foreach(field => throw ExtraField(field.pos, name, field))

          env1.add(id, recTyp)
        }
        case t => throw UnexpectedType(exp.pos, "let", "record type", t)
      }
    }
    case l@CLet(id, typ, Some(exp)) => {
      // Check if the explicit type is bound in scope. Also, if the type is
      // a static int, upcast it to sized int. We do not allow variables to
      // have static types.
      val rTyp = typ.map(env.resolveType(_) match {
        case TStaticInt(v) => TSizedInt(bitsNeeded(v), false)
        case t => t
      })
      val (t, e1) = checkE(exp)
      rTyp match {
        case Some(t2) => {
          if (isSubtype(t, t2))
            e1.add(id, t2)
          else
            throw UnexpectedSubtype(exp.pos, "let", t2, t)
        }
        case None => {
          val typ = t match {
            case TStaticInt(v) => TSizedInt(bitsNeeded(v), false)
            case t => t
          }
          // Add inferred type to the AST Node.
          l.typ = Some(typ); e1.add(id, typ)
        }
      }
    }
    case l@CLet(id, typ, None) => {
      val fullTyp = typ
        .map(env.resolveType(_))
        .getOrThrow(ExplicitTypeMissing(l.pos,
                                        "Let binding without initializer",
                                        id))

      // If this is an array literal, bind the physical resource too.
      val nEnv = fullTyp match {
        case ta: TArray => addPhysicalResource(id, ta)(env)
        case _ => env
      }

      nEnv.add(id, fullTyp)
    }
    case CFor(range, pipeline, par, combine) => {
      checkPipeline(pipeline, cmd, par)

      val iter = range.iter
      val (e1, binds) = env.withScope(range.u) { newScope =>
        // Add binding for iterator in a separate scope.
        val e2 = newScope.add(iter, range.idxType)
        checkC(par)(e2)
      }
      // Create scope where ids bound in the parallel scope map to fully banked
      // arrays. Remove the iterator binding.
      val vecBinds = binds
        .withFilter({ case (id, _) => id != iter })
        .map({ case (id, typ) =>
          id -> TArray(typ, List((range.u, range.u)))
        })

      e1.withScope(1)(e2 => checkC(combine)(e2 ++ vecBinds))._1
    }
    case view@CView(id, arrId, vdims) => env(arrId) match {
      case TArray(typ, adims) => {
        // Check if the view is defined inside an unroll
        if (env.getResources != 1) {
          throw ViewInsideUnroll(view.pos, arrId)
        }

        val (vlen, alen) = (vdims.length, adims.length)
        if(vlen != alen) {
          throw IncorrectAccessDims(arrId, alen, vlen)
        }

        // Check all dimensions in the view are well formed.
        val (env1, ndims) = adims.zip(vdims).foldLeft(env -> List[(Int, Int)]())({
          case ((env, dims), (arrDim, view)) =>
            val (nEnv, dim) = checkView(view, arrDim)(env)
            (nEnv, dim :: dims)
        })

        // Fully consume the array
        val nEnv = env1.addGadget(id, ViewGadget(env1.getGadget(arrId), adims))

        // Annotate the ids in the expressions
        val viewTyp = TArray(typ, ndims.reverse)
        id.typ = Some(viewTyp)
        arrId.typ = Some(env(arrId))

        // Add binding for the new array. The dimensions are reversed because
        // we used foldLeft above.
        nEnv.add(id, viewTyp)
      }
      case t => throw UnexpectedType(cmd.pos, "view", "array", t)
    }
    case view@CSplit(id, arrId, dims) => env(arrId) match {
      case TArray(typ, adims) => {
        if (env.getResources != 1) {
          throw ViewInsideUnroll(view.pos, arrId)
        }
        val (vlen, alen) = (dims.length, adims.length)
        if(vlen != alen) {
          throw IncorrectAccessDims(arrId, alen, vlen)
        }

        /**
         * Create a type for the split view. For the following split view:
         * ```
         * a[d0 bank b0][d1 bank b1]...
         * split s = a[by k0][by k1]...
         * ```
         * [[s]] gets the type t[k0 bank k0][(d0 / k0) bank (b0 / k0)] ....
         */
        val viewDims = adims.zip(dims).flatMap({
          case ((dim, bank), n) if n > 0 => {
            if (bank % n == 0) {
              List((n, n), (dim / n, bank / n))
            } else {
              throw InvalidSplitFactor(id, arrId, n, bank, dim)
            }
          }
          case ((dim, bank), n) => throw InvalidSplitFactor(id, arrId, n, bank, dim)
        })

        // Create a gadget for the view
        val nEnv = env.addGadget(id,
          ViewGadget(env.getGadget(arrId), adims , viewDims))

        // Annotate the ids in the expressions
        val viewTyp = TArray(typ, viewDims)
        id.typ = Some(viewTyp)
        arrId.typ = Some(env(arrId))

        nEnv.add(id, viewTyp)
      }
      case t => throw UnexpectedType(cmd.pos, "split", "array", t)
    }
    case CSeq(c1, c2) => {
      val (env1, binds) = env.withScope(1) { newScope =>
        checkC(c1)(newScope)
      }
      val env2 = checkC(c2)(env ++ binds)
      env2 merge env1
    }
    case CExpr(e) => checkE(e)._2
    case CReturn(expr) => {
      env.getReturn match {
        case Some(retType) => {
          val (t, e) = checkE(expr)
          if (isSubtype(t, retType) == false) {
            throw UnexpectedSubtype(expr.pos, "return", retType, t)
          }
          e
        }
        case None => throw ReturnNotInFunc(cmd.pos)
      }
    }
    case CEmpty => env
    case _:CDecorate => env
  }
}
