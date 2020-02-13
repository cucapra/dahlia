package fuselang.typechecker

import Subtyping._
import NewTypeEnv._
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
object NewTypeChecker {

  def pr[A](v: A): A = {
    println(v)
    v
  }

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
      val env2 = env.withScope { newScope =>

        // Bind all declarations to the body.
        val envWithArgs = args.foldLeft(newScope)({ case (env, Decl(id, typ)) =>
          val rTyp = env.resolveType(typ)
          id.typ = Some(rTyp);
          env.add(id, rTyp)
        })

        // Add the return type
        val envWithRet = envWithArgs.withReturn(ret)

        bodyOpt
          .map(body => checkC(body)(envWithRet))
          .getOrElse(envWithRet)
      }
      env2.add(id, TFun(args.map(_.typ), ret))
    }
    case RecordDef(name, fields) => {
      val rFields = fields.map({case (k, t) => k -> env.resolveType(t)})
      env.addType(name, TRecType(name, rFields))
    }
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
      case (_:TRational|_:TDouble|_:TFloat|_:TFixed, _:TRational) => TBool()
      case _ => throw BinopError(op, "float, integer, rational, or double", t1, t2)
    }
    case _:NumOp =>
      joinOf(t1, t2, op).getOrThrow(NoJoin(op.pos, op.toString, t1, t2))
    //case _:DoubleOp =>
    //  joinOf(t1, t2, op).getOrThrow(NoJoin(op.pos, op.toString, t1, t2))
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
    case ERational(v) => TRational(v) -> env
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
          e1
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
    case EArrAccess(id, idxs) => env(id).matchOrError(expr.pos, "array access", s"array type"){
      case TArray(typ, dims, _) => {
        if (dims.length != idxs.length) {
          throw IncorrectAccessDims(id, dims.length, idxs.length)
        }
        idxs.foldLeft(env)((env, idx) => {
          val (typ, nEnv) = checkE(idx)(env)
          typ match {
            case _:IntType => ()
            case _ => throw UnexpectedType(idx.pos, "array index", "integer type", typ)
          }
          nEnv
        })
        // Bind the type of to Id
        id.typ = Some(env(id));
        typ -> env
      }
    }
  }

  // Check if this array dimension is well formed and return the dimension
  // spec for the corresponding dimension in the view.
  private def checkView(view: View, arrDim: DimSpec)
                       (implicit env: Environment): (Environment, DimSpec) = {

    val View(suf, prefix, shrink) = view
    val (len, bank) = arrDim

    // Shrinking factor must be a factor of banking for the dimension
    if (shrink.isDefined && (shrink.get > bank || bank % shrink.get != 0)) {
      throw InvalidShrinkWidth(view.pos, bank, shrink.get)
    }

    val newBank = shrink.getOrElse(bank)

    // Get the indexing expression
    val idx = suf match {
      case Aligned(fac, idx) => if (newBank > fac) {
        throw InvalidAlignFactor(suf.pos, s"Invalid align factor. Banking factor $newBank is bigger than alignment factor $fac.")
      }
      else if(fac % newBank != 0) {
        throw InvalidAlignFactor(suf.pos,
          s"Invalid align factor. Banking factor $newBank not a factor of the alignment factor $fac.")
      } else {
        idx
      }
      case Rotation(idx) => idx
    }

    val (typ, nEnv) = checkE(idx)
    typ.matchOrError(idx.pos, "view", "integer type") {
      case _:IntType => () // IntTypes are valid
    }

    (nEnv, (prefix.getOrElse(len) -> newBank))
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
    case CSeq(c1, c2) => checkC(c2)(checkC(c1))
    case CIf(cond, cons, alt) => {
      val (cTyp, e1) = checkE(cond)(env)
      cTyp.matchOrError(cond.pos, "if condition", "bool"){ case _:TBool => () }
      e1.withScope(e => checkC(cons)(e))
      e1.withScope(e => checkC(alt)(e))
      // No binding updates need to be reflected.
      e1
    }
    case CWhile(cond, pipeline, body) => {
      checkPipeline(pipeline, cmd, body)
      val (cTyp, e1) = checkE(cond)(env)
      if (cTyp != TBool()) {
        throw UnexpectedType(cond.pos, "while condition", TBool().toString, cTyp)
      }
      e1.withScope(e => checkC(body)(e))
    }
    case CUpdate(lhs, rhs) => {
      val (t1, e1) = checkE(lhs)
      val (t2, e2) = checkE(rhs)(e1)
      if (isSubtype(t2, t1)) e2
      else throw UnexpectedSubtype(rhs.pos, "assignment", t1, t2)
    }
    case CReduce(_, l, r) => {
      val (t1, e1) = checkE(l)
      val (t2, e2) = checkE(r)(e1)

      if (isSubtype(t2, t1)) e2
      else throw UnexpectedSubtype(r.pos, "reduction operator", t1, t2)
    }
    case l@CLet(id, typ, Some(EArrLiteral(idxs))) => {
      val expTyp = typ.getOrThrow(ExplicitTypeMissing(l.pos, "Array literal", id))

      env
        .resolveType(expTyp)
        .matchOrError(l.pos, "Let bound array literal", "array type") {
          case ta@TArray(elemTyp, dims, _) => {
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
            nEnv.add(id, ta)
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
      // a static int, upcast it to sized int; if the type is rational,
      // upcast it to double. We do not allow variables to have
      // static or rational types.
      val rTyp = typ.map(env.resolveType(_) match {
        case TStaticInt(v) => TSizedInt(bitsNeeded(v), false)
        case _:TRational => TDouble()
        case t => t
      })
      // Check the type of the expression
      val (t, e1) = checkE(exp)
      // Check if type of expression is a subtype of the annotated type.
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
            case _:TRational => TDouble()
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

      env.add(id, fullTyp)
    }
    case CFor(range, pipeline, par, combine) => {
      // Check if this is a pipelined loop.
      checkPipeline(pipeline, cmd, par)

      // Check the for loop body and make variables declared in loop body
      // available to the combine block.
      env.withScope { newScope =>
        // Add binding for iterator in a separate scope.
        val forEnv = newScope.add(range.iter, range.idxType)
        val combEnv = checkC(par)(forEnv)
        checkC(combine)(combEnv)
      }
    }
    case CView(id, arrId, vdims) => env(arrId) match {
      case TArray(typ, adims, port) => {
        val (vlen, alen) = (vdims.length, adims.length)
        if(vlen != alen) {
          throw IncorrectAccessDims(arrId, alen, vlen)
        }

        val (env1, viewDims) =
          adims
            .zip(vdims)
            .foldLeft((env, List[DimSpec]()))({
              case ((env, viewDims), (arrDim, view)) =>
                val (nEnv, vDim) = checkView(view, arrDim)(env)
                (nEnv, vDim :: viewDims)
            })

        // View get the same type the array they are built from.
        val viewTyp = TArray(typ, viewDims.reverse, port)
        id.typ = Some(viewTyp)
        arrId.typ = Some(env(arrId))

        // Add binding for the new array
        env1.add(id, viewTyp)
      }
      case t => throw UnexpectedType(cmd.pos, "view", "array", t)
    }
    case CSplit(id, arrId, dims) => env(arrId) match {
      case TArray(typ, adims, ports) => {
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

        // Annotate the ids in the expressions
        val viewTyp = TArray(typ, viewDims, ports)
        id.typ = Some(viewTyp)
        arrId.typ = Some(env(arrId))

        env.add(id, viewTyp)
      }
      case t => throw UnexpectedType(cmd.pos, "split", "array", t)
    }
    case CExpr(e) => checkE(e)._2
    case CReturn(expr) => {
      val retType = env.getReturn.get
      val (t, e) = checkE(expr)
      if (isSubtype(t, retType) == false) {
        throw UnexpectedSubtype(expr.pos, "return", retType, t)
      }
      e
    }
    case CEmpty => env
    case _:CDecorate => env
  }
}
