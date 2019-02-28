package fuselang

import Syntax._
import Errors._
import TypeInfo._

/**
 * Type checker implementation for Fuse. Apart from normal typechecking, such as
 * ensuring condition in `if` is a boolean, it does following.
 * - Linear properties of banks in memories.
 * - Checking combine blocks correctly use bound variables.
 * - Generate subtype joins for binary operators.
 * - Checks read/write capabilites for array accesses
 *
 * It also MUTATES and ANNOTATES the input AST:
 * - CLet with an explicit type get the correct type
 * - Binders for id update the `typ` var defined on the Id class.
 */

/**
 * Type checking array accesses is the most important and complex step.
 * Here is the pseudocode for the algorithm:
 *
 *  function consume_banks(expr, env):
 *    consume banks and return product of static parts of index types of accessors.
 *
 *  function check_eaa(env, cap_env, req_resources, required_cap, expr):
 *    if cap_env(expr) == R and required_cap == W: FAIL
 *    else if cap_env(expr) == R: SUCCESS
 *    else if cap_env(expr) == W: FAIL
 *    else:
 *      n = consume_banks(expr, env)
 *      if required_cap == W and req_resources != n: FAIL
 *      cap_env(expr).bind(req_resources)
 *      SUCCESS
 */
object TypeChecker {
  import TypeEnv._

  /* A program consists of a list of function or type definitions, a list of
   * variable declarations and then a command. We build up an environment with
   * all the declarations and definitions, then check the command in that environment
   * (`checkC`).
   */
  def typeCheck(p: Prog) = {
    val funcsEnv = p.defs.foldLeft(emptyEnv)({ case (e, d) => checkDef(d, e) })
    val initEnv = p.decls.foldLeft(funcsEnv)({ case (env, Decl(id, typ)) =>
      val rTyp = resolveType(typ, env);
      id.typ = Some(rTyp)
      env.add(id, rTyp)
    })
    checkC(p.cmd)(initEnv, 1)
  }

  private def checkDef(defi: Definition, env: Environment) = defi match {
    case FuncDef(id, args, body) => {
      val envWithArgs = args.foldLeft(env.addScope)({ case (env, Decl(id, typ)) =>
        val rTyp = resolveType(typ, env)
        id.typ = Some(rTyp);
        env.add(id, rTyp)
      })
      val bodyEnv = checkC(body)(envWithArgs, 1)
      bodyEnv.endScope._1.add(id, TFun(args.map(_.typ)))
    }
    case RecordDef(name, fields) => {
      val rFields = fields.map({ case (k, t) => k -> resolveType(t, env) })
      env.addType(name, TRecType(name, rFields))
    }
  }

  private def resolveType(typ: Type, env: Environment): Type = typ match {
    case TAlias(n) => env.getType(n)
    case TFun(args) => TFun(args.map(resolveType(_, env)))
    case arr@TArray(t, _) => arr.copy(typ = resolveType(t, env))
    case t => t
  }

  private def consumeBanks
    (id: Id, idxs: List[Expr], dims: List[(Int, Int)])
    (implicit env: Environment, rres: ReqResources) =
    idxs.zipWithIndex.foldLeft((env, 1))({
      case ((env1, bres), (e, i)) => checkE(e)(env1, rres) match {
        case (TIndex((s, e), _), env2) =>
          env2.update(id, env2(id).consumeDim(i, e - s)) -> bres * (e - s)
        case (TStaticInt(v), env2) =>
          env2.update(id, env(id).consumeBank(i, v % dims(i)._2)) -> bres * 1
        case (TSizedInt(_), env2) =>
          if (dims(i)._2 != 1) throw InvalidDynamicIndex(id, dims(i)._2)
          else env2.update(id, env(id).consumeBank(i, 0)) -> bres * 1

        case (t, _) => throw InvalidIndex(id, t)
      }
    })

  private def checkLVal(e: Expr)(implicit env: Environment, rreq: ReqResources) = e match {
    case EArrAccess(id, idxs) => env(id).typ match {
      // This only triggers for r-values. l-values are checked in checkLVal
      case TArray(typ, dims) => {
        if (dims.length != idxs.length) {
          throw IncorrectAccessDims(id, dims.length, idxs.length)
        }
        // Bind the type of to Id
        id.typ = Some(env(id).typ);
        // Capability check
        env.getCap(e) match {
          case Some(Write) => throw AlreadyWrite(e)
          case Some(Read) => throw InvalidCap(e, Write, Read)
          case None => {
            val (e1, bres) = consumeBanks(id, idxs, dims)
            if (bres != rreq)
              throw InsufficientResourcesInUnrollContext(rreq, bres, e)
            typ -> e1.addCap(e, Write)
          }
        }
      }
      case t => throw UnexpectedType(e.pos, "array access", s"$t[]", t)
    }
    case _ => checkE(e)
  }

  private def checkB(t1: Type, t2: Type, op: BOp) = op match {
    case OpEq() | OpNeq() => {
      if (t1.isInstanceOf[TArray])
        throw UnexpectedType(op.pos, op.toString, "primitive types", t1)
      else if (t1 :< t2 || t2 :< t1) TBool()
      else throw UnexpectedSubtype(op.pos, op.toString, t1, t2)
    }
    case _:OpAnd | _:OpOr => (t1, t2) match {
      case (TBool(), TBool()) => TBool()
      case _ => throw BinopError(op, t1, t2)
    }
    case _:OpLt | _:OpLte | _:OpGt | _:OpGte => (t1, t2) match {
      case (_:IntType, _:IntType) => TBool()
      case (_: TFloat, _: TFloat) => TBool()
      case _ => throw BinopError(op, t1, t2)
    }
    case _:OpAdd | _:OpMul | _:OpSub | _:OpDiv | _:OpMod | _:OpBAnd |
         _:OpBOr | _:OpBXor => (t1, t2) match {
      case (_:IntType, _:IntType) => t1.join(t2, op.toFun)
      case (_: TFloat, _: TFloat) => TFloat()
      case _ => throw BinopError(op, t1, t2)
    }
    case _:OpLsh | _:OpRsh => (t1, t2) match {
      case (_:TSizedInt, _:IntType) => t1
      case (_:TStaticInt, _:IntType) => TSizedInt(32)
      case _ => throw BinopError(op, t1, t2)
    }
  }

  // Implicit parameters can be elided when a recursive call is reusing the
  // same env and its. See EBinop case for an example.
  private def checkE(expr: Expr)(implicit env:Environment, rres: ReqResources): (Type,Environment) = expr match {
    case EFloat(_) => TFloat() -> env
    case EInt(v, _) => TStaticInt(v) -> env
    case EBool(_) => TBool() -> env
    case ERecLiteral(_) => throw RecLiteralNotInBinder(expr.pos)
    case EVar(id) => {
      // Add type information to variable
      id.typ = Some(env(id).typ);
      env(id).typ -> env
    }
    case EBinop(op, e1, e2) => {
      val (t1, env1) = checkE(e1)
      val (t2, env2) = checkE(e2)(env1, rres)
      checkB(t1, t2, op) -> env2
    }
    case EApp(f, args) => env(f).typ match {
      case TFun(argTypes) => {
        // All functions return `void`.
        TVoid() -> args.zip(argTypes).foldLeft(env)({ case (e, (arg, expectedTyp)) => {
          val (typ, e1) = checkE(arg)(e, rres);
          if (typ :< expectedTyp == false) {
            throw UnexpectedSubtype(arg.pos, "parameter", expectedTyp, typ)
          }
          // If an array id is used as a parameter, consume it completely.
          // This works correctly with capabilties.
          (typ, arg) match {
            case (_:TArray, EVar(id)) => e1.update(id, e1(id).consumeAll)
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
    case EArrAccess(id, idxs) => env(id).typ match {
      // This only triggers for r-values. l-values are checked in checkLVal
      case TArray(typ, dims) => {
        if (dims.length != idxs.length) {
          throw IncorrectAccessDims(id, dims.length, idxs.length)
        }
        // Bind the type of to Id
        id.typ = Some(env(id).typ);
        // Check capabilities
        env.getCap(expr) match {
          case Some(Write) =>
            throw InvalidCap(expr, Read, Write)
          case Some(Read) => typ -> env
          case None => {
            val e1 = consumeBanks(id, idxs, dims)._1
            typ -> e1.addCap(expr, Read)
          }
        }
      }
      case t => throw UnexpectedType(expr.pos, "array access", s"$t[]", t)
    }
  }

  private def checkC(cmd: Command)(implicit env:Environment, rres: ReqResources):Environment = cmd match {
    case CPar(c1, c2) => checkC(c2)(checkC(c1), rres)
    case CIf(cond, cons, alt) => {
      val (cTyp, e1) = checkE(cond)(env.addScope, rres)
      if (cTyp != TBool()) {
        throw UnexpectedType(cond.pos, "if condition", TBool().toString, cTyp)
      } else {
        val (e2, _, _) = checkC(cons)(e1, rres).endScope
        val (e3, _, _) = checkC(alt)(e1, rres).endScope
        e2 merge e3
      }
    }
    case CWhile(cond, body) => {
      val (cTyp, e1) = checkE(cond)(env.addScope, rres)
      if (cTyp != TBool()) {
        throw UnexpectedType(cond.pos, "while condition", TBool().toString, cTyp)
      } else {
        checkC(body)(e1, rres).endScope._1
      }
    }
    case CUpdate(lhs, rhs) => {
      val (t1, e1) = checkLVal(lhs)
      val (t2, e2) = checkE(rhs)(e1, rres)
      // :< is a subtyping method we defined on the type trait.
      if (t2 :< t1) (t1, t2, lhs) match {
        // Reassignment of static ints upcasts to bit<32>
        case (TStaticInt(_), TStaticInt(_), EVar(id)) =>
          e2.update(id, Info(id, TSizedInt(32)))
        case (TStaticInt(_), TSizedInt(_), EVar(id)) =>
          e2.update(id, Info(id, t2))
        case _ => e2
      }
      else throw UnexpectedSubtype(rhs.pos, "assignment", t1, t2)
    }
    case CReduce(rop, l, r) => {
      val (t1, e1) = checkLVal(l)
      val (ta, e2) = checkE(r)(e1, rres)
      (t1, ta) match {
        case (t1, TArray(t2, dims)) =>
          if (t2 :< t1 == false || dims.length != 1 || dims(0)._1 != dims(0)._2) {
            throw ReductionInvalidRHS(r.pos, rop, t1, ta)
          } else {
            e2
          }
        case _ =>
          throw ReductionInvalidRHS(r.pos, rop, t1, ta)
      }
    }
    case l@CLet(id, typ, exp@ERecLiteral(fs)) => typ match {
      case Some(typ) => resolveType(typ, env) match {
        case recTyp@TRecType(name, expTypes) => {
          // Typecheck expressions in the literal and generate a new id to type map.
          val (env1, actualTypes) = fs.foldLeft((env, Map[Id, Type]()))({
            case ((env, map), (id, exp)) => {
              val (t, e1) = checkE(exp)(env, rres)
              (e1, map + (id -> t))
            }
          })

          // Check all fields have the expected type and the literal has all required fields.
          expTypes.keys.foreach(field => {
            val (eTyp, acTyp) = (expTypes(field), actualTypes.get(field))
            if (acTyp.isDefined == false) {
              throw MissingField(exp.pos, name, field)
            }
            if (acTyp.get :< eTyp == false) {
              throw UnexpectedType(
                fs(field).pos, "record literal", expTypes(field).toString, acTyp.get)
            }
          })

          // Check there are no extra fields in the literal
          (actualTypes.keys.toSet diff expTypes.keys.toSet).foreach({ case field => {
            throw ExtraField(field.pos, name, field)
          }})
          env1.add(id, recTyp)
        }
        case t => throw UnexpectedType(exp.pos, "let", "record type", t)
      }
      case None => throw ExplicitRecTypeMissing(l.pos, id)
    }
    case l@CLet(id, typ, exp) => {
      // Check if the explicit type is bound in scope
      val rTyp = typ.map(resolveType(_, env))
      val (t, e1) = checkE(exp)
      rTyp match {
        case Some(t2) if t :< t2 => e1.add(id, t2)
        case Some(t2) => throw UnexpectedType(exp.pos, "let", t.toString, t2)
        case None => l.typ = Some(t); e1.add(id, t)
      }
    }
    case CView(id, k@Shrink(arrId, vdims)) => env(arrId).typ match {
      case arrTyp@TArray(t, dims) => {
        // Annotate arrId with type
        arrId.typ = Some(arrTyp)
        // Cannot create shrink views in unrolled contexts
        if (rres != 1) {
          throw ViewInsideUnroll(cmd.pos, k, arrId)
        }
        // Check if view has the same dimensions as underlying array.
        if (vdims.length != dims.length) {
          throw IncorrectAccessDims(arrId, dims.length, vdims.length)
        }
        // Foreach dimension, check if bankingFactor % shrinkFactor == 0
        // and the offset variable is a constant or a simple iterator.
        val env1 = dims.zip(vdims).zipWithIndex.foldLeft(env)({ case (env, (zdim, idx)) => {
          val ((_, bank), (e, width, _)) = zdim
          if (bank % width != 0) {
            throw InvalidShrinkWidth(e.pos, bank, width)
          }
          // Completely conumse the current dimension
          val env2 = env.update(arrId, env(arrId).consumeDim(idx, bank))
          val (accessType, env3) = checkE(e)(env2, rres)
          accessType match {
            case _: TStaticInt | _: TIndex => true
            case t => throw InvalidIndex(arrId, t)
          }
          env3
        }})
        // Add view into scope
        val typ = TArray(t, vdims.map({ case (_, w, _) => (w, w) }))
        env1.add(id, typ)
      }
      case t => throw UnexpectedType(cmd.pos, "shrink view", "array", t)
    }
    case CFor(range, par, combine) => {
      val iter = range.iter
      // Add binding for iterator in a separate scope.
      val e1 = env.addScope.add(iter, range.idxType).addScope
      // Check for body and pop the scope.
      val (e2, binds, _) = checkC(par)(e1, range.u * rres).endScope
      // Create scope where ids bound in the parallel scope map to fully banked arrays.
      val vecBinds = binds.map({ case (id, inf) =>
        id -> Info(id, TArray(inf.typ, List((range.u, range.u))))
      })
      // Remove the binding of the iterator and add the updated vector bindings.
      checkC(combine)(e2.endScope._1.addScope ++ vecBinds, rres).endScope._1
    }
    case CExpr(e) => checkE(e)._2
    case CEmpty => env
    case CSeq(c1, c2) => {
      val _ = checkC(c1)
      val e2 = checkC(c2)(env, rres)
      // FIXME(rachit): This should probably be intersection of e1 and e2
      e2
    }
  }
}
