package fuselang.passes

import scala.{PartialFunction => PF}
import fuselang.Utils.RichOption
import fuselang.common._
import Transformer._
import EnvHelpers._
import Syntax._
import CompilerError._
import CodeGenHelpers._

object LowerUnroll extends PartialTransformer {
  var curIdx = 0
  def genName(prefix: String): String = {
    curIdx += 1;
    prefix + curIdx
  }

  private def cartesianProduct[T](llst: Seq[Seq[T]]): Seq[Seq[T]] = {
    def pel(e: T, ll: Seq[Seq[T]], a: Seq[Seq[T]] = Nil): Seq[Seq[T]] =
      ll match {
        case Nil => a.reverse
        case x +: xs => pel(e, xs, (e +: x) +: a)
      }

    llst match {
      case Nil => Nil
      case x +: Nil => x.map(Seq(_))
      case x +: _ =>
        x match {
          case Nil => Nil
          case _ =>
            llst
              .foldRight(Seq(x))((l, a) => l.flatMap(x => pel(x, a)))
              .map(_.dropRight(x.size))
        }
    }
  }

  private def genViewAccessExpr(suffix: Suffix, idx: Expr): Expr =
    suffix match {
      case Aligned(factor, e2) => (EInt(factor) * e2) + idx
      case Rotation(e) => e + idx
    }

  /**
    * A function for transforming an access expression where each index
    * in the array may or may not have a bank associated with it.
    *
    * For a given array and view:
    * ```
    * decl a: float[6 bank 3][4 bank 2];
    * view a_v = a[3*i: bank 1][_: bank 1];
    * ```
    *
    * The `TKey` corresponds to the bank the index for each dimension.
    * For example, the following key have the respective meanings:
    * 1. Seq(Some(0), Some(1): Bank 0 in dim 1 and Bank 1 in dim 2
    * 2. Seq(None, Some(0)): All the banks in dim 1 and Bank 0 in dim 2.
    *
    * The function returns a map from a Seq[Int] (bank numbers) to the
    * access expression that corresponds to it.
    */
  private type TKey = Seq[(Expr, Option[Int])]
  private type TVal = Map[Seq[Int], Expr]
  case class ViewTransformer(val t: TKey => TVal) extends AnyVal
  object ViewTransformer {
    def fromArray(id: Id, ta: TArray) = {
      // Get the name of all the memories
      val t = (idxs: Seq[(Expr, Option[Int])]) => {
        val allBanks: Seq[Seq[Int]] = ta.dims
          .zip(idxs)
          .map({
            case ((_, arrBank), (_, bank)) => {
              bank.map(Seq(_)).getOrElse(0 until arrBank)
            }
          })
        cartesianProduct(allBanks)
          .map(banks => {
            (banks, EArrAccess(Id(id.v + banks.mkString("_")), idxs.map(_._1)))
          })
          .toMap
      }
      ViewTransformer(t)
    }

    def fromView(dims: Seq[DimSpec], v: CView): ViewTransformer = {
      val t = (idxs: Seq[(Expr, Option[Int])]) => {
        if (idxs.length != dims.length) {
          throw PassError("LowerUnroll: Incorrect access dimensions")
        }

        // Bank and index for a dimension
        type PhyIdx = (Int, Expr)

        // Get the set of expressions "generated" by each idx. If the idx
        // key is None, we return the list with all expressions. Otherwise,
        // we return just the expression corresponding to that list.
        val eachIdx: Seq[Seq[(Int, PhyIdx)]] = idxs
          .zip(v.dims)
          .zip(dims.map(_._2))
          .map({
            case (((idxExpr, bank), View(suf, _, sh)), arrBank) => {
              // bankMap[i] is the banks in the array that correspond to the
              // bank `i` in this view.
              val shrinkFactor = sh.getOrElse(1)
              val viewBank = arrBank / shrinkFactor
              val bankMap: Seq[Seq[Int]] = (0 until viewBank)
                .map(s => (s.until(arrBank, viewBank)))
              // Get the banks accessed by this bank.
              val banks: Seq[Int] = bank
                .map(b => bankMap(b))
                .getOrElse(bankMap.flatten)
              banks.map(bank =>
                (bank, (bank, genViewAccessExpr(suf, idxExpr / EInt(arrBank))))
              )
            }
          })

        // Take the cartesians product of all generated expressions from
        // each index and tarnsform it into the result type
        cartesianProduct(eachIdx)
          .map(bankAndIdx => {
            val (banks, phyIndices) = bankAndIdx.unzip
            (banks, EPhysAccess(v.arrId, phyIndices))
          })
          .toMap
      }
      ViewTransformer(t)
    }
  }

  // XXX(rachit): There are three maps that currently track some form of
  // rewriting: rewriteMap transforms local variables, combineReg transforms
  // combine registers, and viewMap transforms arrays.
  //
  // Find the right abstraction for them and unify them.
  case class ForEnv(
      // The bank a variable is current referring to.
      idxMap: Map[Id, Int],
      // Rename for variables in the current binding.
      rewriteMap: Map[Id, Id],
      // Set of variables bound by this scope.
      // XXX(rachit): This should probably be a scoped set
      localVars: Set[Id],
      // Set of variables a combine register refers to.
      combineReg: Map[Id, Set[Id]],
      // Bindings for transformer for views.
      viewMap: Map[Id, ViewTransformer],
      // DimSpec of bound arrays in this context.
      dimsMap: Map[Id, TArray]
  ) extends ScopeManager[ForEnv]
      with Tracker[Id, Int, ForEnv] {
    def merge(that: ForEnv) = {
      ForEnv(
        this.idxMap ++ that.idxMap,
        this.rewriteMap ++ that.rewriteMap,
        this.localVars ++ that.localVars,
        this.combineReg ++ that.combineReg,
        this.viewMap ++ that.viewMap,
        this.dimsMap ++ that.dimsMap
      )
    }

    def get(key: Id) = this.idxMap.get(key)
    def add(key: Id, bank: Int) =
      this.copy(idxMap = this.idxMap + (key -> bank))

    def rewriteGet(key: Id) = this.rewriteMap.get(key)
    def rewriteAdd(k: Id, v: Id) =
      this.copy(rewriteMap = this.rewriteMap + (k -> v))

    def localVarAdd(key: Id) = this.copy(localVars = this.localVars + key)

    def combineRegAdd(k: Id, v: Set[Id]) =
      this.copy(combineReg = this.combineReg + (k -> v))
    def combineRegGet(k: Id) = this.combineReg.get(k)

    def viewAdd(k: Id, v: ViewTransformer) =
      this.copy(viewMap = viewMap + (k -> v))
    def viewGet(k: Id) =
      this.viewMap.get(k)

    def dimsAdd(k: Id, v: TArray) =
      this.copy(dimsMap = dimsMap + (k -> v))
    def dimsGet(k: Id) =
      this.dimsMap
        .get(k)
        .getOrThrow(Impossible(s"Dimensions for `$k' not bound"))

  }

  type Env = ForEnv
  val emptyEnv = ForEnv(Map(), Map(), Set(), Map(), Map(), Map())

  def unbankedDecls(id: Id, ta: TArray): Seq[(Id, Type)] = {
    val TArray(typ, dims, ports) = ta
    cartesianProduct(dims.map({
      case (size, banks) => (0 to banks - 1).map((size / banks, _))
    })).map(idxs => {
      val name = id.v + idxs.map(_._2).mkString("_")
      val dims = idxs.map({ case (s, _) => (s, 1) })
      (Id(name), TArray(typ, dims, ports))
    })
  }

  override def rewriteDeclSeq(ds: Seq[Decl])(implicit env: Env) = {
    ds.flatMap(d =>
      d.typ match {
        case ta: TArray => {
          unbankedDecls(d.id, ta).map((x: (Id, Type)) => Decl(x._1, x._2))
        }
        case _ => List(d)
      }
    ) -> ds.foldLeft[Env](env)({
      case (env, Decl(id, typ)) =>
        typ match {
          case ta: TArray =>
            env.viewAdd(id, ViewTransformer.fromArray(id, ta)).dimsAdd(id, ta)
          case _ => env
        }
    })
  }

  private def getBanks(arr: Id, idxs: Seq[Expr])(implicit env: Env) =
    env
      .dimsGet(arr)
      .dims
      .zip(idxs)
      .map({
        case ((_, bank), idx) =>
          idx match {
            case EInt(n, 10) => Some(n % bank)
            case EInt(_, _) =>
              throw NotImplemented(
                "Indexing using non decimal integers",
                idx.pos
              )
            case EVar(id) => env.get(id)
            case _ => None
          }
      })

  // Rewrite Seq(A0 --- B0, A1 --- B1, ...) to
  // { A0; A1 ... } --- { B0; B1 ... }
  private def interleaveSeq(cmds: Seq[Command]): Seq[Command] = {
    val nested = if (cmds.forall(cmd => cmd.isInstanceOf[CSeq])) {
      cmds.collect[Seq[Command]]({ case CSeq(cmds) => cmds })
    } else {
      cmds.map(Seq(_))
    }
    nested.transpose.map(mergePar(_))
  }

  /**
   * Tries to merge Sequences of pars that contain only for loops.
   * This is called when we generate for loops in parallel from an unroll.
   * Transforms:
   * ```
   * for (let i ...) { A0 --- B0 };
   * for (let i ...) { A1 --- B1 };
   * ```
   * into:
   * ```
   * for (let i ...) { mergePar(A0; A1) --- mergePar(B0; B1) }
   * ```
   * Read https://github.com/cucapra/dahlia/issues/311 for details.
   */
  private def mergePar(cmds: Seq[Command]): Command = {
    // If all the commands are for loops, merge their bodies
    if (cmds.forall(_.isInstanceOf[CFor])) {
      val fors = cmds.map[CFor](_.asInstanceOf[CFor])
      assert(
        fors.map(_.range).distinct.length == 1,
        "sequence of for loops had different ranges"
      )
      val cfor = fors.head
      val (pars, combs) = fors.map(c => (c.par, c.combine)).unzip
      val mergedBody = interleaveSeq(pars)
      // Recursively merge the generated par body of the loop
      cfor.copy(
        par = mergePar(mergedBody),
        combine = CSeq.smart(combs)
      )
    } else {
      CPar.smart(cmds)
    }
  }

  // Generate a sequence of commands based on `allExps`
  private def condCmd(
      allExprs: TVal,
      idxs: Seq[Expr],
      arrDims: Seq[DimSpec],
      newCommand: Expr => Command
  )(implicit env: Env): (Command, Env) = {
    // If we got exactly on value in TVal, that means that the returned
    // expression corresponds exactly to the input bank. In this case,
    // don't generate a condition.
    if (allExprs.size == 1) {
      val elem = allExprs.toArray
      return (newCommand(elem(0)._2), env)
    }
    val (bankComps, prelude) = idxs
      .zip(arrDims)
      .map({
        case (idx, (_, arrBank)) => {
          val bank = genName("_bank")
          (Id(bank), CLet(Id(bank), None, Some(idx % EInt(arrBank))))
        }
      })
      .unzip
    val condAssign = allExprs.foldLeft[Command](CEmpty)({
      case (cmd, (bankVals, accExpr)) => {
        val cond = bankVals
          .zip(bankComps)
          .foldLeft[Expr](EBool(true))({
            case (expr, (bv, bankComp)) => {
              EBinop(
                BoolOp("&&"),
                EBinop(EqOp("=="), EVar(bankComp), EInt(bv)),
                expr
              )
            }
          })
        CIf(cond, newCommand(accExpr), cmd)
      }
    })

    // Update the environment with all the newly generated names
    CPar.smart(prelude :+ condAssign) -> bankComps.foldLeft[Env](env)({
      case (e, n) => e.rewriteAdd(n, n)
    })
  }

  def myRewriteC: PF[(Command, Env), (Command, Env)] = {
    // Transform reads from memories
    case (c @ CLet(bind, _, Some(acc @ EArrAccess(arrId, idxs))), env) => {
      val transformer = env.viewGet(arrId)
      if (transformer.isDefined) {
        val allExprs =
          (transformer.get.t)(idxs.zip(getBanks(arrId, idxs)(env)))
        // Calculate the value for all indices and let-bind them.
        val TArray(typ, arrDims, _) = env.dimsGet(arrId)
        // We generate update expressions for the variable.
        val updCmd = CUpdate(EVar(bind), acc)
        val (newCmd, nEnv) =
          condCmd(allExprs, idxs, arrDims, (e) => updCmd.copy(rhs = e))(env)
        rewriteC(CPar.smart(CLet(bind, Some(typ), None), newCmd))(nEnv)
      } else {
        c -> env
      }
    }
    // Rewrite banked let bound memories
    case (CLet(id, Some(ta: TArray), None), env) => {
      val cmd =
        CPar(
          unbankedDecls(id, ta).map({ case (i, t) => CLet(i, Some(t), None) })
        )
      cmd -> env
        .dimsAdd(id, ta)
        .viewAdd(id, ViewTransformer.fromArray(id, ta))
    }
    // Handle case for initialized, unbanked memories.
    case (CLet(id, Some(ta: TArray), init), env) => {
      val nInit = init.map(i => rewriteE(i)(env)._1)
      if (ta.dims.exists({ case (_, bank) => bank > 1 })) {
        throw NotImplemented("Banked local arrays with initial values")
      }
      CLet(Id(v = id.v + "0"), Some(ta), nInit) -> env
    }
    // Rewrite let bound variables if needed.
    case (c @ CLet(id, _, init), env) => {
      val nInit = init.map(i => rewriteE(i)(env)._1)
      // Don't rewrite this name if there is already a binding in
      // rewrite map.
      val rewriteVal = env.rewriteGet(id)
      if (rewriteVal.isDefined) {
        c.copy(e = nInit) -> env
      } else {
        val suf = env.idxMap.toList.sortBy(_._1.v).map(_._2).mkString("_")
        val newName = id.copy(s"${id.v}_${suf}")
        c.copy(id = newName, e = nInit) -> env
          .localVarAdd(id)
          .rewriteAdd(id, newName)
      }
    }
    // Handle views
    case (v @ CView(id, arrId, dims), env) => {
      val TArray(typ, arrDims, ports) = env.dimsGet(arrId)
      val nDims = arrDims
        .zip(dims.map(_.shrink))
        .map({
          case ((len, bank), shrink) =>
            (len, shrink.map(sh => bank / sh).getOrElse(bank))
        })
      val nEnv = env
        .dimsAdd(id, TArray(typ, nDims, ports))
        .viewAdd(id, ViewTransformer.fromView(env.dimsGet(arrId).dims, v))
      (CEmpty, nEnv)
    }
    case (c @ CFor(range, _, par, combine), env) => {
      if (range.u > 1 && range.s != 0) {
        throw NotImplemented(
          "Unrolling loops with non-zero start idx",
          range.pos
        )
      }
      // Rewrite the nested commands and pull them up into the sequence. At
      // this point, we haven't duplicated anything.
      val nested: Seq[Command] = par match {
        case CSeq(cmds) => cmds
        case _ => Seq(par)
      }
      val (nestedRep, nestedLocals) = nested
        .map(cmd => {
          val (duplicated, envs) = if (range.u > 1) {
            (0 until range.u)
              .map(idx => rewriteC(cmd)(env.add(range.iter, idx)))
              .unzip
          } else {
            val (nCmd, env1) = rewriteC(cmd)(env)
            Seq(nCmd) -> Seq(env1)
          }
          val allLocals = envs.flatMap(_.localVars).toSet
          val ret = cmd match {
            case _: CFor => mergePar(duplicated)
            case _ => {
              CPar.smart(duplicated)
            }
          }
          ret -> allLocals
        })
        .unzip
      val (nPar, locals) = (CSeq.smart(nestedRep), nestedLocals.flatten.toSet)

      val nEnv = locals.foldLeft(env)({
        case (env, l) => {
          val regs = (0 to range.u - 1).map(i => {
            val suf = ((range.iter, i) :: env.idxMap.toList)
              .sortBy(_._1.v)
              .map(_._2)
              .mkString("_")
            Id(s"${l.v}_${suf}")
          })
          env.combineRegAdd(l, regs.toSet)
        }
      })
      val nComb = rewriteC(combine)(nEnv)._1

      val nRange = range.copy(e = range.e / range.u, u = 1).copy()

      c.copy(range = nRange, par = nPar, combine = nComb) -> env
    }
    case (CReduce(rop, l, r), env) => {
      import Syntax.{OpConstructor => OC}
      val binop = rop.op match {
        case "+=" => NumOp("+", OC.add)
        case "-=" => NumOp("-", OC.sub)
        case "*=" => NumOp("*", OC.mul)
        case "/=" => NumOp("/", OC.div)
        case op => throw PassError(s"Unknown reduction operator: $op", rop.pos)
      }
      // Read the current value of the combine LHS
      val (prelude, curVal, nEnv) = l match {
        case _: EArrAccess | _: EPhysAccess => {
          val name = genName("_rread")
          (
            Seq(CLet(Id(name), None, Some(l))),
            EVar(Id(name)),
            env.rewriteAdd(Id(name), Id(name))
          )
        }
        case e => (Seq(), e, env)
      }
      val nR = r match {
        case EVar(rId) =>
          nEnv
            .combineRegGet(rId)
            .map(ids => {
              ids.foldLeft[Expr](curVal)({
                case (l, r) => EBinop(binop, l, EVar(r))
              })
            })
            .getOrElse(r)
        case _ =>
          throw NotImplemented(
            "LowerUnroll: Reduce with complex RHS expression"
          )
      }
      rewriteC(CSeq.smart(prelude :+ CUpdate(l, nR)))(nEnv)
    }
    case (CUpdate(lhs, rhs), env0) =>
      val (nRhs, env) = rewriteE(rhs)(env0)
      val c = CUpdate(lhs, nRhs)
      lhs match {
        case e @ EVar(id) =>
          c.copy(lhs = env.rewriteGet(id).map(nId => EVar(nId)).getOrElse(e)) -> env
        case EArrAccess(id, idxs) => {
          val transformer = env.viewGet(id)
          if (transformer.isDefined) {
            val allExprs =
              (transformer.get.t)(idxs.zip(getBanks(id, idxs)(env)))
            // Calculate the value for all indices and let-bind them.
            val arrDims = env.dimsGet(id).dims
            val (newCmd, nEnv) =
              condCmd(allExprs, idxs, arrDims, (e) => c.copy(lhs = e))(env)
            rewriteC(newCmd)(nEnv)
          } else {
            c -> env
          }
        }
        case (EPhysAccess(id, physIdxs)) => {
          val transformer = env
            .viewGet(id)
            .getOrThrow(
              Impossible(s"Array `$id' has no transformer associated with it.")
            )
          val allExprs =
            (transformer.t)(physIdxs.map(idx => (idx._2, Some(idx._1))))
          assert(allExprs.size == 1,
            s"Physical access expression generated more than one expression")
          // Calculate the value for all indices and let-bind them.
          val arrDims = env.dimsGet(id).dims
          // Generate conditional assignment tree
          val (newCmd, nEnv) =
            condCmd(
              allExprs,
              physIdxs.map(_._2),
              arrDims,
              (e) => c.copy(lhs = e)
            )(env)
          rewriteC(newCmd)(nEnv)
        }
        case _ => throw Impossible("Not an LHS")
      }
  }

  def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
    case (e @ EVar(id), env) => {
      env.rewriteGet(id).map(nId => EVar(nId)).getOrElse(e) -> env
    }
    // Since physical access expression imply exactly on expression, we can
    // rewrite them.
    case (EPhysAccess(id, physIdxs), env) => {
      val transformer = env
        .viewGet(id)
        .getOrThrow(
          Impossible(s"Array `$id' has no transformer associated with it.")
        )
      val allExprs =
        (transformer.t)(physIdxs.map(idx => (idx._2, Some(idx._1))))
      assert(allExprs.size == 1,
        s"Physical access expression generated more than one expression")
      val nExpr = allExprs.values.toArray
      rewriteE(nExpr(0))(env)
    }
  }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)

  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)

}
