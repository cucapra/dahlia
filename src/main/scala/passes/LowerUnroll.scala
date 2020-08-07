package fuselang.passes

import scala.{PartialFunction => PF}
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
    * A "function" for transforming an access expression where each index
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
          throw Impossible("LowerUnroll: Incorrect access dimensions")
        }

        // Get the set of expressions "generated" by each idx. If the idx
        // key is None, we return the list with all expressions. Otherwise,
        // we return just the expression corresponding to that list.
        val eachIdx: Seq[Seq[(Int, Expr)]] = idxs
          .zip(v.dims)
          .map({
            case ((idx, bank), View(suf, _, sh)) => {
              val banks = bank.map(Seq(_)).getOrElse(0 until sh.getOrElse(1))
              banks.map(bank => (bank, genViewAccessExpr(suf, idx)))
            }
          })

        // Take the cartesians product of all generated expressions from
        // each index and tarnsform it into the result type
        cartesianProduct(eachIdx)
          .map(bankAndIdx => {
            val (banks, indices) = bankAndIdx.unzip
            (banks, EArrAccess(v.arrId, indices))
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
      dimsMap: Map[Id, Seq[DimSpec]]
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

    def dimsAdd(k: Id, v: Seq[DimSpec]) =
      this.copy(dimsMap = dimsMap + (k -> v))
    def dimsGet(k: Id) = {
      println(k)
      this.dimsMap.get(k).get
    }
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
          case TArray(_, dims, _) => env.dimsAdd(id, dims)
          case _ => env
        }
    })
  }

  private def getBanks(arr: Id, idxs: Seq[Expr])(implicit env: Env) =
    env
      .dimsGet(arr)
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

  def myRewriteC: PF[(Command, Env), (Command, Env)] = {
    // Rewrite banked let bound memories
    case (CLet(id, Some(ta: TArray), None), env) => {
      val cmd =
        CPar(
          unbankedDecls(id, ta).map({ case (i, t) => CLet(i, Some(t), None) })
        )
      cmd -> env
    }
    // Handle case for initialized, unbanked memories.
    case (CLet(id, Some(ta: TArray), init), env) => {
      val nInit = init.map(i => rewriteE(i)(env)._1)
      if (ta.dims.exists({ case (_, bank) => bank > 1 })) {
        throw NotImplemented("Banked local arrays with initial values")
      }
      CLet(Id(v = id.v + "0"), Some(ta), nInit) -> env
    }
    // Rewrite let bound variables
    case (c @ CLet(id, _, init), env) => {
      val nInit = init.map(i => rewriteE(i)(env)._1)
      val suf = env.idxMap.toList.sortBy(_._1.v).map(_._2).mkString("_")
      val newName = id.copy(id.v + suf)
      val nEnv = if (suf != "") {
        env.localVarAdd(id)
      } else {
        env
      }
      c.copy(id = newName, e = nInit) -> nEnv.rewriteAdd(id, newName)
    }
    // Handle views
    case (v @ CView(id, arrId, dims), env) => {
      val nDims = env
        .dimsGet(arrId)
        .zip(dims.map(_.shrink))
        .map({
          case ((len, bank), shrink) =>
            (len, shrink.map(sh => bank / sh).getOrElse(bank))
        })
      val nEnv = env
        .dimsAdd(id, nDims)
        .viewAdd(id, ViewTransformer.fromView(env.dimsGet(arrId), v))
      (CEmpty, nEnv)
    }
    case (c @ CFor(range, _, par, combine), env) => {
      if (range.u == 1) {
        val (npar, env1) = env.withScopeAndRet(rewriteC(par)(_))
        val (ncomb, env2) = rewriteC(combine)(env1)
        c.copy(par = npar, combine = ncomb) -> env2
      } else if (range.u > 1 && range.s != 0) {
        throw NotImplemented("Unrolling loops with non-zero start idx", c.pos)
      } else {
        // We need to compile A --- B --- C into
        // {A0; A1 ... --- B0; B1 ... --- C0; C1}
        val nested = par match {
          case CSeq(cmds) => cmds
          case _ => Seq(par)
        }

        val nPar = {
          val seqOfSeqs = (0 to range.u - 1).map(idx => {
            rewriteCSeq(nested)(env.add(range.iter, idx))._1
          })
          CSeq(seqOfSeqs.transpose.map(CPar.smart(_)))
        }

        // Run rewrite just to collect all the local variables
        val locals = rewriteC(par)(env)._2.localVars
        val nComb = rewriteC(combine)(locals.foldLeft(env)({
          case (env, l) => {
            val regs = (0 to range.u - 1).map(i => {
              val suf = ((range.iter, i) :: env.idxMap.toList)
                .sortBy(_._1.v)
                .map(_._2)
                .mkString("_")
              Id(l.v + suf)
            })
            env.combineRegAdd(l, regs.toSet)
          }
        }))._1

        val nRange = range.copy(e = range.e / range.u, u = 1).copy()

        // Refuse lowering without explicit type on iterator.
        c.copy(range = nRange, par = nPar, combine = nComb) -> env
      }
    }
    case (c @ CReduce(rop, _, r), env) => {
      val nR = r match {
        case EVar(rId) =>
          env
            .combineRegGet(rId)
            .map(ids => {
              import Syntax.{OpConstructor => OC}
              val binop = rop.op match {
                case "+=" => NumOp("+", OC.add)
                case "-=" => NumOp("-", OC.sub)
                case "*=" => NumOp("*", OC.mul)
                case "/=" => NumOp("/", OC.div)
                case op => throw Impossible(s"Unknown reduction operator: $op")
              }
              val localsArr = ids.toArray
              val init = EVar(localsArr(0))
              ids.foldLeft[Expr](init)({
                case (l, r) => EBinop(binop, l, EVar(r))
              })
            })
            .getOrElse(r)
        case _ =>
          throw NotImplemented(
            "LowerUnroll: Reduce with complex RHS expression"
          )
      }
      c.copy(rhs = nR) -> env
    }
    case (c @ CUpdate(lhs, _), env) =>
      lhs match {
        case e @ EVar(id) =>
          c.copy(lhs = env.rewriteGet(id).map(nId => EVar(nId)).getOrElse(e)) -> env
        case EArrAccess(id, idxs) => {
          val transformer = env.viewGet(id)
          if (transformer.isDefined) {
            val allExprs =
              (transformer.get.t)(idxs.zip(getBanks(id, idxs)(env)))
            // Calculate the value for all indices and let-bind them.
            val (names, calcIndices) = idxs
              .map(idx => {
                val name = genName("_idx")
                (Id(name), CLet(Id(name), None, Some(idx)))
              })
              .unzip
            // Generate conditional assignment tree
            val condAssign = allExprs.foldLeft[Command](CEmpty)({
              case (cmd, (bankVals, accExpr)) => {
                // TODO(rachit): % with the bank number.
                val cond = names
                  .zip(bankVals)
                  .foldLeft[Expr](EBool(true))({
                    case (expr, (n, bv)) =>
                      EBinop(
                        BoolOp("&&"),
                        EBinop(EqOp("=="), EVar(n) % EInt(1), EInt(bv)),
                        expr
                      )
                  })
                CIf(cond, c.copy(lhs = accExpr), cmd)
              }
            })
            CSeq.smart(calcIndices :+ condAssign) -> env
          } else {
            c -> env
          }
        }
        case _ => throw Impossible("Not an LHS")
      }
  }

  def myRewriteE: PF[(Expr, Env), (Expr, Env)] = {
    case (e @ EVar(id), env) => {
      env.rewriteGet(id).map(nId => EVar(nId)).getOrElse(e) -> env
    }
    case (EArrAccess(id, idxs), env) => {
      val banks: Seq[Int] = idxs.map(idx =>
        idx match {
          case EVar(id) => env.get(id).getOrElse(0)
          case _ => 0
        }
      )
      val arrName = id.v + banks.mkString("_")
      EArrAccess(Id(arrName), idxs) -> env
    }
  }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)

  override def rewriteE(expr: Expr)(implicit env: Env) =
    mergeRewriteE(myRewriteE)(expr, env)

}
