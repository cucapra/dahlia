package fuselang.backend.futil

import scala.math.max
import scala.util.parsing.input.{Position}

import fuselang.backend.futil.Futil._
import fuselang.Utils._
import fuselang.common._
import Syntax._
import Configuration._
import CompilerError._
import fuselang.common.{Configuration => C}

/**
  *  Helper class that gives names to the fields of the output of `emitExpr` and
  *  `emitBinop`.
  *  - `port` holds either an input or output port that represents how data
  *    flows between exprs
  *  - `done` holds the port that signals when the writing or reading from `port`
  *     is done
  *  - `structure` represents additional structure involved in computing the
  *    expression.
  */
private case class EmitOutput(
    val port: Port,
    val done: Port,
    val structure: List[Structure],
    val delay: Option[Int]
)

private class FutilBackendHelper {

  /** Helper for generating unique names. */
  var idx: Map[String, Int] = Map();
  def genName(base: String): CompVar = {
    // update idx
    idx get base match {
      case Some(n) => idx = idx + (base -> (n + 1))
      case None => idx = idx + (base -> 0)
    }
    CompVar(s"$base${idx(base)}")
  }

  /** Extracts the bits needed from an optional type annotation.
    *  Returns (total size, Option[integral]) bits for the computation.
    */
  def bitsForType(t: Option[Type], pos: Position): (Int, Option[Int]) = {
    t match {
      case Some(TSizedInt(width, _)) => (width, None)
      case Some(TFixed(t, i, _)) => (t, Some(i))
      case Some(_: TBool) => (1, None)
      case Some(_: TVoid) => (0, None)
      case x =>
        throw NotImplemented(
          s"Futil cannot infer bitwidth for type $x. Please manually annotate it using a cast expression.",
          pos
        )
    }
  }

  /** Returns true if the given int or fixed point is signed
    */
  def signed(typ: Option[Type]) = {
    typ match {
      case Some(TSizedInt(_, un)) => un == false
      case Some(TFixed(_, _, un)) => un == false
      case _ => false
    }
  }

  /** A Futil variable will either be a
    * local variable (LocalVar) or
    * a function parameter (ParameterVar). */
  sealed trait VType
  case object LocalVar extends VType
  case object ParameterVar extends VType

  /** Store mappings from Dahlia variables to
    * generated Futil variables. */
  type Store = Map[CompVar, (CompVar, VType)]

  /** Mappings from Function Id to Function Definition. */
  type FunctionMapping = Map[Id, FuncDef]

  /** `external` is a flag that differentiates between generating
    *  external memories and internal memories. This is so that
    *  we can generate external memories for `decl`s and internal
    *  memories for local arrays. */
  def emitArrayDecl(typ: TArray, id: Id, external: Boolean): List[Structure] = {
    // No support for multi-ported memories or banked memories.
    assertOrThrow(
      typ.ports == 1,
      NotImplemented("Multi-ported memories.")
    )
    assertOrThrow(
      typ.dims.forall(_._2 == 1),
      Impossible(
        "Banked memories should be lowered. Did you pass the `--lower` flag to the compiler?."
      )
    )

    val width = typ.typ match {
      case _: TBool => 1
      case TSizedInt(size, _) => {
        size
      }
      case TFixed(size, _, _) => {
        size
      }
      case x => throw NotImplemented(s"Arrays of $x")
    }
    val name = CompVar(s"${id}")

    val mem = typ.dims.length match {
      case 1 => {
        val size = typ.dims(0)._1
        val idxSize = bitsNeeded(size)
        LibDecl(name, Stdlib.mem_d1(width, size, idxSize), external)
      }
      case 2 => {
        val size0 = typ.dims(0)._1
        val size1 = typ.dims(1)._1
        val idxSize0 = bitsNeeded(size0)
        val idxSize1 = bitsNeeded(size1)
        LibDecl(
          name,
          Stdlib.mem_d2(width, size0, size1, idxSize0, idxSize1),
          external
        )
      }
      case 3 => {
        val size0 = typ.dims(0)._1
        val size1 = typ.dims(1)._1
        val size2 = typ.dims(2)._1
        val idxSize0 = bitsNeeded(size0)
        val idxSize1 = bitsNeeded(size1)
        val idxSize2 = bitsNeeded(size2)
        LibDecl(
          name,
          Stdlib
            .mem_d3(width, size0, size1, size2, idxSize0, idxSize1, idxSize2),
          external
        )
      }
      case 4 => {
        val size0 = typ.dims(0)._1
        val size1 = typ.dims(1)._1
        val size2 = typ.dims(2)._1
        val size3 = typ.dims(3)._1
        val idxSize0 = bitsNeeded(size0)
        val idxSize1 = bitsNeeded(size1)
        val idxSize2 = bitsNeeded(size2)
        val idxSize3 = bitsNeeded(size3)
        LibDecl(
          name,
          Stdlib
            .mem_d4(
              width,
              size0,
              size1,
              size2,
              size3,
              idxSize0,
              idxSize1,
              idxSize2,
              idxSize3
            ),
          external
        )
      }
      case n => throw NotImplemented(s"Arrays of size $n")
    }
    List(mem)
  }

  /** `emitDecl(d)` computes the structure that is needed to
    *  represent the declaration `d`. Simply returns a `List[Structure]`.
    */
  def emitDecl(d: Decl): List[Structure] = d.typ match {
    case tarr: TArray => emitArrayDecl(tarr, d.id, true)
    case _: TBool => {
      val reg = LibDecl(CompVar(s"${d.id}"), Stdlib.register(1), false)
      List(reg)
    }
    case TSizedInt(size, _) => {
      val reg = LibDecl(CompVar(s"${d.id}"), Stdlib.register(size), false)
      List(reg)
    }
    case TFixed(ltotal, _, _) => {
      val reg = LibDecl(CompVar(s"${d.id}"), Stdlib.register(ltotal), false)
      List(reg)
    }
    case x => throw NotImplemented(s"Type $x not implemented for decls.", x.pos)
  }

  /** `emitBinop` is a helper function to generate the structure
    *  for `e1 binop e2`. The return type is described in `emitExpr`.
    */
  def emitBinop(compName: String, e1: Expr, e2: Expr)(
      implicit store: Store
  ): EmitOutput = {
    val e1Out = emitExpr(e1)
    val e2Out = emitExpr(e2)
    val (e1Bits, e1Int) = bitsForType(e1.typ, e1.pos)
    val (e2Bits, e2Int) = bitsForType(e2.typ, e2.pos)
    // Throw error on numeric or bitwidth mismatch.
    (e1Int, e2Int) match {
      case (Some(_), Some(_)) => { /* Fixed-points allow this */ }
      case (None, None) => {
        assertOrThrow(
          e1Bits == e2Bits,
          Impossible(
            "The widths of the left and right side of a binop didn't match." +
              s"\nleft: ${Pretty.emitExpr(e1)(false).pretty}: ${e1Bits}" +
              s"\nright: ${Pretty.emitExpr(e2)(false).pretty}: ${e2Bits}"
          )
        )
      }
      case _ => {
        throw Impossible(
          "Cannot perform mixed arithmetic between fixed-point and non-fixed-point numbers" +
            s"\nleft: ${Pretty.emitExpr(e1)(false).pretty}" +
            s"\nright: ${Pretty.emitExpr(e2)(false).pretty}"
        )
      }
    }

    bitsForType(e1.typ, e1.pos) match {
      case (e1Bits, None) => {
        val binop =
          if (signed(e1.typ)) Stdlib.s_op(s"$compName", e1Bits)
          else Stdlib.op(s"$compName", e1Bits)
        val comp = LibDecl(genName(compName), binop, false)
        val struct = List(
          comp,
          Connect(e1Out.port, comp.id.port("left")),
          Connect(e2Out.port, comp.id.port("right"))
        )
        EmitOutput(
          comp.id.port("out"),
          ConstantPort(1, 1),
          struct ++ e1Out.structure ++ e2Out.structure,
          for (d1 <- e1Out.delay; d2 <- e2Out.delay)
            yield d1 + d2
        )
      }
      // if there is additional information about the integer bit,
      // use fixed point binary operation
      case (e1Bits, Some(intBit1)) => {
        val (e2Bits, Some(intBit2)) = bitsForType(e2.typ, e2.pos)
        val fracBit1 = e1Bits - intBit1
        val fracBit2 = e2Bits - intBit2
        val outBit = max(intBit1, intBit2) + max(fracBit1, fracBit2)
        val binop =
          if (fracBit1 != fracBit2) {
            assertOrThrow(
              compName == "add",
              NotImplemented("Signed diffwidth computation other than addition")
            )

            val prim =
              if (signed(e1.typ)) Stdlib.sdiff_width_add _
              else Stdlib.diff_width_add _

            prim(
              e1Bits,
              e2Bits,
              intBit1,
              fracBit1,
              intBit2,
              fracBit2,
              outBit
            )
          } else if (signed(e1.typ)) {
            Stdlib.fxd_p_sop(s"$compName", e1Bits, intBit1, fracBit1);
          } else {
            Stdlib.fxd_p_op(s"$compName", e1Bits, intBit1, fracBit1);
          }
        val comp = LibDecl(genName(compName), binop, false)
        val struct = List(
          comp,
          Connect(e1Out.port, comp.id.port("left")),
          Connect(e2Out.port, comp.id.port("right"))
        )
        EmitOutput(
          comp.id.port("out"),
          ConstantPort(1, 1),
          struct ++ e1Out.structure ++ e2Out.structure,
          for (d1 <- e1Out.delay; d2 <- e2Out.delay)
            yield d1 + d2
        )
      }
    }
  }

  def emitMultiCycleBinop(
      compName: String,
      e1: Expr,
      e2: Expr,
      delay: Option[Int]
  )(
      implicit store: Store
  ): EmitOutput = {
    val e1Out = emitExpr(e1)
    val e2Out = emitExpr(e2)
    val (e1Bits, e1Int) = bitsForType(e1.typ, e1.pos)
    val (e2Bits, e2Int) = bitsForType(e2.typ, e2.pos)
    (e1Int, e2Int) match {
      case (Some(intBit1), Some(intBit2)) => {
        assertOrThrow(
          intBit1 == intBit2,
          NotImplemented(
            "Multiplication between different int-bitwith fixed points"
          )
        )
      }
      case (None, None) => {
        assertOrThrow(
          e1Bits == e2Bits,
          Impossible(
            "The widths of the left and right side of a binop didn't match." +
              s"\nleft: ${Pretty.emitExpr(e1)(false).pretty}: ${e1Bits}" +
              s"\nright: ${Pretty.emitExpr(e2)(false).pretty}: ${e2Bits}"
          )
        )
      }
      case _ => {
        throw Impossible(
          "Cannot perform mixed arithmetic between fixed-point and non-fixed-point numbers" +
            s"\nleft: ${Pretty.emitExpr(e1)(false).pretty}" +
            s"\nright: ${Pretty.emitExpr(e2)(false).pretty}"
        )
      }
    }
    val (typ_b, _) = bitsForType(e1.typ, e1.pos)
    val binop =
      if (signed(e1.typ)) Stdlib.s_op(s"$compName", typ_b)
      else Stdlib.op(s"$compName", typ_b);

    val comp = LibDecl(genName(compName), binop, false)
    val struct = List(
      comp,
      Connect(e1Out.port, comp.id.port("left")),
      Connect(e2Out.port, comp.id.port("right")),
      Connect(
        ConstantPort(1, 1),
        comp.id.port("go"),
        Some(Not(Atom(comp.id.port("done"))))
      )
    )
    EmitOutput(
      comp.id.port("out"),
      comp.id.port("done"),
      struct ++ e1Out.structure ++ e2Out.structure,
      for (d1 <- e1Out.delay; d2 <- e2Out.delay; d3 <- delay)
        yield d1 + d2 + d3
    )
  }

  /** `emitExpr(expr, rhsInfo)(implicit store)` calculates the necessary structure
    *  to compute `expr`. It return the pair (Port, List[Structure]).
    *  If `rhsInfo = None`, then `Port` is the port that will hold the output
    *  of computing this expression. If `rhsInfo = Some(...)`, then `Port` represents
    *  the port that can be used to put a value into the location represented by
    *  `expr`.
    */
  def emitExpr(expr: Expr, rhsInfo: Option[(Port, Option[Int])] = None)(
      implicit store: Store
  ): EmitOutput =
    expr match {
      case _: EInt => {
        throw PassError(
          "Cannot compile unannotated constants. Wrap constant in `as` expression",
          expr.pos
        )
      }
      case EBinop(op, e1, e2) => {
        val compName = op.op match {
          case "+" => "add"
          case "-" => "sub"
          case "*" => "mult_pipe"
          case "/" => "div_pipe"
          case "<" => "lt"
          case ">" => "gt"
          case "<=" => "le"
          case ">=" => "ge"
          case "!=" => "neq"
          case "==" => "eq"
          case "%" => "mod_pipe"
          case "&&" => "and"
          case "||" => "or"
          case "&" => "and"
          case "|" => "or"
          case ">>" => "rsh"
          case "<<" => "lsh"
          case x =>
            throw NotImplemented(
              s"Futil backend does not support '$x' yet.",
              op.pos
            )
        }
        op.op match {
          case "*" =>
            emitMultiCycleBinop(
              compName,
              e1,
              e2,
              Stdlib.staticTimingMap("mult")
            )
          case "/" =>
            emitMultiCycleBinop(compName, e1, e2, Stdlib.staticTimingMap("div"))
          case "%" =>
            emitMultiCycleBinop(compName, e1, e2, Stdlib.staticTimingMap("mod"))
          case _ => emitBinop(compName, e1, e2)
        }
      }
      case EVar(id) =>
        val portName = if (rhsInfo.isDefined) "in" else "out"
        val (varName, futilVarType) = store
          .get(CompVar(s"$id"))
          .getOrThrow(BackendError(s"`$id' was not in store", expr.pos))
        val struct =
          rhsInfo match {
            case Some((port, _)) =>
              List(Connect(port, varName.port("write_en")))
            case None => List()
          }
        // calculate static delay, rhsDelay + 1 for writes, 0 for reads
        val delay =
          rhsInfo match {
            case Some((_, delay)) => delay.map(_ + 1)
            case None => Some(0)
          }
        EmitOutput(
          if (futilVarType == LocalVar) varName.port(portName)
          else ThisPort(varName),
          if (rhsInfo.isDefined) varName.port("done") else ConstantPort(1, 1),
          struct,
          delay
        )
      // Integers don't need adaptors
      case ECast(EInt(v, _), typ) => {
        val _ = rhsInfo
        val (typ_b, _) = bitsForType(Some(typ), expr.pos)
        val const =
          LibDecl(
            genName("const"),
            Stdlib.constant(typ_b, v),
            false
          )
        EmitOutput(
          const.id.port("out"),
          ConstantPort(1, 1),
          List(const),
          Some(0)
        )
      }
      // Cast ERational to fixed_points
      case ECast(ERational(d), typ) => {
        val _ = rhsInfo
        val (width, Some(int_bit)) = bitsForType(Some(typ), expr.pos)
        val frac_bit = width - int_bit
        val lst = d.split('.')
        val v_1 = lst(0).toInt
        val v_2 = lst(1).toInt
        val fpconst =
          LibDecl(
            genName("fpconst"),
            Stdlib.fixed_point(width, int_bit, frac_bit, v_1, v_2),
            false
          )
        EmitOutput(
          fpconst.id.port("out"),
          ConstantPort(1, 1),
          List(fpconst),
          Some(0)
        )
      }
      case ECast(e, t) => {
        val (vBits, _) = bitsForType(e.typ, e.pos)
        val (cBits, _) = bitsForType(Some(t), e.pos)
        val comp = if (cBits > vBits) {
          LibDecl(genName("pad"), Stdlib.pad(vBits, cBits), false)
        } else {
          LibDecl(genName("slice"), Stdlib.slice(vBits, cBits), false)
        }
        val res = emitExpr(e)
        val struct = List(
          comp,
          Connect(res.port, comp.id.port("in"))
        )

        EmitOutput(
          comp.id.port("out"),
          ConstantPort(1, 1),
          struct ++ res.structure,
          Some(0)
        )
      }
      case EArrAccess(id, accessors) => {
        val (arr, _) = store
          .get(CompVar(s"$id"))
          .getOrThrow(
            BackendError(
              s"No array named `${id}' in the current store",
              expr.pos
            )
          )
        // We always need to specify and address on the `addr` ports. Generate
        // the additional structure.
        val indexing = accessors.zipWithIndex.foldLeft(List[Structure]())({
          case (structs, (accessor, idx)) => {
            val result = emitExpr(accessor)
            val con = Connect(result.port, arr.port("addr" + idx))
            con :: result.structure ++ structs
          }
        })

        // The value is generated on `read_data` and written on `write_data`.
        val portName = if (rhsInfo.isDefined) "write_data" else "read_data"
        val writeEnStruct =
          rhsInfo match {
            case Some((port, _)) => List(Connect(port, arr.port("write_en")))
            case None => List()
          }
        // calculate static delay, rhsDelay + 1 for writes, 0 for reads
        val delay =
          rhsInfo match {
            case Some((_, delay)) => delay.map(_ + 1)
            case None => Some(0)
          }
        EmitOutput(
          arr.port(portName),
          if (rhsInfo.isDefined) arr.port("done") else ConstantPort(1, 1),
          indexing ++ writeEnStruct,
          delay
        )
      }
      case EApp(functionId, _) =>
        throw NotImplemented(
          s"`$functionId` should be assigned to its own `let` statement, "
            + s"e.g. `let _temp = $functionId(...);`",
          functionId.pos
        )
      case x =>
        throw NotImplemented(s"Futil backend does not support $x yet.", x.pos)
    }

  def emitCmd(
      c: Command
  )(
      implicit store: Store,
      id2FuncDef: FunctionMapping
  ): (List[Structure], Control, Store) = {
    c match {
      case CBlock(cmd) => emitCmd(cmd)
      case CPar(cmds) => {
        cmds.foldLeft[(List[Structure], Control, Store)](
          (List[Structure](), Empty, store)
        )({
          case ((struct, con, st), cmd) => {
            val (s1, c1, st1) = emitCmd(cmd)(st, id2FuncDef)
            (struct ++ s1, con.par(c1), st1)
          }
        })
      }
      case CSeq(cmds) => {
        cmds.foldLeft[(List[Structure], Control, Store)](
          (List[Structure](), Empty, store)
        )({
          case ((struct, con, st), cmd) => {
            val (s1, c1, st1) = emitCmd(cmd)(st, id2FuncDef)
            (struct ++ s1, con.seq(c1), st1)
          }
        })
      }
      case CLet(id, Some(tarr: TArray), None) => {
        val arr = CompVar(s"$id")
        (
          emitArrayDecl(tarr, id, false),
          Empty,
          store + (arr -> (arr, LocalVar))
        )
      }
      case CLet(_, Some(_: TArray), Some(_)) =>
        throw NotImplemented(s"Futil backend cannot initialize memories", c.pos)
      // if not clearly specified, Cast the TRational to TFixed
      case CLet(id, Some(TFixed(t, i, un)), Some(e)) => {
        val reg =
          LibDecl(genName(s"$id"), Stdlib.register(t), false)
        val out = emitExpr(ECast(e, TFixed(t, i, un)))(store)
        val groupName = genName("let")
        val doneHole = Connect(
          reg.id.port("done"),
          HolePort(groupName, "done")
        )
        val struct =
          Connect(out.port, reg.id.port("in")) :: Connect(
            out.done,
            reg.id.port("write_en")
          ) :: doneHole :: out.structure
        val (group, st) =
          Group.fromStructure(groupName, struct, out.delay.map(_ + 1))
        (
          reg :: group :: st,
          Enable(group.id),
          store + (CompVar(s"$id") -> (reg.id, LocalVar))
        )
      }
      case CLet(id, typ, Some(EApp(invokeId, inputs))) => {
        val functionName = invokeId.toString()
        val argumentPorts = inputs.map(inp => emitExpr(inp).port)
        val parameters =
          id2FuncDef(invokeId).args.map(decl => CompVar(decl.id.toString()))
        val declName = genName(functionName)

        val decl = if (id2FuncDef(invokeId).bodyOpt == None) {
          // If the function definition does not have a body, it is imported.
          // Also assume that for all Futil imports, they are
          // (1) `invoke`-able, and (2) primitive
          LibDecl(declName, CompInst(functionName, List()), false)
        } else {
          CompDecl(declName, CompVar(functionName))
        }

        val (typ_b, _) = bitsForType(typ, c.pos)
        val reg = LibDecl(genName(s"$id"), Stdlib.register(typ_b), false)

        val groupName = genName("let")
        val doneHole = Connect(reg.id.port("done"), HolePort(groupName, "done"))

        val struct =
          List(
            Connect(declName.port("out"), reg.id.port("in")),
            Connect(ConstantPort(1, 1), reg.id.port("write_en")),
            doneHole
          )

        val (group, st) = Group.fromStructure(groupName, struct, None)
        val control = Invoke(
          declName,
          argumentPorts.toList,
          parameters.toList,
          Enable(group.id)
        )
        (
          decl :: reg :: group :: st,
          control,
          store + (CompVar(s"$id") -> (reg.id, LocalVar))
        )
      }
      case CLet(id, typ, Some(e)) => {
        val (typ_b, _) = bitsForType(typ, c.pos)
        val reg =
          LibDecl(genName(s"$id"), Stdlib.register(typ_b), false)
        val out = emitExpr(e)(store)
        val groupName = genName("let")
        val doneHole = Connect(
          reg.id.port("done"),
          HolePort(groupName, "done")
        )
        val struct =
          Connect(out.port, reg.id.port("in")) :: Connect(
            out.done,
            reg.id.port("write_en")
          ) :: doneHole :: out.structure
        val (group, st) =
          Group.fromStructure(groupName, struct, out.delay.map(_ + 1))
        (
          reg :: group :: st,
          Enable(group.id),
          store + (CompVar(s"$id") -> (reg.id, LocalVar))
        )
      }
      case CLet(id, typ, None) => {
        val (typ_b, _) = bitsForType(typ, c.pos)
        val reg =
          LibDecl(genName(s"$id"), Stdlib.register(typ_b), false)
        val struct = List(reg)
        (struct, Empty, store + (CompVar(s"$id") -> (reg.id, LocalVar)))
      }
      case CUpdate(lhs, rhs) => {
        val rOut = emitExpr(rhs)(store)
        val lOut = emitExpr(lhs, Some((rOut.done, rOut.delay)))(store)
        val groupName = genName("upd")
        val doneHole =
          Connect(
            ConstantPort(1, 1),
            HolePort(groupName, "done"),
            Some(Atom(lOut.done))
          )
        val struct =
          lOut.structure ++ rOut.structure ++ List(
            Connect(rOut.port, lOut.port, Some(Atom(rOut.done))),
            doneHole
          )
        val (group, other_st) =
          Group.fromStructure(groupName, struct, lOut.delay)
        (group :: other_st, Enable(group.id), store)
      }
      case CIf(cond, tbranch, fbranch) => {
        val condOut = emitExpr(cond)
        val (tStruct, tCon, _) = emitCmd(tbranch)
        val (fStruct, fCon, _) = emitCmd(fbranch)
        val struct = tStruct ++ fStruct
        val groupName = genName("cond")
        val doneHole = Connect(condOut.done, HolePort(groupName, "done"))
        val (group, st) =
          Group.fromStructure(
            groupName,
            doneHole :: condOut.structure,
            condOut.delay
          )
        val control = If(condOut.port, group.id, tCon, fCon)
        (group :: st ++ struct, control, store)
      }
      case CEmpty => (List(), Empty, store)
      case CWhile(cond, _, body) => {
        val condOut = emitExpr(cond)
        val groupName = genName("cond")
        val doneHole = Connect(condOut.done, HolePort(groupName, "done"))
        val (condGroup, condDefs) =
          Group.fromStructure(
            groupName,
            doneHole :: condOut.structure,
            condOut.delay
          )
        val (bodyStruct, bodyCon, st) = emitCmd(body)
        val control = While(condOut.port, condGroup.id, bodyCon)
        (condGroup :: bodyStruct ++ condDefs, control, st)
      }
      case _: CFor =>
        throw BackendError(
          "for loops cannot be directly generated. Use the `--lower` flag to turn them into while loops."
        )
      case c @ CReduce(op, e1, e2) => {
        // rewrite statements of the form
        //   lhs += rhs
        // to
        //   lhs = lhs + rhs
        op.toString match {
          case "+=" =>
            emitCmd(CUpdate(e1, EBinop(NumOp("+", _ + _), e1, e2)))
          case "*=" =>
            emitCmd(CUpdate(e1, EBinop(NumOp("*", _ * _), e1, e2)))
          case _ =>
            throw NotImplemented(
              s"Futil backend does not support $op yet",
              c.pos
            )
        }
      }
      case CReturn(expr) => {
        // Hooks the output port of the emitted `expr` to PortDef `out` of the component.
        val condOut = emitExpr(expr)
        val outPort = ThisPort(CompVar("out"))
        val returnConnect = Connect(condOut.port, outPort)
        (returnConnect :: condOut.structure, Empty, store)
      }
      case _: CDecorate => (List(), Empty, store)
      case x =>
        throw NotImplemented(s"Futil backend does not support $x yet", x.pos)
    }
  }

  /** Emits the function definition if a body exists. */
  def emitDefinition(definition: Definition): FuncDef = {
    definition match {
      case FuncDef(id, params, retTy, Some(bodyOpt)) => {
        FuncDef(id, params, retTy, Some(bodyOpt))
      }
      case x =>
        throw NotImplemented(s"Futil backend does not support $x yet", x.pos)
    }
  }

  def getBitWidth(typ: Type): Int = {
    typ match {
      case _: TVoid => 0
      case _: TBool => 1
      case TSizedInt(bitwidth, _) => bitwidth
      case TFixed(bitwidth, _, _) => bitwidth
      case x => throw NotImplemented("Get bitwidth not supported for $x", x.pos)
    }
  }

  def emitProg(p: Prog, c: Config): String = {
    val _ = c

    val imports =
      p.includes.flatMap(_.backends.get(C.Futil)).map(i => Import(i)).toList
    val importDefinitions = p.includes.flatMap(_.defs).toList

    val definitions =
      p.defs.map(definition => emitDefinition(definition)) ++ importDefinitions
    val id2FuncDef =
      definitions.foldLeft(Map[Id, FuncDef]())((definitions, defn) =>
        defn match {
          case FuncDef(id, params, retTy, bodyOpt) => {
            definitions + (id -> FuncDef(id, params, retTy, bodyOpt))
          }
          case _ => definitions
        }
      )

    val declStruct =
      p.decls.map(x => emitDecl(x)).foldLeft(List[Structure]())(_ ++ _)
    val store =
      declStruct.foldLeft(Map[CompVar, (CompVar, VType)]())((store, struct) =>
        struct match {
          case CompDecl(id, _) => store + (id -> (id, LocalVar))
          case LibDecl(id, _, _) => store + (id -> (id, LocalVar))
          case _ => store
        }
      )
    val (cmdStruct, control, _) = emitCmd(p.cmd)(store, id2FuncDef)

    val functionDefinitions: List[Component] =
      for ((id, FuncDef(_, params, retType, Some(bodyOpt))) <- id2FuncDef.toList)
        yield {
          val inputs = params.map(param =>
            param.typ match {
              case TArray(_, _, _) =>
                throw NotImplemented(
                  "Memory as a parameter is not supported yet.",
                  param.pos
                )
              case _ =>
                PortDef(CompVar(param.id.toString()), getBitWidth(param.typ))
            }
          )

          val functionStore = inputs.foldLeft(Map[CompVar, (CompVar, VType)]())(
            (functionStore, inputs) =>
              inputs match {
                case PortDef(id, _) =>
                  functionStore + (id -> (id, ParameterVar))
                case _ => functionStore
              }
          )
          val (cmdStructure, controls, _) =
            emitCmd(bodyOpt)(functionStore, id2FuncDef)

          val outputBitWidth = getBitWidth(retType)
          val output =
            if (outputBitWidth == 0) List()
            else List(PortDef(CompVar("out"), outputBitWidth))

          Component(
            id.toString(),
            inputs.toList,
            output,
            cmdStructure.sorted,
            controls
          )
        }
    val struct = declStruct ++ cmdStruct
    val mainComponentName =
      if (c.kernelName == "kernel") "main" else c.kernelName
    Namespace(
      "prog",
      imports
        ++ functionDefinitions
        ++ List(
          Component(mainComponentName, List(), List(), struct.sorted, control)
        )
    ).emit()
  }
}

case object FutilBackend extends fuselang.backend.Backend {
  def emitProg(p: Prog, c: Config) = {
    (new FutilBackendHelper()).emitProg(p, c)
  }
  val canGenerateHeader = false
  override val commentPrefix: String = "//"
}
