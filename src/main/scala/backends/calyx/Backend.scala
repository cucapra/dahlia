package fuselang.backend.calyx

import scala.math.{BigDecimal, BigInt}

import fuselang.backend.calyx.Calyx._
import fuselang.Utils._
import fuselang.common._
import Syntax._
import Configuration._
import CompilerError._
import fuselang.common.{Configuration => C}

import Helpers._

/**
  *  Helper class that gives names to the fields of the output of `emitExpr` and
  *  `emitBinop`.
  *  - `port` holds either an input or output port that represents how data
  *    flows between expressions.
  *  - `done` holds the port that signals when the writing or reading from `port`
  *    is done.
  *  - `structure` represents additional structure involved in computing the
  *    expression.
  *  - `delay` is the static delay required to complete the structure within
  *    the emitted output.
  *  - `multiCycleInfo` is the variable and delay of the of the op that requires
  *    multiple cycles to complete. This is necessary for the case when a
  *    `write_en` signal should not be high until the op is `done`. If this is
  *    None, then the emitted output has no multi-cycle ops.
  */
private case class EmitOutput(
    // The port that contains the output from the operation
    val port: Port,
    // The done condition returned by the operation
    val done: Option[Port],
    // The structure defined by the operation
    val structure: List[Structure],
    // The statically known delay of the operation, if any
    val delay: Option[Int],
    // TODO(rachit): ???
    val multiCycleInfo: Option[(Port, Option[Int])]
)

/**
  * CALLING CONVENTION:
  * The backend supports functions using Calyx's component definitions.
  * For a function:
  * ```
  * def id(x: ubit<32>): ubit<32> = {
  *   let out: ubit<32> = x;
  *   return out;
  * }
  * ```
  * The function generates a port named unique (`out`).
  * Values returned by the method are carried on this port.
  *
  * Calls are transformed into `invoke` statements in Calyx. Uses of the
  * returned value from the function are assumed to available after the
  * `invoke` statement.
  *
  * The `out` port is marked using the "stable" attributed which is verified
  * by the Calyx compiler to enable such uses:
  * https://github.com/cucapra/Calyx/issues/304
  */
private class CalyxBackendHelper {

  /** A list of function IDs that require width arguments
    * in their SystemVerilog module definition.
    */
  val requiresWidthArguments = List("fp_sqrt", "sqrt")

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

  /** A Calyx variable will either be a
    * local variable (LocalVar) or
    * a function parameter (ParameterVar). */
  sealed trait VType
  case object LocalVar extends VType
  case object ParameterVar extends VType

  /** Store mappings from Dahlia variables to
    * generated Calyx variables. */
  type Store = Map[CompVar, (CompVar, VType)]

  /** Mappings from Function Id to Function Definition. */
  type FunctionMapping = Map[Id, FuncDef]

  def emitArrayDecl(
      arr: TArray,
      id: Id,
      attrs: List[(String, Int)] = List()
  ): Cell = {
    // No support for multi-ported memories or banked memories.
    assertOrThrow(
      arr.ports == 1,
      NotImplemented("Multi-ported memories.")
    )
    assertOrThrow(
      arr.dims.forall(_._2 == 1),
      Impossible(
        "Banked memories should be lowered. Did you pass the `--lower` flag to the compiler?."
      )
    )
    val dims = arr.dims.length
    assertOrThrow(
      dims <= 4,
      NotImplemented(
        s"Calyx standard library does not support ${dims}-dimensional memories"
      )
    )

    val (width, _) = bitsForType(Some(arr.typ), arr.typ.pos)
    val name = CompVar(s"${id}")

    // Memory primitives are defined as:
    // std_mem_d$n(WIDTH, DIM_1_SIZE, .., DIM_$n_SIZE, IDX_1_SIZE, IDX_$n_SIZE)
    val dimSizes = arr.dims.map(_._1)
    val idxSizes = dimSizes.map(bitsNeeded)
    val mem_type = "seq_mem_d"
    Cell(
      name,
      CompInst(
        s"${mem_type}${dims}",
        ((width +: dimSizes) ++ idxSizes).toList.map(i => BigInt(i))
      ),
      false,
      attrs
    )
  }

  /** Returns the name of `p`, if it has one. */
  def getPortName(p: Port): String = {
    p match {
      case CompPort(id, _) => id.name
      case ThisPort(id) => id.name
      case HolePort(id, _) => id.name
      case ConstantPort(_, _) =>
        throw Impossible("Constant Ports do not have names.")
    }
  }

  /** Returns a list of tuples (name, width) for each address port
        in a memory. For example, a D1 Memory declared as (32, 1, 1)
        would return List[("addr0", 1)].
    */
  def getAddrPortToWidths(typ: TArray, id: Id): List[(String, BigInt)] = {
    // Emit the array to determine the port widths.
    val Cell(_, CompInst(_, arrayArgs), _, _) = emitArrayDecl(typ, id)

    // A memory's arguments follow a similar pattern:
    // (bitwidth, size0, ..., sizeX, addr0, ..., addrX),
    // where X is the number of dimensions - 1.
    val dims =
      arrayArgs.length match {
        case 3 => 1
        case 5 => 2
        case 7 => 3
        case 9 => 4
        case _ => throw NotImplemented(s"Arrays of dimension > 4.")
      }
    val addressIndices = (dims + 1 to dims << 1).toList

    addressIndices.zipWithIndex.map({
      case (n: Int, i: Int) => (s"addr${i}", arrayArgs(n))
    })
  }

  /** Returns the width argument(s) of a given function, based on the return
    * type of the function. This is necessary because some components may
    * require certain module parameters in SystemVerilog. For example,
    * `foo` with SystemVerilog module definition:
    * ```
    *   module foo #(
    *     parameter WIDTH
    *   ) ( ... );
    * ```
    * Requires that a `WIDTH` be provided. Currently, the functions that
    * do require these parameters must be manually added to the list
    * `requiresWidthArguments`. */
  def getCompInstArgs(
      funcId: Id
  )(implicit id2FuncDef: FunctionMapping): List[BigInt] = {
    val id = funcId.toString()
    if !requiresWidthArguments.contains(id) then {
      List()
    } else {
      val typ = id2FuncDef(funcId).retTy;
      typ match {
        case TSizedInt(width, _) => List(width)
        case TFixed(width, intWidth, _) =>
          List(width, intWidth, width - intWidth)
        case _ => throw Impossible(s"Type: $typ for $id is not supported.")
      }
    }
  }

  /** `emitInvokeDecl` computes the necessary structure and control for Syntax.EApp. */
  def emitInvokeDecl(app: EApp)(
      implicit store: Store,
      id2FuncDef: FunctionMapping
  ): (Cell, Seq[Structure], Control) = {
    val functionName = app.func.toString()
    val declName = genName(functionName)
    val compInstArgs = getCompInstArgs(app.func)
    // Define a new cell for the function instance
    val decl =
      Cell(declName, CompInst(functionName, compInstArgs), false, List())

    // Compile all arguments to the function and get the relevant ports
    val (argPorts, argSt) = app.args
      .map(inp => {
        val out = emitExpr(inp)
        (out.port, out.structure)
      })
      .unzip

    val (refCells, inConnects) = id2FuncDef(app.func).args
      .zip(argPorts)
      .partitionMap({
        case (param, v) =>
          param.typ match {
            case _: TArray => {
              Left((param.id.v, CompVar(getPortName(v))))
            }
            case _ => Right((param.id.v, v))
          }
      })

    (
      decl,
      argSt.flatten,
      Invoke(declName, refCells.toList, inConnects.toList, List()).withPos(app)
    )
  }

  /** `emitDecl(d)` computes the structure that is needed to
    *  represent the declaration `d`. Simply returns a `List[Structure]`.
    */
  def emitDecl(d: Decl): Structure = d.typ match {
    case tarr: TArray => emitArrayDecl(tarr, d.id, List("external" -> 1))
    case _: TBool => Stdlib.register(CompVar(s"${d.id}"), 1)
    case TSizedInt(size, _) => Stdlib.register(CompVar(s"${d.id}"), size)
    case TFixed(ltotal, _, _) => Stdlib.register(CompVar(s"${d.id}"), ltotal)
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
            "The widths of the left and right side of a binary operation didn't match." +
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
        val isSigned = signed(e1.typ)
        val binOp = Stdlib.binop(s"$compName", e1Bits, isSigned)
        val comp = Cell(genName(compName), binOp, false, List())
        val struct = List(
          comp,
          Assign(e1Out.port, comp.name.port("left")),
          Assign(e2Out.port, comp.name.port("right"))
        )
        EmitOutput(
          comp.name.port("out"),
          None,
          struct ++ e1Out.structure ++ e2Out.structure,
          for d1 <- e1Out.delay; d2 <- e2Out.delay
            yield d1 + d2,
          None
        )
      }
      // if there is additional information about the integer bit,
      // use fixed point binary operation
      case (e1Bits, Some(intBit1)) => {
        val (e2Bits, Some(intBit2)) = bitsForType(e2.typ, e2.pos) : @unchecked
        val fracBit1 = e1Bits - intBit1
        val fracBit2 = e2Bits - intBit2
        val isSigned = signed(e1.typ)
        assertOrThrow(
          fracBit1 == fracBit2 && intBit1 == intBit2,
          NotImplemented(
            "Different-width fixed point not implemented. " +
              "Please open an issue if this is blocking."
          )
        )
        val binOp =
          Stdlib.fixed_point_binop(
            s"$compName",
            e1Bits,
            intBit1,
            fracBit1,
            isSigned
          );
        val comp = Cell(genName(compName), binOp, false, List())
        val struct = List(
          comp,
          Assign(e1Out.port, comp.name.port("left")),
          Assign(e2Out.port, comp.name.port("right"))
        )
        EmitOutput(
          comp.name.port("out"),
          None,
          struct ++ e1Out.structure ++ e2Out.structure,
          for d1 <- e1Out.delay; d2 <- e2Out.delay
            yield d1 + d2,
          None
        )
      }
    }
  }

  def emitMultiCycleBinop(
      compName: String,
      e1: Expr,
      e2: Expr,
      outPort: String,
      delay: Option[Int]
  )(
      implicit store: Store
  ): EmitOutput = {
    val e1Out = emitExpr(e1)
    val e2Out = emitExpr(e2)
    val (e1Bits, e1Int) = bitsForType(e1.typ, e1.pos)
    val (e2Bits, e2Int) = bitsForType(e2.typ, e2.pos)
    // Check if we can compile this expression.
    (e1Int, e2Int) match {
      case (Some(intBit1), Some(intBit2)) => {
        assertOrThrow(
          intBit1 == intBit2,
          NotImplemented(
            "Multiplication between different int-bitwidth fixed points"
          )
        )
      }
      case (None, None) => {
        assertOrThrow(
          e1Bits == e2Bits,
          Impossible(
            "The widths of the left and right side of a binary operation didn't match." +
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
    val binOp = e1.typ match {
      case Some(TFixed(width, intWidth, unsigned)) =>
        Stdlib.fixed_point_binop(
          s"$compName",
          width,
          intWidth,
          width - intWidth,
          !unsigned
        )
      case Some(TSizedInt(width, unsigned)) =>
        Stdlib.binop(s"$compName", width, !unsigned)
      case _ =>
        throw NotImplemented(s"Multi-cycle binary operation with type: $e1.typ")
    }
    val compVar = genName(compName)
    val comp = Cell(compVar, binOp, false, List())
    val struct = List(
      comp,
      Assign(e1Out.port, comp.name.port("left")),
      Assign(e2Out.port, comp.name.port("right")),
      Assign(
        ConstantPort(1, 1),
        comp.name.port("go"),
        Not(Atom(comp.name.port("done")))
      )
    )
    EmitOutput(
      comp.name.port(outPort),
      Some(comp.name.port("done")),
      struct ++ e1Out.structure ++ e2Out.structure,
      for d1 <- e1Out.delay; d2 <- e2Out.delay; d3 <- delay
        yield d1 + d2 + d3,
      Some((comp.name.port("done"), delay))
    )
  }

  /** `emitExpr(expr, rhsInfo)(implicit store)` calculates the necessary structure
    *  to compute `expr`.
    *  - If rhsInfo is defined then this expression is an LHS. rhsInfo contains
    *    (done, delay) information for the RHS being written to this LHS.
    *  - Otherwise, this is an RHS expression.
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
          case "%" => "div_pipe"
          case "&&" => "and"
          case "||" => "or"
          case "&" => "and"
          case "|" => "or"
          case ">>" => "rsh"
          case "<<" => "lsh"
          case "^" => "xor"
          case x =>
            throw NotImplemented(
              s"Calyx backend does not support '$x' yet.",
              op.pos
            )
        }
        op.op match {
          case "*" =>
            emitMultiCycleBinop(
              compName,
              e1,
              e2,
              "out",
              Stdlib.staticTimingMap.get("mult")
            )
          case "/" =>
            emitMultiCycleBinop(
              compName,
              e1,
              e2,
              "out_quotient",
              None
            )
          case "%" =>
            emitMultiCycleBinop(
              compName,
              e1,
              e2,
              "out_remainder",
              None
            )
          case _ => emitBinop(compName, e1, e2)
        }
      }
      case EVar(id) =>
        val (cell, calyxVarType) = store
          .get(CompVar(s"$id"))
          .getOrThrow(BackendError(s"`$id' was not in store", expr.pos))

        val (struct, port, done, delay) =
          rhsInfo match {
            case Some((port, delay)) =>
              (
                List(Assign(port, cell.port("write_en"))),
                "in",
                Some(cell.port("done")),
                delay.map(_ + 1) /* writing takes one cycle */
              )
            case None =>
              (
                List(),
                "out",
                None,
                Some(0) /* reading from a register is combinational */
              )
          }

        EmitOutput(
          if calyxVarType == LocalVar then cell.port(port)
          else ThisPort(cell),
          done,
          struct,
          delay,
          None
        )
      // Integers don't need adaptors
      case ECast(EInt(v, _), typ) => {
        val _ = rhsInfo
        val (typ_b, _) = bitsForType(Some(typ), expr.pos)
        val const =
          Cell(
            genName("const"),
            Stdlib.constant(typ_b, v),
            false,
            List()
          )
        EmitOutput(
          const.name.port("out"),
          None,
          List(const),
          Some(0),
          None
        )
      }
      case EBool(v) => {
        val const =
          Cell(
            genName("bool"),
            Stdlib.constant(1, if v then 1 else 0),
            false,
            List()
          )
        EmitOutput(
          const.name.port("out"),
          None,
          List(const),
          Some(0),
          None
        )
      }
      // Cast ERational to Fixed Point.
      case ECast(ERational(value), typ) => {
        val _ = rhsInfo
        val (width, Some(intWidth)) = bitsForType(Some(typ), expr.pos) : @unchecked
        val fracWidth = width - intWidth
        // Interpret as an integer.
        val isNegative = value.startsWith("-")
        val partition = value.split('.')
        val sIntPart = partition(0)
        val intPart = if isNegative then {
          sIntPart.substring(1, sIntPart.length())
        } else {
          sIntPart
        }
        val bdFracPart = BigDecimal("0." + partition(1))
        val fracValue = (bdFracPart * BigDecimal(2).pow(fracWidth))
        if !fracValue.isWhole then {
          throw BackendError(
            s"The value $value of type $typ is not representable in fixed point",
            expr.pos
          )
        }

        val intBits = binaryString(intPart.toInt, intWidth)
        val fracBits = binaryString(fracValue.toBigInt, fracWidth)
        val bits = if isNegative then {
          negateTwosComplement(intBits + fracBits)
        } else {
          intBits + fracBits
        }
        val fpconst =
          Cell(
            genName("fp_const"),
            Stdlib.constant(width, BigInt(bits, 2)),
            false,
            List()
          )
        EmitOutput(
          fpconst.name.port("out"),
          None,
          List(fpconst),
          Some(0),
          None
        )
      }
      case ECast(e, t) => {
        val (vBits, _) = bitsForType(e.typ, e.pos)
        val (cBits, _) = bitsForType(Some(t), e.pos)
        val res = emitExpr(e)
        if vBits == cBits then {
          // No slicing or padding is necessary.
          EmitOutput(
            res.port,
            None,
            res.structure,
            Some(0),
            res.multiCycleInfo
          )
        } else {
          val comp = if cBits > vBits then {
            Cell(genName("pad"), Stdlib.pad(vBits, cBits), false, List())
          } else {
            Cell(genName("slice"), Stdlib.slice(vBits, cBits), false, List())
          }
          val struct = List(
            comp,
            Assign(res.port, comp.name.port("in"))
          )
          EmitOutput(
            comp.name.port("out"),
            None,
            struct ++ res.structure,
            Some(0),
            res.multiCycleInfo
          )
        }
      }
      case EArrAccess(id, accessors) => {
        val (arr, typ) = store
          .get(CompVar(s"$id"))
          .getOrThrow(
            BackendError(
              s"No array named `${id}' in the current store",
              expr.pos
            )
          )

        // The value is generated on `read_data` and written on `write_data`.
        val portName =
          if rhsInfo.isDefined then "write_data" else "read_data"

        // The array ports change if the array is a function parameter. We want to access the
        // component ports, e.g. `x_read_data`, rather than the memory ports, `x.read_data`.
        val isParam = (typ == ParameterVar)

        val (writeEnPort, donePort, accessPort, contentEnPort) =
          if isParam then {
            (
              ThisPort(CompVar(s"${id}_write_en")),
              ThisPort(CompVar(s"${id}_done")),
              ThisPort(CompVar(s"${id}_${portName}")),
              ThisPort(CompVar(s"${id}_content_en"))
            )
          } else {
            (
              arr.port("write_en"),
              arr.port("done"),
              arr.port(portName),
              arr.port("content_en")
            )
          }

        // We always need to specify and address on the `addr` ports. Generate
        // the additional structure.
        val indexing = accessors.zipWithIndex.foldLeft(List[Structure]())({
          case (structs, (accessor, idx)) => {
            val result = emitExpr(accessor)
            val addrPort =
              if isParam then ThisPort(CompVar(s"${id}_addr${idx}"))
              else arr.port("addr" + idx)
            val con = Assign(result.port, addrPort)
            con :: result.structure ++ structs
          }
        })

        // set ContentEn to 1'd1
        val contentEnStruct =  List(Assign(ConstantPort(1,1), contentEnPort))

        // Set write_en to 1'd0 for reads, to port for writes.
        val writeEnStruct =
          rhsInfo match {
            case Some((port, _)) => List(Assign(port, writeEnPort))
            case None => List(Assign(ConstantPort(1,0), writeEnPort))
          }

        val delay = (rhsInfo) match {
          case (None) => Some(1)
          case (Some((_, delay))) => delay.map(_ + 1)
        }

        EmitOutput(
          accessPort,
          Some(donePort),
          contentEnStruct ++ (indexing ++ (if rhsInfo.isDefined then writeEnStruct else List())),
          delay,
          Some((donePort, delay))
        )
      }
      case EApp(functionId, _) =>
        throw NotImplemented(
          s"`$functionId` should be assigned to its own `let` statement, "
            + s"e.g. `let _temp = $functionId(...);`",
          functionId.pos
        )
      case e: ERational => {
        val pretty = Pretty.emitExpr(e)(false).pretty
        throw BackendError(
          s"Expression `${pretty}` inferred to be an `ERational` which cannot be emitted. If you intended this to be a fixed-point constant, use a cast expression: ($pretty as fix<64, 32>) ",
          e.pos
        )
      }
      case x =>
        throw NotImplemented(s"Calyx backend does not support $x yet.", x.pos)
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
          List(emitArrayDecl(tarr, id, List())),
          Empty,
          store + (arr -> (arr, LocalVar))
        )
      }
      case CLet(_, Some(_: TArray), Some(_)) =>
        throw NotImplemented(s"Calyx backend cannot initialize memories", c.pos)
      case CLet(id, typ, Some(app: EApp)) => {
        // Generate the invocation to run this function.
        val (invokeDecl, argSt, invokeControl) = emitInvokeDecl(app)
        val (typ_b, _) = bitsForType(typ, c.pos)

        // Generate a group that reads the value from the function and saves it
        // into a register.
        val reg = Stdlib.register(genName(s"$id"), typ_b)
        val groupName = genName("let")
        val doneHole = Assign(reg.name.port("done"), HolePort(groupName, "done"))

        val struct =
          List(
            Assign(invokeDecl.name.port("out"), reg.name.port("in")),
            Assign(ConstantPort(1, 1), reg.name.port("write_en")),
            doneHole
          )
        val (group, st) = Group.fromStructure(groupName, struct, None, false)
        val control = SeqComp(
          List(
            invokeControl,
            Enable(group.id).withPos(c)
          )
        )
        (
          argSt.toList ++ (invokeDecl :: reg :: group :: st),
          control,
          store + (CompVar(s"$id") -> (reg.name, LocalVar))
        )
      }
      // If not clearly specified, Cast the TRational to TFixed.
      case CLet(id, Some(TFixed(t, i, un)), Some(e)) => {
        val reg = Stdlib.register(genName(s"$id"), t)
        val out = emitExpr(ECast(e, TFixed(t, i, un)))(store)
        val groupName = genName("let")
        val doneHole = Assign(
          reg.name.port("done"),
          HolePort(groupName, "done")
        )
        // The write enable signal should not be high until
        // the multi-cycle operation is complete, if it exists.
        val (writeEnableSrcPort, delay) = out.multiCycleInfo match {
          case Some((port, Some(delay))) =>
            (Some(port), out.delay.map(_ + delay + 1))
          case Some((port, None)) =>
            (Some(port), None)
          case None => (out.done, out.delay.map(_ + 1))
        }
        val struct =
          Assign(out.port, reg.name.port("in")) :: Assign(
            writeEnableSrcPort.getOrElse(ConstantPort(1, 1)),
            reg.name.port("write_en")
          ) :: doneHole :: out.structure
        val (group, st) =
          Group.fromStructure(groupName, struct, delay, false)
        (
          reg :: group :: st,
          Enable(group.id).withPos(c),
          store + (CompVar(s"$id") -> (reg.name, LocalVar))
        )
      }
      case CLet(id, typ, Some(e)) => {
        val (typ_b, _) = bitsForType(typ, c.pos)
        val reg =
          Stdlib.register(genName(s"$id"), typ_b)
        // XXX(rachit): Why is multiCycleInfo ignored here?
        val EmitOutput(port, done, structure, delay, _) = emitExpr(e)(store)
        val groupName = genName("let")
        val doneHole = Assign(
          reg.name.port("done"),
          HolePort(groupName, "done")
        )
        val struct =
          Assign(port, reg.name.port("in")) :: Assign(
            done.getOrElse(ConstantPort(1, 1)),
            reg.name.port("write_en")
          ) :: doneHole :: structure
        val (group, st) =
          Group.fromStructure(groupName, struct, delay.map(_ + 1), false)
        (
          reg :: group :: st,
          Enable(group.id).withPos(c),
          store + (CompVar(s"$id") -> (reg.name, LocalVar))
        )
      }
      // A `let` that just defines a register for future use.
      case CLet(id, typ, None) => {
        val (typ_b, _) = bitsForType(typ, c.pos)
        val reg =
          Stdlib.register(genName(s"$id"), typ_b)
        val struct = List(reg)
        (struct, Empty, store + (CompVar(s"$id") -> (reg.name, LocalVar)))
      }
      case CExpr(app: EApp) => {
        val (invokeDecl, argSt, invokeControl) = emitInvokeDecl(app)
        (
          invokeDecl :: argSt.toList,
          SeqComp(List(invokeControl)),
          store
        )
      }
      case CUpdate(lhs, rhs) => {
        val rOut = emitExpr(rhs)(store)
        val lOut = emitExpr(
          lhs,
          Some((rOut.done.getOrElse(ConstantPort(1, 1)), rOut.delay))
        )(store)
        val groupName = genName("upd")

        assertOrThrow(
          lOut.done.isDefined,
          BackendError(
            "Compiling expression generated combination done condition"
          )
        )

        // The group is done when the left register commits the write.
        val doneHole =
          Assign(
            lOut.done.get,
            HolePort(groupName, "done")
          )
        // can assign guard as true for lOut.port = rOut.port since the write_en
        // signal is what actually determines what is written into
        val struct =
          lOut.structure ++ rOut.structure ++ List(
            Assign(
              rOut.port,
              lOut.port,
              True
            ),
            doneHole
          )

        val (group, other_st) =
          Group.fromStructure(groupName, struct, lOut.delay, false)
        (group :: other_st, Enable(group.id).withPos(c), store)
      }
      case CIf(cond, tbranch, fbranch) => {
        val condOut = emitExpr(cond)
        val (tStruct, tCon, _) = emitCmd(tbranch)
        val (fStruct, fCon, _) = emitCmd(fbranch)
        val struct = tStruct ++ fStruct
        val groupName = genName("cond")

        // If the conditional computation is not combinational, generate a group.
        condOut.done match {
          case Some(done) => {
            val doneAssign = Assign(done, HolePort(groupName, "done"))
            val (group, st) =
              Group.fromStructure(
                groupName,
                condOut.structure :+ doneAssign,
                condOut.delay,
                false
              )
            val control = SeqComp(
              List(Enable(group.id), If(condOut.port, group.id, tCon, fCon))
            )
            (group :: st ++ struct, control, store)
          }
          case None => {
            val (group, st) =
              Group.fromStructure(
                groupName,
                condOut.structure,
                condOut.delay,
                true
              )
            val control = If(condOut.port, group.id, tCon, fCon)
            (group :: st ++ struct, control, store)
          }
        }
      }
      case CEmpty => (List(), Empty, store)
      case wh @ CWhile(cond, _, body) => {
        val condOut = emitExpr(cond)
        val groupName = genName("cond")
        assertOrThrow(
          !condOut.done.isDefined,
          BackendError("Loop condition is non-combinational")
        )
        val (condGroup, condDefs) =
          Group.fromStructure(
            groupName,
            condOut.structure,
            condOut.delay,
            true
          )
        val (bodyStruct, bodyCon, st) = emitCmd(body)
        val control = While(condOut.port, condGroup.id, bodyCon)
        control.attributes = wh.attributes
        (condGroup :: bodyStruct ++ condDefs, control, st)
      }
      case _: CFor =>
        throw BackendError(
          "for loops cannot be directly generated. Use the `--lower` flag to turn them into while loops."
        )
      case c @ CReduce(rop, e1, e2) => {
        // rewrite statements of the form
        //   lhs += rhs
        // to
        //   lhs = lhs + rhs
        val (op, numOp) =
          rop.toString match {
            case "+=" => ("+", (x: Double, y: Double) => x + y)
            case "*=" => ("*", (x: Double, y: Double) => x * y)
            case _ =>
              throw NotImplemented(
                s"Calyx backend does not support $rop yet",
                c.pos
              )
          }

        e1 match {
          case _: EVar =>
            emitCmd(CUpdate(e1, EBinop(NumOp(op, numOp), e1, e2)))
          case ea: EArrAccess => {
            // Create a binding for the memory read.
            val name = Id(genName("red_read").name)
            val bind = CLet(name, ea.typ, Some(ea))
            val nLhs = EVar(name)
            nLhs.typ = ea.typ;
            val upd = CUpdate(e1, EBinop(NumOp(op, numOp), nLhs, e2))
            emitCmd(CSeq.smart(Seq(bind, upd)))
          }
          case e =>
            throw Impossible(
              s"LHS is neither a variable nor a memory access: ${Pretty.emitExpr(e)(false).pretty}"
            )
        }
      }
      case CReturn(expr:EVar) => {
        // Hooks the output port of the emitted `expr` to PortDef `out` of the component.
        val condOut = emitExpr(expr)
        val outPort = ThisPort(CompVar("out"))
        val returnConnect = Assign(condOut.port, outPort)
        (returnConnect :: condOut.structure, Empty, store)
      }
      case CReturn(e) => {
        throw NotImplemented(
          s"Only allowed to return variables. Store the return expression in a variable to return it."
          + s"e.g. `let _tmp = ${Pretty.emitExpr(e)(false).pretty}; return _tmp`",
          e.pos
        )
      }
      case _: CDecorate => (List(), Empty, store)
      case x =>
        throw NotImplemented(s"Calyx backend does not support $x yet", x.pos)
    }
  }

  /** Emits the function definition if a body exists. */
  def emitDefinition(definition: Definition): FuncDef = {
    definition match {
      case fd: FuncDef => fd
      case x =>
        throw NotImplemented(s"Calyx backend does not support $x yet", x.pos)
    }
  }

  def emitProg(p: Prog, c: Config): String = {

    implicit val meta = Metadata()

    val importDefinitions = p.includes.flatMap(_.defs).toList
    val definitions =
      p.defs.map(definition => emitDefinition(definition)) ++ importDefinitions

    // Build a mapping from names to function definitions
    val id2FuncDef =
      definitions.foldLeft(Map[Id, FuncDef]())((definitions, defn) =>
        defn match {
          case FuncDef(id, params, retTy, bodyOpt) => {
            definitions + (id -> FuncDef(id, params, retTy, bodyOpt))
          }
//          case _: RecordDef => definitions
        }
      )

    val functionDefinitions: List[Component] =
      for ( case (id, FuncDef(_, params, retType, Some(body))) <- id2FuncDef.toList )
        yield {
          val (refCells, inputPorts) = params.partitionMap(param =>
            param.typ match {
              case tarr: TArray => {
                Left(emitArrayDecl(tarr, param.id).copy(ref = true))
              }
              case _ => {
                val (bits, _) = bitsForType(Some(param.typ), param.typ.pos)
                Right(PortDef(CompVar(param.id.toString()), bits))
              }
            }
          )

          val functionStore = params.foldLeft(Map[CompVar, (CompVar, VType)]())(
            (functionStore, param) => {
              val id = CompVar(param.id.v)
              val pv = param.typ match {
                case _: TArray => LocalVar
                case _ => ParameterVar
              }
              functionStore + (id -> (id, pv))
            }
          )
          val (cmdStructure, controls, _) =
            emitCmd(body)(functionStore, id2FuncDef)

          val (outputBitWidth, _) = bitsForType(Some(retType), retType.pos)

          Component(
            id.toString(),
            inputPorts.toList,
            if retType == TVoid() then List()
            // If the return type of the component is not void, add an `out` wire.
            else
              List(
                PortDef(CompVar("out"), outputBitWidth, List(("stable" -> 1)))
              ),
            refCells.toList ++ cmdStructure.sorted,
            controls
          )
        }

    val imports =
      Import("primitives/core.futil") ::
      Import("primitives/memories/seq.futil") ::
        Import("primitives/binary_operators.futil") ::
        p.includes.flatMap(_.backends.get(C.Calyx)).map(i => Import(i)).toList

    val main = if !c.compilerOpts.contains("no-main") then {
      val declStruct = p.decls.map(emitDecl)
      val store =
        declStruct.foldLeft(Map[CompVar, (CompVar, VType)]())((store, struct) =>
          struct match {
            case Cell(id, _, _, _) => store + (id -> (id, LocalVar))
            case _ => store
          }
        )
      val (cmdStruct, control, _) = emitCmd(p.cmd)(store, id2FuncDef)

      val struct = declStruct.toList ++ cmdStruct
      val mainComponentName =
        if c.kernelName == "kernel" then "main" else c.kernelName
      List(
        Component(mainComponentName, List(), List(), struct.sorted, control)
      )
    } else {
      List()
    }

    // Emit the program
    (PrettyPrint.Doc
      .vsep(
        (imports ++ functionDefinitions ++ main)
          .map(_.doc(meta))
      ) <@> meta.doc()).pretty
  }
}

case object CalyxBackend extends fuselang.backend.Backend {
  def emitProg(p: Prog, c: Config) = {
    (new CalyxBackendHelper()).emitProg(p, c)
  }
  val canGenerateHeader = false
  override val commentPrefix: String = "//"
}
