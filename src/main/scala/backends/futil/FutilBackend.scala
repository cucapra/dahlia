package fuselang.backend.futil

import scala.math.{max, BigDecimal}
import scala.util.parsing.input.{Position}

import fuselang.backend.futil.Calyx._
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
    val port: Port,
    val done: Port,
    val structure: List[Structure],
    val delay: Option[Int],
    val multiCycleInfo: Option[(CompVar, Option[Int])]
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
  * https://github.com/cucapra/futil/issues/304
  */
private class FutilBackendHelper {

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

  /** Given a binary string, returns the negated
    * two's complement representation.
    */
  def negateTwosComplement(bitString: String): String = {
    if (bitString.forall(_ == '0')) {
      bitString
    }
    val t = bitString
      .replaceAll("0", "_")
      .replaceAll("1", "0")
      .replaceAll("_", "1")
    (Integer.parseInt(t, 2) + 1).toBinaryString
  }

  /** Given an integer, returns the corresponding
    * zero-padded string of size `width`. */
  def binaryString(value: Int, width: Int): String = {
    val s = value.toBinaryString
    "0" * max(width - s.length(), 0) + s
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
          s"Calyx cannot infer bitwidth for type $x. Please manually annotate it using a cast expression.",
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
    Cell(
      name,
      CompInst(s"std_mem_d${dims}", ((width +: dimSizes) ++ idxSizes).toList),
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
  def getAddrPortToWidths(typ: TArray, id: Id): List[(String, Int)] = {
    // Emit the array to determine the port widths.
    val Cell(_, CompInst(_, arrayArgs), _) = emitArrayDecl(typ, id)

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
      case (n, i) => (s"addr${i}", arrayArgs(n))
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
  )(implicit id2FuncDef: FunctionMapping): List[Int] = {
    val id = funcId.toString()
    if (!requiresWidthArguments.contains(id)) {
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
    val decl =
      Cell(declName, CompInst(functionName, compInstArgs), List())
    val (argPorts, argSt) = app.args
      .map(inp => {
        val out = emitExpr(inp)
        (out.port, out.structure)
      })
      .unzip
    val argToParam = argPorts zip id2FuncDef(app.func).args

    val inConnects = argToParam.foldLeft(List[(String, Port)]())(
      {
        case (inConnects, (inputPort, paramArg)) =>
          paramArg.typ match {
            case _: TArray => {
              val argId = getPortName(inputPort)
              val paramId = paramArg.id.toString()
              inConnects ++ List(
                (
                  s"${paramId}_read_data",
                  CompPort(CompVar(s"$argId"), "read_data")
                ),
                (s"${paramId}_done", CompPort(CompVar(s"$argId"), "done"))
              )
            }
            case _ => inConnects ++ List((paramArg.id.toString(), inputPort))
          }
      }
    )

    val outConnects = argToParam.foldLeft(List[(String, Port)]())(
      {
        case (outConnects, (inputPort, paramArg)) =>
          paramArg.typ match {
            case tarr: TArray => {
              val paramId = paramArg.id.toString()
              val argId = getPortName(inputPort)

              // Connect address ports.
              val addrPortToWidth = getAddrPortToWidths(tarr, paramArg.id)
              val addressPorts =
                addrPortToWidth.map({
                  case (name, _) =>
                    (
                      s"${paramId}_${name}",
                      CompPort(CompVar(s"$argId"), s"$name")
                    )
                })

              outConnects ++ List(
                (
                  s"${paramId}_write_data",
                  CompPort(CompVar(s"$argId"), "write_data")
                ),
                (
                  s"${paramId}_write_en",
                  CompPort(CompVar(s"$argId"), "write_en")
                )
              ) ++ addressPorts
            }
            case _ => outConnects
          }
      }
    )

    (
      decl,
      argSt.flatten,
      Invoke(declName, inConnects, outConnects)
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
        val comp = Cell(genName(compName), binOp, List())
        val struct = List(
          comp,
          Assign(e1Out.port, comp.id.port("left")),
          Assign(e2Out.port, comp.id.port("right"))
        )
        EmitOutput(
          comp.id.port("out"),
          ConstantPort(1, 1),
          struct ++ e1Out.structure ++ e2Out.structure,
          for (d1 <- e1Out.delay; d2 <- e2Out.delay)
            yield d1 + d2,
          None
        )
      }
      // if there is additional information about the integer bit,
      // use fixed point binary operation
      case (e1Bits, Some(intBit1)) => {
        val (e2Bits, Some(intBit2)) = bitsForType(e2.typ, e2.pos)
        val fracBit1 = e1Bits - intBit1
        val fracBit2 = e2Bits - intBit2
        val outBit = max(intBit1, intBit2) + max(fracBit1, fracBit2)
        val isSigned = signed(e1.typ)
        val binOp =
          if (fracBit1 != fracBit2) {
            assertOrThrow(
              compName == "add",
              NotImplemented(
                "Only addition is supported for different-width operators"
              )
            )
            Stdlib.fixed_point_diff_width(
              s"$compName",
              e1Bits,
              e2Bits,
              intBit1,
              fracBit1,
              intBit2,
              fracBit2,
              outBit,
              isSigned
            )
          } else {
            Stdlib.fixed_point_binop(
              s"$compName",
              e1Bits,
              intBit1,
              fracBit1,
              isSigned
            );
          }
        val comp = Cell(genName(compName), binOp, List())
        val struct = List(
          comp,
          Assign(e1Out.port, comp.id.port("left")),
          Assign(e2Out.port, comp.id.port("right"))
        )
        EmitOutput(
          comp.id.port("out"),
          ConstantPort(1, 1),
          struct ++ e1Out.structure ++ e2Out.structure,
          for (d1 <- e1Out.delay; d2 <- e2Out.delay)
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
    val comp = Cell(compVar, binOp, List())
    val struct = List(
      comp,
      Assign(e1Out.port, comp.id.port("left")),
      Assign(e2Out.port, comp.id.port("right")),
      Assign(
        ConstantPort(1, 1),
        comp.id.port("go"),
        Not(Atom(comp.id.port("done")))
      )
    )
    EmitOutput(
      comp.id.port(outPort),
      comp.id.port("done"),
      struct ++ e1Out.structure ++ e2Out.structure,
      for (d1 <- e1Out.delay; d2 <- e2Out.delay; d3 <- delay)
        yield d1 + d2 + d3,
      Some((compVar, delay))
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
          case "%" => "div_pipe"
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
              "out",
              Stdlib.staticTimingMap.get("mult")
            )
          case "/" =>
            emitMultiCycleBinop(
              compName,
              e1,
              e2,
              "out_quotient",
              Stdlib.staticTimingMap.get("div")
            )
          case "%" =>
            emitMultiCycleBinop(
              compName,
              e1,
              e2,
              "out_remainder",
              Stdlib.staticTimingMap.get("mod")
            )
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
              List(Assign(port, varName.port("write_en")))
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
            List()
          )
        EmitOutput(
          const.id.port("out"),
          ConstantPort(1, 1),
          List(const),
          Some(0),
          None
        )
      }
      // Cast ERational to Fixed Point.
      case ECast(ERational(value), typ) => {
        val _ = rhsInfo
        val (width, Some(intWidth)) = bitsForType(Some(typ), expr.pos)
        val fracWidth = width - intWidth
        // Interpret as an integer.
        val isNegative = value.startsWith("-")
        val partition = value.split('.')
        val sIntPart = partition(0)
        val intPart = if (isNegative) {
          sIntPart.substring(1, sIntPart.length())
        } else {
          sIntPart
        }
        val bdFracPart = BigDecimal("0." + partition(1))
        val fracValue = (bdFracPart * BigDecimal(2).pow(fracWidth))
        if (!fracValue.isWhole) {
          throw BackendError(
            s"The value $value of type $typ is not representable in fixed point",
            expr.pos
          )
        }

        val intBits = binaryString(intPart.toInt, intWidth)
        val fracBits = binaryString(fracValue.toIntExact, fracWidth)
        val bits = if (isNegative) {
          negateTwosComplement(intBits + fracBits)
        } else {
          intBits + fracBits
        }

        val fpconst =
          Cell(
            genName("fp_const"),
            Stdlib.constant(width, Integer.parseInt(bits, 2)),
            List()
          )
        EmitOutput(
          fpconst.id.port("out"),
          ConstantPort(1, 1),
          List(fpconst),
          Some(0),
          None
        )
      }
      case ECast(e, t) => {
        val (vBits, _) = bitsForType(e.typ, e.pos)
        val (cBits, _) = bitsForType(Some(t), e.pos)
        val res = emitExpr(e)
        if (vBits == cBits) {
          // No slicing or padding is necessary.
          EmitOutput(
            res.port,
            ConstantPort(1, 1),
            res.structure,
            Some(0),
            res.multiCycleInfo
          )
        } else {
          val comp = if (cBits > vBits) {
            Cell(genName("pad"), Stdlib.pad(vBits, cBits), List())
          } else {
            Cell(genName("slice"), Stdlib.slice(vBits, cBits), List())
          }
          val struct = List(
            comp,
            Assign(res.port, comp.id.port("in"))
          )
          EmitOutput(
            comp.id.port("out"),
            ConstantPort(1, 1),
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
        // The array ports change if the array is a function parameter. We want to access the
        // component ports, e.g. `x_read_data`, rather than the memory ports, `x.read_data`.
        val isParam = (typ == ParameterVar)

        // The value is generated on `read_data` and written on `write_data`.
        val portName = if (rhsInfo.isDefined) "write_data" else "read_data"

        val (writeEnPort, donePort, accessPort) =
          if (isParam) {
            (
              ThisPort(CompVar(s"${id}_write_en")),
              ThisPort(CompVar(s"${id}_done")),
              ThisPort(CompVar(s"${id}_${portName}"))
            )
          } else {
            (
              arr.port("write_en"),
              arr.port("done"),
              arr.port(portName)
            )
          }

        // We always need to specify and address on the `addr` ports. Generate
        // the additional structure.
        val indexing = accessors.zipWithIndex.foldLeft(List[Structure]())({
          case (structs, (accessor, idx)) => {
            val result = emitExpr(accessor)
            val addrPort =
              if (isParam) ThisPort(CompVar(s"${id}_addr${idx}"))
              else arr.port("addr" + idx)
            val con = Assign(result.port, addrPort)
            con :: result.structure ++ structs
          }
        })

        val writeEnStruct =
          rhsInfo match {
            case Some((port, _)) => List(Assign(port, writeEnPort))
            case None => List()
          }
        // calculate static delay, rhsDelay + 1 for writes, 0 for reads
        val delay =
          rhsInfo match {
            case Some((_, delay)) => delay.map(_ + 1)
            case None => Some(0)
          }
        EmitOutput(
          accessPort,
          if (rhsInfo.isDefined) donePort else ConstantPort(1, 1),
          indexing ++ writeEnStruct,
          delay,
          None
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
          List(emitArrayDecl(tarr, id, List())),
          Empty,
          store + (arr -> (arr, LocalVar))
        )
      }
      case CLet(_, Some(_: TArray), Some(_)) =>
        throw NotImplemented(s"Futil backend cannot initialize memories", c.pos)
      case CLet(id, typ, Some(app: EApp)) => {
        val (invokeDecl, argSt, invokeControl) = emitInvokeDecl(app)

        val (typ_b, _) = bitsForType(typ, c.pos)
        val reg = Stdlib.register(genName(s"$id"), typ_b)

        val groupName = genName("let")
        val doneHole = Assign(reg.id.port("done"), HolePort(groupName, "done"))

        val struct =
          List(
            Assign(invokeDecl.id.port("out"), reg.id.port("in")),
            Assign(ConstantPort(1, 1), reg.id.port("write_en")),
            doneHole
          )
        val (group, st) = Group.fromStructure(groupName, struct, None)
        val control = SeqComp(
          List(
            invokeControl,
            Enable(group.id)
          )
        )
        (
          argSt.toList ++ (invokeDecl :: reg :: group :: st),
          control,
          store + (CompVar(s"$id") -> (reg.id, LocalVar))
        )
      }
      // If not clearly specified, Cast the TRational to TFixed.
      case CLet(id, Some(TFixed(t, i, un)), Some(e)) => {
        val reg = Stdlib.register(genName(s"$id"), t)
        val out = emitExpr(ECast(e, TFixed(t, i, un)))(store)
        val groupName = genName("let")
        val doneHole = Assign(
          reg.id.port("done"),
          HolePort(groupName, "done")
        )
        // The write enable signal should not be high until
        // the multi-cycle operation is complete, if it exists.
        val (writeEnableSrcPort, delay) = out.multiCycleInfo match {
          case Some((compVar, Some(delay))) =>
            (CompPort(compVar, "done"), out.delay.map(_ + delay + 1))
          case Some((compVar, None)) =>
            (CompPort(compVar, "done"), None)
          case None => (out.done, out.delay.map(_ + 1))
        }
        val struct =
          Assign(out.port, reg.id.port("in")) :: Assign(
            writeEnableSrcPort,
            reg.id.port("write_en")
          ) :: doneHole :: out.structure
        val (group, st) =
          Group.fromStructure(groupName, struct, delay)
        (
          reg :: group :: st,
          Enable(group.id),
          store + (CompVar(s"$id") -> (reg.id, LocalVar))
        )
      }
      case CExpr(app: EApp) => {
        val (invokeDecl, argSt, invokeControl) = emitInvokeDecl(app)
        (
          invokeDecl :: argSt.toList,
          SeqComp(List(invokeControl)),
          store
        )
      }
      case CLet(id, typ, Some(e)) => {
        val (typ_b, _) = bitsForType(typ, c.pos)
        val reg =
          Stdlib.register(genName(s"$id"), typ_b)
        val out = emitExpr(e)(store)
        val groupName = genName("let")
        val doneHole = Assign(
          reg.id.port("done"),
          HolePort(groupName, "done")
        )
        val struct =
          Assign(out.port, reg.id.port("in")) :: Assign(
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
          Stdlib.register(genName(s"$id"), typ_b)
        val struct = List(reg)
        (struct, Empty, store + (CompVar(s"$id") -> (reg.id, LocalVar)))
      }
      case CUpdate(lhs, rhs) => {
        val rOut = emitExpr(rhs)(store)
        val lOut = emitExpr(lhs, Some((rOut.done, rOut.delay)))(store)
        val groupName = genName("upd")
        val doneHole =
          Assign(
            ConstantPort(1, 1),
            HolePort(groupName, "done"),
            Atom(lOut.done)
          )
        val struct =
          lOut.structure ++ rOut.structure ++ List(
            Assign(rOut.port, lOut.port, Atom(rOut.done)),
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
        val doneHole = Assign(condOut.done, HolePort(groupName, "done"))
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
        val doneHole = Assign(condOut.done, HolePort(groupName, "done"))
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
        val returnConnect = Assign(condOut.port, outPort)
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
      case fd: FuncDef => fd
      case x =>
        throw NotImplemented(s"Futil backend does not support $x yet", x.pos)
    }
  }

  def emitProg(p: Prog, c: Config): String = {

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

    val declStruct = p.decls.map(x => emitDecl(x))
    val store =
      declStruct.foldLeft(Map[CompVar, (CompVar, VType)]())((store, struct) =>
        struct match {
          case Cell(id, _, _) => store + (id -> (id, LocalVar))
          case _ => store
        }
      )
    val (cmdStruct, control, _) = emitCmd(p.cmd)(store, id2FuncDef)

    val functionDefinitions: List[Component] =
      for ((id, FuncDef(_, params, retType, Some(bodyOpt))) <- id2FuncDef.toList)
        yield {
          val inputs = params.map(param => CompVar(param.id.toString()))

          val inputPorts =
            params.foldLeft(List[PortDef]())((inputPorts, params) =>
              params.typ match {
                case tarr: TArray => {
                  // Currently, we default to exposing all ports for arrays.
                  val (bits, _) = bitsForType(Some(tarr.typ), tarr.typ.pos)
                  val id = params.id.toString()
                  inputPorts ++ List(
                    PortDef(CompVar(s"${id}_read_data"), bits),
                    PortDef(CompVar(s"${id}_done"), 1)
                  )
                }
                case _ => {
                  val (bits, _) = bitsForType(Some(params.typ), params.typ.pos)
                  inputPorts ++ List(
                    PortDef(CompVar(params.id.toString()), bits)
                  )
                }
              }
            )

          val outputPorts =
            params.foldLeft(List[PortDef]())((outputPorts, params) =>
              params.typ match {
                case tarr: TArray => {
                  val (bits, _) = bitsForType(Some(tarr.typ), tarr.typ.pos)
                  val id = params.id.toString()
                  val addrPortToWidth = getAddrPortToWidths(tarr, params.id)
                  val addressPortDefs = addrPortToWidth.map(
                    {
                      case (name, idxSize) =>
                        PortDef(CompVar(s"${id}_${name}"), idxSize)
                    }
                  )

                  outputPorts ++ List(
                    PortDef(CompVar(s"${id}_write_data"), bits),
                    PortDef(CompVar(s"${id}_write_en"), 1)
                  ) ++ addressPortDefs
                }
                case _ => outputPorts
              }
            )

          val functionStore = inputs.foldLeft(Map[CompVar, (CompVar, VType)]())(
            (functionStore, inputs) =>
              inputs match {
                case id: CompVar =>
                  functionStore + (id -> (id, ParameterVar))
              }
          )
          val (cmdStructure, controls, _) =
            emitCmd(bodyOpt)(functionStore, id2FuncDef)

          val (outputBitWidth, _) = bitsForType(Some(retType), retType.pos)

          Component(
            id.toString(),
            inputPorts,
            if (retType == TVoid()) outputPorts
            // If the return type of the component is not void, add an `out` wire.
            else outputPorts ++ List(PortDef(CompVar("out"), outputBitWidth)),
            cmdStructure.sorted,
            controls
          )
        }
    val imports =
      Import("primitives/std.lib") ::
        p.includes.flatMap(_.backends.get(C.Futil)).map(i => Import(i)).toList

    val struct = declStruct.toList ++ cmdStruct
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
