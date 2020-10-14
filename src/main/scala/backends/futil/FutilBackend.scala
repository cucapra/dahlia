package fuselang.backend.futil

import scala.util.parsing.input.{Position}

import fuselang.backend.futil.Futil._
import fuselang.Utils._
import fuselang.common._
import Syntax._
import Configuration._
import CompilerError._

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

 // Helper to differentiate operations between fixedpoints and other types.
 def isfixedpoint(ty: Option[Type]) : Boolean ={
   ty match {
     case Some(TFixed(_, _, _)) => true 
     case _ => false
   }
 }
  /** Extracts the bits needed from an optional type annotation. */
  // Q - extend fixedpints ?
  def bitsForType(t: Option[Type], pos: Position): (Int, Option[Int]) = {
    t match {
      case Some(TSizedInt(width, _)) => (width, None)
      case Some(TFixed(t, i, _)) => (t, Some(i)) // my change
      // case Some(_: TRational) =>
      case Some(_: TBool) => (1, None)
      case Some(_: TVoid) => (0, None)
      case x =>
        throw NotImplemented(
          s"Futil cannot infer bitwidth for type $x. Please manually annotate it using a cast expression.",
          pos
        )
    }
  }

  /** Store mappings from Dahlia variables to
    * generated Futil variables.
    */
  type Store = Map[CompVar, CompVar]

  /** `external` is a flag that differentiates between generating
    *  external memories and internal memories. This is so that
    *  we can generate external memories for `decl`s and internal
    *  memories for local arrays. */
  def emitArrayDecl(typ: TArray, id: Id, external: Boolean): List[Structure] = {
    // No support for multi-ported memories or banked memories.
    assertOrThrow(
      typ.ports == 1,
      NotImplemented("Emitting multi-ported memories.")
    )
    assertOrThrow(
      typ.dims.forall(_._2 == 1),
      NotImplemented("Banked memories.")
    )

    val width = typ.typ match {
      case _: TBool => 1
      case TSizedInt(size, unsigned) => {
        assert(unsigned, NotImplemented("Arrays of signed integers."))
        size
      }
      //later expand fixd point
      case x => throw NotImplemented(s"Arrays of $x")
    }
    val name = CompVar(s"${id}")

    val mem = typ.dims.length match {
      case 1 => {
        val size = typ.dims(0)._1
        val idxSize = bitsNeeded(size)
        if (external) {
          LibDecl(name, Stdlib.mem_d1_ext(width, size, idxSize))
        } else {
          LibDecl(name, Stdlib.mem_d1(width, size, idxSize))
        }
      }
      case 2 => {
        val size0 = typ.dims(0)._1
        val size1 = typ.dims(1)._1
        val idxSize0 = bitsNeeded(size0)
        val idxSize1 = bitsNeeded(size1)
        if (external) {
          LibDecl(
            name,
            Stdlib.mem_d2_ext(width, size0, size1, idxSize0, idxSize1)
          )
        } else {
          LibDecl(name, Stdlib.mem_d2(width, size0, size1, idxSize0, idxSize1))
        }
      }
      case 3 => {
        val size0 = typ.dims(0)._1
        val size1 = typ.dims(1)._1
        val size2 = typ.dims(2)._1
        val idxSize0 = bitsNeeded(size0)
        val idxSize1 = bitsNeeded(size1)
        val idxSize2 = bitsNeeded(size2)
        if (external) {
          LibDecl(
            name,
            Stdlib
              .mem_d3_ext(
                width,
                size0,
                size1,
                size2,
                idxSize0,
                idxSize1,
                idxSize2
              )
          )
        } else {
          LibDecl(
            name,
            Stdlib
              .mem_d3(width, size0, size1, size2, idxSize0, idxSize1, idxSize2)
          )
        }
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
      val reg = LibDecl(CompVar(s"${d.id}"), Stdlib.register(1))
      List(reg)
    }
    case TSizedInt(size, unsigned) => {
      assert(unsigned, NotImplemented("Generating signed integers", d.pos))
      val reg = LibDecl(CompVar(s"${d.id}"), Stdlib.register(size))
      List(reg)
    }
    case TFixed(ltotal, _ , unsigned)=>{
      assert(unsigned, NotImplemented("Generating signed fixed points", d.pos))
      val reg = LibDecl(CompVar(s"${d.id}"), Stdlib.register(ltotal))
      List(reg)
    } // my change 
    case x => throw NotImplemented(s"Type $x not implemented for decls.", x.pos)
  }

  /** `emitBinopF` is a helper function to generate the structure
    *  for `e1 binop e2`. when the given type is fixed points The return type is described in `emitExpr`.
    */
  def emitBinopF(compName: String, e1: Expr, e2: Expr)(
      implicit store: Store
  ): EmitOutput = {
    val e1Out = emitExpr(e1)
    val e2Out = emitExpr(e2)
    val (e1Bits, Some(int_bit1) ) = bitsForType(e1.typ, e1.pos)
    val (e2Bits, Some(_) ) = bitsForType(e2.typ, e2.pos)
    assertOrThrow( // later extend this to support different bit addtion
      e1Bits == e2Bits,
      Impossible(
        "The widths of the left and right side of a binop didn't match." +
          s"\nleft: ${Pretty.emitExpr(e1)(false).pretty}: ${e1Bits}" +
          s"\nright: ${Pretty.emitExpr(e2)(false).pretty}: ${e2Bits}"
      )
    )
    val frac_bit = e1Bits - int_bit1
    val binop = Stdlib.fxd_p_op(s"$compName", e1Bits, int_bit1, frac_bit); // do Stdlib.fxd_p_op instead of Stdlib.op

    val comp = LibDecl(genName(compName), binop)
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

  def emitBinop(compName: String, e1: Expr, e2: Expr)(
      implicit store: Store
  ): EmitOutput = {
    val e1Out = emitExpr(e1)
    val e2Out = emitExpr(e2)
    val (e1Bits, _ ) = bitsForType(e1.typ, e1.pos)
    val (e2Bits, _ ) = bitsForType(e2.typ, e2.pos)
    assertOrThrow(
      e1Bits == e2Bits,
      Impossible(
        "The widths of the left and right side of a binop didn't match." +
          s"\nleft: ${Pretty.emitExpr(e1)(false).pretty}: ${e1Bits}" +
          s"\nright: ${Pretty.emitExpr(e2)(false).pretty}: ${e2Bits}"
      )
    )
    val (typ_b, _) = bitsForType(e1.typ, e1.pos)
    val binop = Stdlib.op(s"$compName", typ_b);

    val comp = LibDecl(genName(compName), binop)
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
    val (e1Bits, _) = bitsForType(e1.typ, e1.pos)
    val (e2Bits, _) = bitsForType(e2.typ, e2.pos)
    assertOrThrow(
      e1Bits == e2Bits,
      Impossible(
        "The widths of the left and right side of a binop didn't match." +
          s"\nleft: ${Pretty.emitExpr(e1)(false).pretty}: ${e1Bits}" +
          s"\nright: ${Pretty.emitExpr(e2)(false).pretty}: ${e2Bits}"
      )
    )
    val (typ_b, _) = bitsForType(e1.typ, e1.pos)
    val binop = Stdlib.op(s"$compName", typ_b);

    val comp = LibDecl(genName(compName), binop)
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
        // if the typ of e1 is  fixed points do fixed point binary operation 
        if (isfixedpoint(e1.typ)) {
          val compName =op.op match {
            case "+" => "add"
            case "-" => "sub"
            case "*" => "mult"
            case "/" => "div"
            case x =>
              throw NotImplemented(
                s"Futil backend does not support '$x' yet.",
                op.pos
              )
          }
          op.op match {
          case _ => emitBinopF(compName, e1, e2)
        }
        } else {
          val compName =op.op match {
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
      }
      case EVar(id) =>
        val portName = if (rhsInfo.isDefined) "in" else "out"
        val varName = store
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
          varName.port(portName),
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
            Stdlib.constant(typ_b, v)
          )
        EmitOutput(
          const.id.port("out"),
          ConstantPort(1, 1),
          List(const),
          Some(0)
        )
      }
      // Cast ERational to fixed_points 
      case ECast(ERational(d), typ) =>{
        val _ = rhsInfo
        val (width, Some(int_bit)) = bitsForType(Some(typ), expr.pos)
        val frac_bit = width - int_bit
        val lst = d.split('.')
        val v_1 = lst(0).toInt
        val v_2 = lst(1).toInt
        val fpconst = 
        LibDecl(
             genName("fpconst"),
             Stdlib.fixed_point( width, int_bit, frac_bit, v_1, v_2) // use fixed_point primitive 
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
        if (cBits > vBits) {
          throw NotImplemented(
            "Cast expressions that imply zero-padding",
            expr.pos
          )
        }
        val res = emitExpr(e)
        val sliceOp =
          Stdlib.slice(vBits, cBits)
        val comp = LibDecl(genName("slice"), sliceOp)
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
        val arr = store
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
      case EApp(Id("sqrt"), List(arg)) => {
        val argOut = emitExpr(arg)
        val sqrt = LibDecl(genName("sqrt"), Stdlib.sqrt())
        val struct = List(
          sqrt,
          Connect(argOut.port, sqrt.id.port("in")),
          Connect(
            ConstantPort(1, 1),
            sqrt.id.port("go"),
            Some(Not(Atom(sqrt.id.port("done"))))
          )
        )
        EmitOutput(
          sqrt.id.port("out"),
          sqrt.id.port("done"),
          argOut.structure ++ struct,
          Stdlib.staticTimingMap("sqrt")
        )
      }
      case x =>
        throw NotImplemented(s"Futil backend does not support $x yet.", x.pos)
    }

  def emitCmd(
      c: Command
  )(implicit store: Store): (List[Structure], Control, Store) = {
    c match {
      case CBlock(cmd) => emitCmd(cmd)
      case CPar(cmds) => {
        cmds.foldLeft[(List[Structure], Control, Store)](
          (List[Structure](), Empty, store)
        )({
          case ((struct, con, st), cmd) => {
            val (s1, c1, st1) = emitCmd(cmd)(st)
            (struct ++ s1, con.par(c1), st1)
          }
        })
      }
      case CSeq(cmds) => {
        cmds.foldLeft[(List[Structure], Control, Store)](
          (List[Structure](), Empty, store)
        )({
          case ((struct, con, st), cmd) => {
            val (s1, c1, st1) = emitCmd(cmd)(st)
            (struct ++ s1, con.seq(c1), st1)
          }
        })
      }
      case CLet(id, Some(tarr: TArray), None) => {
        val arr = CompVar(s"$id")
        (emitArrayDecl(tarr, id, false), Empty, store + (arr -> arr))
      }
      case CLet(_, Some(_: TArray), Some(_)) =>
        throw NotImplemented(s"Futil backend cannot initialize memories", c.pos)
      // if not clearly specified, Cast the TRational to TFixed  
      case CLet(id, Some(TFixed(t, i, un)), Some(e)) =>
      { 
        val reg =
          LibDecl(genName(s"$id"), Stdlib.register(t))
        val out = emitExpr(ECast(e,TFixed(t, i, un)))(store)
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
          store + (CompVar(s"$id") -> reg.id)
        )
      }
      case CLet(id, typ, Some(e)) => {
        val (typ_b, _) = bitsForType(typ, c.pos) 
        val reg =
          LibDecl(genName(s"$id"), Stdlib.register(typ_b))
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
          store + (CompVar(s"$id") -> reg.id)
        )
      }
      case CLet(id, typ, None) => {
        val (typ_b, _) = bitsForType(typ, c.pos)
        val reg =
          LibDecl(genName(s"$id"), Stdlib.register(typ_b))
        val struct = List(reg)
        (struct, Empty, store + (CompVar(s"$id") -> reg.id))
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
          "for loops cannot be directly generated. Use the --lower flag to turn them into while loops."
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
      case _: CDecorate => (List(), Empty, store)
      case x =>
        throw NotImplemented(s"Futil backend does not support $x yet", x.pos)
    }
  }

  def emitProg(p: Prog, c: Config): String = {
    val _ = c
    val declStruct =
      p.decls.map(x => emitDecl(x)).foldLeft(List[Structure]())(_ ++ _)
    val store = declStruct.foldLeft(Map[CompVar, CompVar]())((store, struct) =>
      struct match {
        case CompDecl(id, _) => store + (id -> id)
        case LibDecl(id, _) => store + (id -> id)
        case _ => store
      }
    )
    val (cmdStruct, control, _) = emitCmd(p.cmd)(store)
    val struct = declStruct ++ cmdStruct
    Namespace(
      "prog",
      List(
        Import("primitives/std.lib"),
        Component("main", List(), List(), struct.sorted, control)
      )
    ).emit
  }
}

case object FutilBackend extends fuselang.backend.Backend {
  def emitProg(p: Prog, c: Config) = {
    (new FutilBackendHelper()).emitProg(p, c)
  }
  val canGenerateHeader = false
  override val commentPrefix: String = "//"
}
