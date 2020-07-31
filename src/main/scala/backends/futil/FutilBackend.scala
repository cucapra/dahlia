package fuselang.backend.futil

import scala.util.parsing.input.{Position}

import fuselang.backend.futil.Futil._
import fuselang.Utils._
import fuselang.common._
import Syntax._
import Configuration._
import CompilerError._

/** Helper class that gives names to the fields of the output of `emitExpr` and `emitBinop`.
  *  - `port` holds either an input or output port that represents how data flows between exprs
  *  - `done` holds the port that signals when the writing or reading from `port` is done
  *  - `structure` represents additional structure involved in computing the expression
  */
private case class EmitOutput(
    val port: Port,
    val done: Port,
    val structure: List[Structure]
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

  /** Extracts the bits needed from an optional type annotation. */
  def bitsForType(t: Option[Type], pos: Position): Int = {
    t match {
      case Some(TSizedInt(width, _)) => width
      case Some(TBool()) => 1
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
    assertOrThrow(
      bitsForType(e1.typ, e1.pos) == bitsForType(e2.typ, e2.pos),
      Impossible(
        "The widths of the left and right side of a binop didn't match."
      )
    )
    val binop = Stdlib.op(s"$compName", bitsForType(e1.typ, e1.pos));

    val comp = LibDecl(genName(compName), binop)
    val struct = List(
      comp,
      Connect(e1Out.port, comp.id.port("left")),
      Connect(e2Out.port, comp.id.port("right"))
    )
    EmitOutput(
      comp.id.port("out"),
      ConstantPort(1, 1),
      struct ++ e1Out.structure ++ e2Out.structure
    )
  }

  /** `emitExpr(expr, lhs)(implicit store)` calculates the necessary structure
    *  to compute `expr`. It return the pair (Port, List[Structure]).
    *  If `lhs = false`, then `Port` is the port that will hold the output
    *  of computing this expression. If `lhs = true`, then `Port` represents
    *  the port that can be used to put a value into the location represented by
    *  `expr`.
    */
  def emitExpr(expr: Expr, lhs: Boolean = false)(
      implicit store: Store
  ): EmitOutput =
    expr match {
      case EInt(v, _) => {
        val _ = lhs
        val const =
          LibDecl(
            genName("const"),
            Stdlib.constant(bitsForType(expr.typ, expr.pos), v)
          )
        EmitOutput(const.id.port("out"), ConstantPort(1, 1), List(const))
      }
      case EBinop(op, e1, e2) => {
        val compName =
          op.op match {
            case "+" => "add"
            case "-" => "sub"
            case "*" => "mult"
            case "/" => "div"
            case "<" => "lt"
            case ">" => "gt"
            case "<=" => "le"
            case ">=" => "ge"
            case "!=" => "neq"
            case "==" => "eq"
            case "%" => "mod"
            case x =>
              throw NotImplemented(
                s"Futil backend does not support '$x' yet.",
                op.pos
              )
          }
        emitBinop(compName, e1, e2)
      }
      case EVar(id) =>
        val portName = if (lhs) "in" else "out"
        val varName = store
          .get(CompVar(s"$id"))
          .getOrThrow(Impossible(s"$id was not in `store`"))
        val struct =
          if (lhs) List(Connect(ConstantPort(1, 1), varName.port("write_en")))
          else List()
        EmitOutput(
          varName.port(portName),
          varName.port("done"),
          struct
        )
      case ECast(e, t) => {
        e.typ = Some(t)
        expr.typ = Some(t)
        emitExpr(e)
      }
      case EArrAccess(id, accessors) => {
        val arr = store(CompVar(s"$id"))
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
        val portName = if (lhs) "write_data" else "read_data"
        val writeEnStruct =
          if (lhs) List(Connect(ConstantPort(1, 1), arr.port("write_en")))
          else List()
        EmitOutput(
          arr.port(portName),
          arr.port("done"),
          indexing ++ writeEnStruct
        )
      }
      case x =>
        throw NotImplemented(s"Futil backend does not support $x yet.", x.pos)
    }

  def emitCmd(
      c: Command
  )(implicit store: Store): (List[Structure], Control, Store) =
    c match {
      case CBlock(cmd) => emitCmd(cmd)
      case CPar(c1, c2) => {
        val (struct1, con1, s1) = emitCmd(c1)
        val (struct2, con2, s2) = emitCmd(c2)(s1)
        (struct1 ++ struct2, con1.par(con2), s2)
      }
      case CSeq(c1, c2) => {
        val (struct1, con1, s1) = emitCmd(c1)
        val (struct2, con2, s2) = emitCmd(c2)(s1)
        (struct1 ++ struct2, con1.seq(con2), s2)
      }
      case CLet(id, Some(tarr: TArray), None) =>
        (emitArrayDecl(tarr, id, false), Empty, store)
      case CLet(_, Some(_: TArray), Some(_)) =>
        throw NotImplemented(s"Futil backend cannot initialize memories", c.pos)
      case CLet(id, typ, Some(e)) => {
        val reg =
          LibDecl(genName(s"$id"), Stdlib.register(bitsForType(typ, c.pos)))
        val out = emitExpr(e)(store)
        val groupName = genName("let")
        val doneHole = Connect(
          reg.id.port("done"),
          HolePort(groupName, "done")
        )
        val struct =
          Connect(out.port, reg.id.port("in")) :: Connect(
            ConstantPort(1, 1),
            reg.id.port("write_en")
          ) :: doneHole :: out.structure
        val (group, st) = Group.fromStructure(groupName, struct)
        (
          reg :: group :: st,
          Enable(group.id),
          store + (CompVar(s"$id") -> reg.id)
        )
      }
      case CLet(id, typ, None) => {
        val reg =
          LibDecl(genName(s"$id"), Stdlib.register(bitsForType(typ, c.pos)))
        val struct = List(reg)
        (struct, Empty, store + (CompVar(s"$id") -> reg.id))
      }
      case CUpdate(lhs, rhs) => {
        val lOut = emitExpr(lhs, true)(store)
        val rOut = emitExpr(rhs)(store)
        val groupName = genName("upd")
        val doneHole =
          Connect(lOut.done, HolePort(groupName, "done"))
        val struct =
          lOut.structure ++ rOut.structure ++ List(
            Connect(rOut.port, lOut.port),
            doneHole
          )
        val (group, other_st) =
          Group.fromStructure(groupName, struct)
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
          Group.fromStructure(groupName, doneHole :: condOut.structure)
        val control = If(condOut.port, group.id, tCon, fCon)
        (group :: st ++ struct, control, store)
      }
      case CEmpty => (List(), SeqComp(List()), store)
      case CWhile(cond, _, body) => {
        val condOut = emitExpr(cond)
        val groupName = genName("cond")
        val doneHole = Connect(condOut.done, HolePort(groupName, "done"))
        val (condGroup, condDefs) =
          Group.fromStructure(groupName, doneHole :: condOut.structure)
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
      case x =>
        throw NotImplemented(s"Futil backend does not support $x yet", x.pos)
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
