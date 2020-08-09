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
    val group: Option[CompVar],
    val structure: List[Structure] = List(),
    val delay: Option[Int] = None
) {
  def groupConnect(f: (CompVar) => Structure): Structure = {
    this.group match {
      case Some(compvar) => f(compvar)
      case None => Empty()
    }
  }
}

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
  def emitBinop(
      compName: String,
      e1: Expr,
      e2: Expr
  )(
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
    val eBitwidth = bitsForType(e1.typ, e1.pos)
    val binop = Stdlib.op(s"$compName", eBitwidth);

    val delay =
      for (d1 <- e1Out.delay; d2 <- e2Out.delay) yield math.max(d1, d2)
    val comp = LibDecl(genName(compName), binop)
    val groupName = genName(s"${compName}_group")

    delay match {
      // combinational case
      case Some(0) => {
        val struct = List(
          comp,
          e1Out.groupConnect((v) => Connect(ConstantPort(1, 1), v.hole("go"))),
          e2Out.groupConnect((v) => Connect(ConstantPort(1, 1), v.hole("go"))),
          Connect(e1Out.port, comp.id.port("left")),
          Connect(e2Out.port, comp.id.port("right")),
          Connect(ConstantPort(1, 1), groupName.hole("done"))
        )
        val (group, st) = Group.fromStructure(groupName, struct, Some(0))
        EmitOutput(
          comp.id.port("out"),
          Some(group.id),
          group :: st ++ e1Out.structure ++ e2Out.structure,
          delay = Some(0)
        )
      }
      // we need to save the dones and values of the children
      case _ => {
        val leftVal =
          LibDecl(genName("left_value"), Stdlib.register(eBitwidth))
        val leftDone =
          LibDecl(genName("left_done"), Stdlib.register(1))
        val rightVal =
          LibDecl(genName("right_value"), Stdlib.register(eBitwidth))
        val rightDone =
          LibDecl(genName("right_done"), Stdlib.register(1))
        val bothDoneGuard = Some(
          And(Atom(leftDone.id.port("out")), Atom(rightDone.id.port("out")))
        )

        val struct = List(
          comp,
          leftVal,
          leftDone,
          rightVal,
          rightDone,
          // tell children to go
          e1Out.groupConnect((g) =>
            Connect(
              ConstantPort(1, 1),
              g.hole("go"),
              Some(Not(Atom(leftDone.id.port("out"))))
            )
          ),
          e2Out.groupConnect((g) =>
            Connect(
              ConstantPort(1, 1),
              g.hole("go"),
              Some(Not(Atom(rightDone.id.port("out"))))
            )
          ),
          // save values
          Connect(
            e1Out.port,
            leftVal.id.port("in"),
            e1Out.group.map((g) => Atom(g.hole("done")))
          ),
          Connect(
            ConstantPort(1, 1),
            leftVal.id.port("write_en"),
            e1Out.group.map((g) => Atom(g.hole("done")))
          ),
          Connect(
            e2Out.port,
            rightVal.id.port("in"),
            e2Out.group.map((g) => Atom(g.hole("done")))
          ),
          Connect(
            ConstantPort(1, 1),
            rightVal.id.port("write_en"),
            e2Out.group.map((g) => Atom(g.hole("done")))
          ),
          // save done holes
          Connect(
            ConstantPort(1, 1),
            leftDone.id.port("in"),
            e1Out.group.map((g) => Atom(g.hole("done")))
          ),
          Connect(
            ConstantPort(1, 1),
            leftDone.id.port("write_en"),
            e1Out.group.map((g) => Atom(g.hole("done")))
          ),
          Connect(
            ConstantPort(1, 1),
            rightDone.id.port("in"),
            e2Out.group.map((g) => Atom(g.hole("done")))
          ),
          Connect(
            ConstantPort(1, 1),
            rightDone.id.port("write_en"),
            e2Out.group.map((g) => Atom(g.hole("done")))
          ),
          // compute binop
          Connect(leftVal.id.port("out"), comp.id.port("left"), bothDoneGuard),
          Connect(
            rightVal.id.port("out"),
            comp.id.port("right"),
            bothDoneGuard
          ),
          // signal done
          Connect(ConstantPort(1, 1), groupName.hole("done"), bothDoneGuard)
        )
        val resetStruct = List(
          Connect(
            ConstantPort(1, 0),
            leftDone.id.port("in"),
            Some(Not(Atom(groupName.hole("go"))))
          ),
          Connect(
            ConstantPort(1, 1),
            leftDone.id.port("write_en"),
            Some(Not(Atom(groupName.hole("go"))))
          ),
          Connect(
            ConstantPort(1, 0),
            rightDone.id.port("in"),
            Some(Not(Atom(groupName.hole("go"))))
          ),
          Connect(
            ConstantPort(1, 1),
            rightDone.id.port("write_en"),
            Some(Not(Atom(groupName.hole("go"))))
          )
        )
        val (group, st) =
          Group.fromStructure(groupName, struct, delay.map(_ + 1))
        EmitOutput(
          comp.id.port("out"),
          Some(group.id),
          group :: st ++ resetStruct ++ e1Out.structure ++ e2Out.structure,
          delay = group.staticDelay
        )
      }
    }
  }

  def emitMultiCycleBinop(
      compName: String,
      e1: Expr,
      e2: Expr,
      delay: Option[Int] = None
  )(
      implicit store: Store
  ): EmitOutput = {
    val name = genName(compName)
    val comp = LibDecl(
      name,
      Stdlib.op(s"${compName}_pipe", bitsForType(e1.typ, e1.pos))
    )

    val e1Out = emitExpr(e1)
    val e2Out = emitExpr(e2)
    // XXX(sam): doesn't work if they take different amounts of time :(
    val bothDoneGuard = e1Out.group.flatMap((x) =>
      e2Out.group.map((y) => And(Atom(x.hole("done")), Atom(y.hole("done"))))
    )

    val groupName = genName(s"${compName}_group")

    val struct = List(
      comp,
      e1Out.groupConnect((g) => Connect(ConstantPort(1, 1), g.hole("go"))),
      e2Out.groupConnect((g) => Connect(ConstantPort(1, 1), g.hole("go"))),
      Connect(
        e1Out.port,
        comp.id.port("left"),
        e1Out.group.map((p) => Atom(p.hole("done")))
      ),
      Connect(
        e2Out.port,
        comp.id.port("right"),
        e2Out.group.map((p) => Atom(p.hole("done")))
      ),
      Connect(ConstantPort(1, 1), groupName.hole("done"), bothDoneGuard)
    )

    val outDelay =
      for (e1d <- e1Out.delay; e2d <- e2Out.delay; opd <- delay)
        yield e1d + e2d + opd;

    val (group, st) = Group.fromStructure(groupName, struct, outDelay)

    EmitOutput(
      comp.id.port("out"),
      Some(group.id),
      group :: st ++ e1Out.structure ++ e2Out.structure,
      delay = outDelay
    )
  }

  /** `emitExpr(expr, rhsInfo)(implicit store)` calculates the necessary structure
    *  to compute `expr`. It return the pair (Port, List[Structure]).
    *  If `rhsInfo = None`, then `Port` is the port that will hold the output
    *  of computing this expression. If `rhsInfo = Some(...)`, then `Port` represents
    *  the port that can be used to put a value into the location represented by
    *  `expr`.
    */
  def emitExpr(expr: Expr, rhsInfo: Option[EmitOutput] = None)(
      implicit store: Store
  ): EmitOutput =
    expr match {
      case EInt(v, _) => {
        val _ = rhsInfo
        val groupName = genName("const_gr")
        val const =
          LibDecl(
            genName("const"),
            Stdlib.constant(bitsForType(expr.typ, expr.pos), v)
          )
        val struct = List(
          const,
          Connect(ConstantPort(1, 1), groupName.hole("done"))
        )
        val (group, st) =
          Group.fromStructure(groupName, struct, Some(0))
        EmitOutput(
          const.id.port("out"),
          Some(group.id),
          group :: st,
          delay = Some(0)
        )
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
            case "&&" => "and"
            case "||" => "or"
            case x =>
              throw NotImplemented(
                s"Futil backend does not support '$x' yet.",
                op.pos
              )
          }
        op.op match {
          case "*" => emitMultiCycleBinop(compName, e1, e2, Some(3))
          case "/" => emitMultiCycleBinop(compName, e1, e2, None)
          case _ => emitBinop(compName, e1, e2)
        }
      }
      case EVar(id) => {
        val varName = store
          .get(CompVar(s"$id"))
          .getOrThrow(Impossible(s"$id was not in `store`"))

        rhsInfo match {
          // we are a lhs, so prepare for writes
          case Some(rhsOut) => {
            val groupName = genName(s"${id}_gr")
            val struct = List(
              Connect(
                ConstantPort(1, 1),
                varName.port("write_en"),
                rhsOut.group.map((g) => Atom(g.hole("done")))
              ),
              Connect(varName.port("done"), groupName.hole("done"))
            )
            val (group, st) =
              Group.fromStructure(groupName, struct, rhsOut.delay.map(_ + 1))
            EmitOutput(
              varName.port("in"),
              Some(group.id),
              group :: st,
              group.staticDelay
            )
          }
          // we are being read from
          case None => {
            EmitOutput(
              varName.port("out"),
              None,
              List(),
              Some(0)
            )
          }
        }
      }
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

        rhsInfo match {
          // we are a lhs, so prepare for writes
          case Some(rhsOut) => {
            val groupName = genName(s"${arr.name}_write")
            val struct = List(
              Connect(
                ConstantPort(1, 1),
                arr.port("write_en"),
                rhsOut.group.map((g) => Atom(g.hole("done")))
              ),
              Connect(arr.port("done"), groupName.hole("done"))
            )
            val (group, st) =
              Group.fromStructure(groupName, struct, rhsOut.delay.map(_ + 1))
            EmitOutput(
              arr.port("write_data"),
              Some(group.id),
              group :: st,
              group.staticDelay
            )
          }
          // we are being read from
          case None => {
            EmitOutput(
              arr.port("read_data"),
              None,
              indexing,
              Some(0)
            )
          }
        }
      }
      case EApp(Id("sqrt"), List(arg)) => {
        // XXX(sam) doesn't support complex argument expressions
        val argOut = emitExpr(arg)
        val sqrt = LibDecl(genName("sqrt"), Stdlib.sqrt())
        val groupName = genName("sqrt_gr")
        val struct = List(
          sqrt,
          Connect(argOut.port, sqrt.id.port("in")),
          Connect(ConstantPort(1, 1), sqrt.id.port("go")),
          Connect(sqrt.id.port("done"), groupName.hole("done"))
        )
        val (group, st) = Group.fromStructure(groupName, struct, Some(1))
        EmitOutput(
          sqrt.id.port("out"),
          Some(group.id),
          group :: st ++ argOut.structure,
          delay = Some(1)
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
        val struct = List(
          reg,
          // tell child to go
          out.groupConnect((g) =>
            Connect(
              ConstantPort(1, 1),
              g.hole("go"),
              Some(Not(Atom(g.hole("done"))))
            )
          ),
          // write when child is done
          Connect(
            out.port,
            reg.id.port("in"),
            out.group.map((g) => Atom(g.hole("done")))
          ),
          Connect(
            ConstantPort(1, 1),
            reg.id.port("write_en"),
            out.group.map((g) => Atom(g.hole("done")))
          ),
          doneHole
        )
        val (group, st) =
          Group.fromStructure(groupName, struct, out.delay.map(_ + 1))
        (
          group :: st ++ out.structure,
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
        val rOut = emitExpr(rhs)(store)
        val lOut = emitExpr(lhs, Some(rOut))(store)
        val groupName = genName("upd")
        val doneHole =
          Connect(
            ConstantPort(1, 1),
            HolePort(groupName, "done"),
            lOut.group.map((g) => Atom(g.hole("done")))
          )
        val struct = List(
          // tell child to go
          rOut.groupConnect((g) =>
            Connect(
              ConstantPort(1, 1),
              g.hole("go"),
              Some(Not(Atom(g.hole("done"))))
            )
          ),
          // connect outputs when done
          Connect(
            rOut.port,
            lOut.port,
            rOut.group.map((g) => Atom(g.hole("done")))
          ),
          doneHole
        )
        // val struct =
        //   lOut.structure ++ rOut.structure ++ List(
        //     Connect(rOut.port, lOut.port, Some(Atom(rOut.done))),
        //     doneHole
        //   )
        val (group, other_st) =
          Group.fromStructure(groupName, struct, lOut.delay)
        (
          group :: other_st ++ lOut.structure ++ rOut.structure,
          Enable(group.id),
          store
        )
      }
      case CIf(cond, tbranch, fbranch) => {
        val condOut = emitExpr(cond)
        val (tStruct, tCon, _) = emitCmd(tbranch)
        val (fStruct, fCon, _) = emitCmd(fbranch)
        val struct = tStruct ++ fStruct ++ condOut.structure

        condOut.group match {
          // there's already a group for the condition
          case Some(group) => {
            val control = If(condOut.port, group, tCon, fCon)
            (struct, control, store)
          }
          // we have to make a group for the condition
          case None => {
            val groupName = genName("cond")
            val doneHole =
              Connect(ConstantPort(1, 1), HolePort(groupName, "done"))
            val (group, st) =
              Group.fromStructure(
                groupName,
                List(doneHole),
                condOut.delay
              )
            val control = If(condOut.port, groupName, tCon, fCon)
            (group :: st ++ struct, control, store)
          }
        }
      }
      case CEmpty => (List(), SeqComp(List()), store)
      case CWhile(cond, _, body) => {
        val condOut = emitExpr(cond)
        val (bodyStruct, bodyCon, st) = emitCmd(body)
        val struct = condOut.structure ++ bodyStruct

        condOut.group match {
          // we already have a group for the condition
          case Some(group) => {
            val control = While(condOut.port, group, bodyCon)
            (struct, control, store)
          }
          // need to make a group for the condition
          case None => {
            val groupName = genName("cond")
            val doneHole =
              Connect(ConstantPort(1, 1), HolePort(groupName, "done"))
            val (condGroup, condDefs) =
              Group.fromStructure(
                groupName,
                List(doneHole),
                condOut.delay
              )
            val control = While(condOut.port, condGroup.id, bodyCon)
            (condGroup :: condDefs ++ struct, control, st)
          }
        }

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
      case _: CDecorate => (List(), SeqComp(List()), store)
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
