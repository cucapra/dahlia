package fuselang.backend.futil

import fuselang.backend.futil.Futil._

import fuselang.common._
import Syntax._

import Configuration._
import CompilerError._

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

  /** Store mappings from Dahlia variables to
   * generated Futil variables.
   */
  type Store = Map[CompVar, CompVar]

  /** `emitDecl(d)` computes the structure that is needed to
   *  represent the declaration `d`. Simply returns a `List[Structure]`.
   */
  def emitDecl(d: Decl): List[Structure] = d.typ match {
    case TArray(_, dims, _) => {
      val const =
        LibDecl(CompVar(s"${d.id}-init"), Stdlib.constant(32, 0))
      val mem = LibDecl(CompVar(s"${d.id}"), Stdlib.memory(dims.map(_._1)))
      List(const, mem, Connect(const.id.port("out"), mem.id.port("data-in")))
    }
    case TBool() | TFloat() | TDouble() => {
      val const =
        LibDecl(CompVar(s"${d.id}-c"), Stdlib.constant(32, 0))
      val reg = LibDecl(CompVar("${d.id}"), Stdlib.register(32))
      List(const, reg, Connect(ThisPort(const.id), reg.id.port("in")))
    }
    case x => throw NotImplemented(s"Type $x not implemented for decls.")
  }

  /** `emitBinop` is a helper function to generate the structure
   *  for `e1 binop e2`. The return type is described in `emitExpr`.
   */
  def emitBinop(compName: String, e1: Expr, e2: Expr)(
    implicit store: Store
  ): (Port, List[Structure], List[CompVar]) = {
    val binop = Stdlib.op(s"$compName", 32);
    val (e1port, e1struct, act1) = emitExpr(e1)
    val (e2port, e2struct, act2) = emitExpr(e2)

    val comp = LibDecl(genName(compName), binop)
    val struct = List(
      comp,
      Connect(e1port, comp.id.port("left")),
      Connect(e2port, comp.id.port("right"))
    )
    (
      comp.id.port("out"),
      struct ++ e1struct ++ e2struct,
      comp.id :: act1 ++ act2
    )
  }

  /** `emitExpr(expr, lhs)(implicit store)` calculates the necessary structure
   *  to compute `expr`. It return the triple (Port, List[Structure], Enable).
   *  If `lhs = false`, then `Port` is the port that will hold the output
   *  of computing this expression. If `lhs = true`, then `Port` represents
   *  the port that can be used to put a value into the location represented by
   *  `expr`. `Enable` is the list of components that need to be activated to
   *  "activate" this expression.
   */
  def emitExpr(expr: Expr, lhs: Boolean = false)(
    implicit store: Store
  ): (Port, List[Structure], List[CompVar]) =
    expr match {
      case EInt(v, _) => {
        val _ = lhs
        val const = LibDecl(genName("const"), Stdlib.constant(32, v))
        (const.id.port("out"), List(const), List(const.id))
      }
      case ERational(v: String) => {
        val const = LibDecl(genName("const"), Stdlib.constant(32, v.toInt))
        (const.id.port("out"), List(const), List(const.id))
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
            case "<=" => "lte"
            case ">=" => "gte"
            case x => throw NotImplemented(s"Haven't implemented binop $x yet.")
          }
        emitBinop(compName, e1, e2)
      }
      case EVar(id) =>
        val portName = if (lhs) "in" else "out"
        (
          store(CompVar(s"$id")).port(portName),
          List(),
          List(store(CompVar(s"$id")))
        )
      case ECast(e, _) => emitExpr(e)
      case x => throw NotImplemented(s"No case for $x yet")
    }

  def emitCmd(
    c: Command
  )(implicit store: Store): (List[Structure], Control, Store) =
    c match {
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
      case CLet(id, _, Some(e)) => {
        val reg = LibDecl(CompVar(s"$id"), Stdlib.register(32))
        val (port, exStruct, acts) = emitExpr(e)(store)
        val struct = reg :: Connect(port, reg.id.port("in")) :: exStruct
        val group = Group(genName("group"), reg.id :: acts)
        (group :: struct, Enable(group.id), store + (CompVar(s"$id") -> reg.id))
      }
      case CLet(id, _, None) => {
        val reg = LibDecl(CompVar(s"$id"), Stdlib.register(32))
        val struct = List(reg)
        (struct, Empty(), store + (CompVar(s"$id") -> reg.id))
      }
      case CUpdate(lhs, rhs) => {
        val (lPort, lexStruct, lActs) = emitExpr(lhs, true)(store)
        val (rPort, rexStruct, rActs) = emitExpr(rhs)(store)
        val group = Group(genName("group"), lActs ++ rActs)
        val struct = lexStruct ++ rexStruct ++ List(Connect(rPort, lPort)) ++ List(
          group
        )
        val control = Enable(group.id)
        (struct, control, store)
      }
      case CIf(cond, tbranch, fbranch) => {
        val (port, condStruct, acts) = emitExpr(cond)
        val (tStruct, tCon, _) = emitCmd(tbranch)
        val (fStruct, fCon, _) = emitCmd(fbranch)
        val struct = condStruct ++ tStruct ++ fStruct
        val group = Group(genName("group"), acts)
        val control = If(port, group.id, tCon, fCon)
        (group :: struct, control, store)
      }
      case CEmpty => (List(), SeqComp(List()), store)
      case x => throw NotImplemented(s"No case for $x yet")
    }

  def emitProg(p: Prog, c: Config): String = {
    val _ = c
    val declStruct =
      p.decls.map(x => emitDecl(x)).foldLeft(List[Structure]())(_ ++ _)
    val store = declStruct.foldLeft(Map[CompVar, CompVar]())(
      (store, struct) =>
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
  override val commentPrefix: String = ";"
}
