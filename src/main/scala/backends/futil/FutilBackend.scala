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
      case None    => idx = idx + (base -> 0)
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
  ): (Port, List[Structure], Enable) = {
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
      Enable(List(comp.id)) ++ act1 ++ act2
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
  ): (Port, List[Structure], Enable) =
    expr match {
      case EInt(v, _) => {
        val const = LibDecl(genName("const"), Stdlib.constant(32, v))
        (const.id.port("out"), List(const), Enable(List(const.id)))
      }
      case ERational(v: String) => {
        val const = LibDecl(genName("const"), Stdlib.constant(32, v.toInt))
        (const.id.port("out"), List(const), Enable(List(const.id)))
      }
      case EBinop(op, e1, e2) => {
        val compName =
          op.op match {
            case "+"  => "add"
            case "-"  => "sub"
            case "*"  => "mult"
            case "/"  => "div"
            case "<"  => "lt"
            case ">"  => "gt"
            case "<=" => "lte"
            case ">=" => "gte"
            case x    => throw NotImplemented(s"Haven't implemented binop $x yet.")
          }
        emitBinop(compName, e1, e2)
      }
      case EVar(id) =>
        val portName = if (lhs) "in" else "out"
        (
          store(CompVar(s"$id")).port(portName),
          List(),
          Enable(List(store(CompVar(s"$id"))))
        )

      case EArrAccess(id, idxs) => {
        // aggh scala you should be smarter
        val (ports, structs, acts) = idxs.map(e => emitExpr(e)).unzip3
        val (gadgets, addrStructs) =
          ports.zipWithIndex.map {
            case (p, i) =>
              val gad = LibDecl(genName("gad"), Stdlib.identity(32))
              (
                gad,
                List(
                  gad,
                  Connect(p, gad.id.port("in")),
                  Connect(gad.id.port("out"), CompVar(s"$id").port(s"addr$i"))
                )
              )
          }.unzip

        val flatActs = acts.map(_.ids).flatten

        if (lhs) {
          val writeEn = LibDecl(genName(s"$id-we"), Stdlib.identity(32))
          val writeStruct = List(
            writeEn,
            Connect(writeEn.id.port("out"), CompVar(s"$id").port("data-in"))
          )
          (
            writeEn.id.port("in"),
            structs.flatten ++ addrStructs.flatten ++ writeStruct,
            Enable(
              List(CompVar(s"$id"), writeEn.id) ++ gadgets.map(_.id) ++ flatActs
            )
          )
        } else {
          (
            CompVar(s"$id").port("out"),
            structs.flatten ++ addrStructs.flatten,
            Enable(List(CompVar(s"$id")) ++ gadgets.map(_.id) ++ flatActs)
          )
        }
      }
      case ECast(e, _) => emitExpr(e)
      case EApp(Id("sqrt"), List(e)) => {
        val (port, struct, act) = emitExpr(e)
        val sqrtComp = LibDecl(genName("sqrt"), Stdlib.sqrt())
        val sqrtStruct = List(sqrtComp, Connect(port, sqrtComp.id.port("in")))
        (
          sqrtComp.id.port("out"),
          struct ++ sqrtStruct,
          Enable(List(sqrtComp.id)) ++ act
        )
      }
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
        val reg = LibDecl(genName(s"$id"), Stdlib.register(32))
        val (port, exStruct, acts) = emitExpr(e)(store)
        val struct = List(reg, Connect(port, reg.id.port("in"))) ++ exStruct
        (
          struct,
          Enable(List(reg.id)) ++ acts,
          store + (CompVar(s"$id") -> reg.id)
        )
      }
      case CLet(id, _, None) => {
        val reg = LibDecl(genName(s"$id"), Stdlib.register(32))
        val struct = List(reg)
        (struct, Empty(), store + (CompVar(s"$id") -> reg.id))
      }
      case CUpdate(lhs, rhs) => {
        val (lPort, lexStruct, lActs) = emitExpr(lhs, true)(store)
        val (rPort, rexStruct, rActs) = emitExpr(rhs)(store)
        val struct = lexStruct ++ rexStruct ++ List(Connect(rPort, lPort))
        val control = lActs ++ rActs
        (struct, control, store)
      }
      case CIf(cond, tbranch, fbranch) => {
        val (port, condStruct, acts) = emitExpr(cond)
        val (tStruct, tCon, _) = emitCmd(tbranch)
        val (fStruct, fCon, _) = emitCmd(fbranch)
        val struct = condStruct ++ tStruct ++ fStruct
        val control = If(port, acts.ids, tCon, fCon)
        (struct, control, store)
      }
      case CFor(range, _, par, comb) =>
        range match {
          case CRange(id, start, end, 1) => {
            val iter = LibDecl(genName(s"$id"), Stdlib.iterator(32, start, 1, end))
            val struct = List(iter)
            val (parStruct, parCon, s1) =
              emitCmd(par)(store + (CompVar(s"$id") -> iter.id))
            val (combStruct, combControl, _) = emitCmd(comb)(s1)
            val body = combControl match {
              case Enable(List()) => parCon
              case _ => parCon.seq(combControl)
            }
            val control = While(
              iter.id.port("stop"), // until (iter @ stop) = 0
              List(iter.id),
              body
            )
            (struct ++ parStruct ++ combStruct, control, store)
          }
          case _ => throw NotImplemented("Haven't done all the iterators yet")
        }
      case CWhile(cond, _, body) => {
        val (port, condStruct, acts) = emitExpr(cond)
        val (bodyStruct, bodyCon, _) = emitCmd(body)
        val struct = bodyStruct ++ condStruct
        val control = While(port, acts.ids, bodyCon.seq(acts))
        (struct, control, store)
      }
      case CReduce(rop, lhs, rhs) => {
        val (linPort, lStruct, lAct) = emitExpr(lhs, true)
        val (loutPort, _, _) = emitExpr(lhs)
        val (rPort, rStruct, rAct) = emitExpr(rhs)
        val (ropStruct, control) = rop.op match {
          case "+=" => {
            val adder = LibDecl(genName("comb-add"), Stdlib.add(32))
            (
              List(
                adder,
                Connect(rPort, adder.id.port("right")),
                Connect(loutPort, adder.id.port("left")),
                Connect(adder.id.port("out"), linPort)
              ),
              Enable(List(adder.id))
            )
          }
          case x => throw Impossible(s"$x is not a reduce operator!")
        }
        (lStruct ++ rStruct ++ ropStruct, control ++ lAct ++ rAct, store)
      }
      // hardcoded print functions
      case CExpr(EApp(Id("print_vec"), List(EVar(x)))) =>
        (List(), Print(CompVar(s"$x")), store)
      case CExpr(EApp(Id("print_vec_2d"), List(EVar(x)))) =>
        (List(), Print(CompVar(s"$x")), store)
      case CExpr(EApp(Id("print_vec_3d"), List(EVar(x)))) =>
        (List(), Print(CompVar(s"$x")), store)
      case CEmpty => (List(), Enable(List()), store)
      case x      => throw NotImplemented(s"No case for $x yet")
    }

  def emitProg(p: Prog, c: Config): String = {
    val _ = c
    val declStruct =
      p.decls.map(x => emitDecl(x)).foldLeft(List[Structure]())(_ ++ _)
    val store = declStruct.foldLeft(Map[CompVar, CompVar]())(
      (store, struct) =>
        struct match {
          case CompDecl(id, _) => store + (id -> id)
          case LibDecl(id, _)  => store + (id -> id)
          case _               => store
        }
    )
    val (cmdStruct, control, _) = emitCmd(p.cmd)(store)
    val struct = declStruct ++ cmdStruct
    Namespace("prog", List(Component("main", List(), List(), struct, control))).emit
  }
}

case object FutilBackend extends fuselang.backend.Backend {
  def emitProg(p: Prog, c: Config) = {
    (new FutilBackendHelper()).emitProg(p, c)
  }
  val canGenerateHeader = false
}
