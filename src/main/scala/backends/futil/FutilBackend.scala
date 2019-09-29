package fuselang.backend.futil

import fuselang.backend.futil.Futil._
// import fuselang.backend.futil.Lib

import fuselang.common._
import Syntax._

import Configuration._
import CompilerError._

private class FutilBackendHelper {

  var idx: Map[String, Int] = Map();
  def genName(base: String): CompVar = {
    // update idx
    idx get base match {
      case Some(n) => idx = idx + (base -> (n + 1))
      case None    => idx = idx + (base -> 0)
    }
    CompVar(s"$base${idx(base)}")
  }

  type Store = Map[CompVar, CompVar]

  def emitDecl(d: Decl): List[Structure] = d.typ match {
    case TArray(_, dims) => {
      val const =
        LibDecl(CompVar(s"${d.id}-init"), Stdlib.constant(VNone()))
      val mem = LibDecl(CompVar(s"${d.id}"), Stdlib.memory(dims.map(_._1)))
      List(
        const,
        mem,
        Connect(const.id @@ "out", mem.id @@ "data-in")
      )
    }
    case TBool() | TFloat() | TDouble() => {
      val const =
        LibDecl(CompVar(s"${d.id}-c"), Stdlib.constant(VNone()))
      val reg = LibDecl(CompVar("${d.id}"), Stdlib.register())
      List(const, reg, Connect(ThisPort(const.id), reg.id @@ "in"))
    }
    case x => throw NotImplemented(s"Type $x not implemented for decls.")
  }

  def emitBinop(binop: CompInst, e1: Expr, e2: Expr)(
      implicit store: Store
  ): (Port, List[Structure], Enable) = {
    val (e1port, e1struct, act1) = emitExpr(e1)
    val (e2port, e2struct, act2) = emitExpr(e2)

    val comp = LibDecl(genName(s"${binop.id}"), binop)
    val struct = List(
      comp,
      Connect(e1port, comp.id @@ "left"),
      Connect(e2port, comp.id @@ "right")
    )
    (
      comp.id @@ "out",
      struct ++ e1struct ++ e2struct,
      Enable(List(comp.id)) ++ act1 ++ act2
    )
  }

  def emitExpr(expr: Expr, lhs: Boolean = false)(
      implicit store: Store
  ): (Port, List[Structure], Enable) =
    expr match {
      case EInt(v, _) => {
        val const = LibDecl(genName("const"), Stdlib.constant(Num(v)))
        (const.id @@ "out", List(const), Enable(List(const.id)))
      }
      case ERational(v: String) => {
        val const = LibDecl(genName("const"), Stdlib.constant(v))
        (const.id @@ "out", List(const), Enable(List(const.id)))
      }
      case EBinop(op, e1, e2) => {
        op.op match {
          case "+"  => emitBinop(Stdlib.op("comp/add"), e1, e2)
          case "-"  => emitBinop(Stdlib.op("comp/sub"), e1, e2)
          case "*"  => emitBinop(Stdlib.op("comp/mult"), e1, e2)
          case "/"  => emitBinop(Stdlib.op("comp/div"), e1, e2)
          case "<"  => emitBinop(Stdlib.op("comp/lt"), e2, e1)
          case ">"  => emitBinop(Stdlib.op("comp/gt"), e1, e2)
          case "<=" => emitBinop(Stdlib.op("comp/lte"), e1, e2)
          case ">=" => emitBinop(Stdlib.op("comp/gte"), e2, e1)
          case x    => throw NotImplemented(s"Haven't implemented binop $x yet.")
        }
      }
      case EVar(id) =>
        val portName = if (lhs) "in" else "out"
        (
          store(CompVar(s"$id")) @@ portName,
          List(),
          Enable(List(store(CompVar(s"$id"))))
        )

      case EArrAccess(id, idxs) => {
        // aggh scala you should be smarter
        val (ports, structs, acts) = idxs.map(e => emitExpr(e)).unzip3
        val (gadgets, addrStructs) =
          ports.zipWithIndex.map {
            case (p, i) =>
              val gad = LibDecl(genName("gad"), Stdlib.identity())
              (
                gad,
                List(
                  gad,
                  Connect(p, gad.id @@ "in"),
                  Connect(gad.id @@ "out", CompVar(s"$id") @@ s"addr$i")
                )
              )
          }.unzip

        val flatActs = acts.map(_.ids).flatten

        if (lhs) {
          val writeEn = LibDecl(genName(s"$id-we"), Stdlib.identity())
          val writeStruct = List(
            writeEn,
            Connect(writeEn.id @@ "out", CompVar(s"$id") @@ "data-in")
          )
          (
            writeEn.id @@ "in",
            structs.flatten ++ addrStructs.flatten ++ writeStruct,
            Enable(
              List(CompVar(s"$id"), writeEn.id) ++ gadgets.map(_.id) ++ flatActs
            )
          )
        } else {
          (
            CompVar(s"$id") @@ "out",
            structs.flatten ++ addrStructs.flatten,
            Enable(List(CompVar(s"$id")) ++ gadgets.map(_.id) ++ flatActs)
          )
        }
      }
      case ECast(e, _) => emitExpr(e)
      case EApp(Id("sqrt"), List(e)) => {
        val (port, struct, act) = emitExpr(e)
        val sqrtComp = LibDecl(genName("sqrt"), Stdlib.sqrt())
        val sqrtStruct = List(sqrtComp, Connect(port, sqrtComp.id @@ "in"))
        (
          sqrtComp.id @@ "out",
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
        (struct1 ++ struct2, con1 <:> con2, s2)
      }
      case CSeq(c1, c2) => {
        val (struct1, con1, s1) = emitCmd(c1)
        val (struct2, con2, s2) = emitCmd(c2)(s1)
        (struct1 ++ struct2, con1 --- con2, s2)
      }
      case CLet(id, _, Some(e)) => {
        val reg = LibDecl(genName(s"$id"), Stdlib.register())
        val (port, exStruct, acts) = emitExpr(e)(store)
        val struct = List(
          reg,
          Connect(port, reg.id @@ "in")
        ) ++ exStruct
        (
          struct,
          Enable(List(reg.id)) ++ acts,
          store + (CompVar(s"$id") -> reg.id)
        )
      }
      case CLet(id, _, None) => {
        val reg = LibDecl(genName(s"$id"), Stdlib.register())
        val struct = List(reg)
        (struct, Empty(), store + (CompVar(s"$id") -> reg.id))
      }
      case CUpdate(lhs, rhs) => {
        val (lPort, lexStruct, lActs) = emitExpr(lhs, true)(store)
        val (rPort, rexStruct, rActs) = emitExpr(rhs)(store)
        val struct = lexStruct ++ rexStruct ++ List(
          Connect(rPort, lPort)
        )
        val control = lActs ++ rActs
        (struct, control, store)
      }
      case CFor(range, _, par, comb) =>
        range match {
          case CRange(id, start, end, 1) => {
            val iter = LibDecl(genName(s"$id"), Stdlib.iterator())

            val iterStart =
              LibDecl(iter.id ^^ "-start", Stdlib.constant(Num(start)))
            val iterIncr =
              LibDecl(iter.id ^^ "-incr", Stdlib.constant(Num(1)))
            val iterEnd =
              LibDecl(iter.id ^^ "-end", Stdlib.constant(Num(end)))
            val iterEn =
              LibDecl(iter.id ^^ "-en", Stdlib.constant(Num(1)))

            val struct = List(
              iter,
              Connect(iterStart.id @@ "out", iter.id @@ "start"),
              Connect(iterIncr.id @@ "out", iter.id @@ "incr"),
              Connect(iterEnd.id @@ "out", iter.id @@ "end"),
              Connect(iterEn.id @@ "out", iter.id @@ "en")
            )
            val (parStruct, parCon, s1) =
              emitCmd(par)(store + (CompVar(s"$id") -> iter.id))
            val (combStruct, combControl, _) = emitCmd(comb)(s1)
            (
              struct ++ parStruct ++ combStruct,
              Enable(
                List(
                  iterEn.id,
                  iterStart.id,
                  iterIncr.id,
                  iterEnd.id,
                  iter.id
                )
              ) --- While(
                iter.id @@ "stop", // until (iter @ stop) = 0
                (parCon --- combControl) ---
                  Enable(List(iter.id, iterEn.id)) // iter++
              ),
              store
            )
          }
          case _ => throw NotImplemented("Haven't done all the iterators yet")
        }
      case CWhile(cond, _, body) => {
        val (port, condStruct, acts) = emitExpr(cond)(store)
        val (bodyStruct, bodyCon, _) = emitCmd(body)(store)
        val struct = bodyStruct ++ condStruct
        val control =
          acts --- (While(port, bodyCon --- acts))

        (struct, control, store)
      }
      case CReduce(rop, lhs, rhs) => {
        val (linPort, lStruct, lAct) = emitExpr(lhs, true)
        val (loutPort, _, _) = emitExpr(lhs)
        val (rPort, rStruct, rAct) = emitExpr(rhs)
        val (ropStruct, control) = rop.op match {
          case "+=" => {
            val adder = LibDecl(genName("comb-add"), Stdlib.op("comp/add"))
            (
              List(
                adder,
                Connect(rPort, adder.id @@ "right"),
                Connect(loutPort, adder.id @@ "left"),
                Connect(adder.id @@ "out", linPort)
              ),
              Enable(List(adder.id))
            )
          }
          case x => throw Impossible(s"$x is not a reduce operator!")
        }
        (lStruct ++ rStruct ++ ropStruct, control ++ lAct ++ rAct, store)
      }
      // XXX(sam): obviously a hack
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
