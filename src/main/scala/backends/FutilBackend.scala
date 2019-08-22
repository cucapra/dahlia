package fuselang.backend

import fuselang.common._
import Syntax._

import Configuration._
import CompilerError._

object Futil {
  sealed trait Primitives
  case class Custom(name: String) extends Primitives {
    override def toString = name
  }
  case class Ident() extends Primitives {
    override def toString = "comp/id"
  }
  case class Memory1d() extends Primitives {
    override def toString = "comp/memory1d"

  }
  case class Memory2d() extends Primitives {
    override def toString = "comp/memory2d"
  }
  case class Iterator() extends Primitives {
    override def toString = "comp/iterator"
  }
  case class Register() extends Primitives {
    override def toString = "comp/reg"
  }
  case class Add() extends Primitives {
    override def toString = "comp/add"
  }
  case class Sub() extends Primitives {
    override def toString = "comp/sub"
  }
  case class TruncSub() extends Primitives {
    override def toString = "comp/trunc-sub"
  }
  case class Mult() extends Primitives {
    override def toString = "comp/mult"
  }
  case class Div() extends Primitives {
    override def toString = "comp/div"
  }
  case class Sqrt() extends Primitives {
    override def toString = "comp/sqrt"
  }
  case class Constant(id: Id, v: Futil.Value) extends Primitives {
    override def toString = s"const $id $v : 32"
  }

  case class Component(id: Id, sub: Primitives)

  sealed trait Value
  case class FInt(v: Int) extends Value {
    override def toString() = s"$v"
  }
  case class FFloat(v: String) extends Value {
    override def toString() = s"$v"
  }
  case class FNone() extends Value {
    override def toString() = "#f"
  }

  sealed trait Port
  case class CompPort(c: Component, name: String) extends Port {
    override def toString = s"${c.id} @ $name"
  }
  case class CustomPort(c: String, name: String) extends Port {
    override def toString = s"$c @ $name"
  }
  case class LonePort(c: Component) extends Port {
    override def toString = s"${c.sub}"
  }

  sealed trait Structure
  case class NewComp(comp: Component) extends Structure {
    override def toString = comp.sub match {
      case Constant(_, _) => ""
      case _ => s"[${comp.id} = new ${comp.sub}]"
    }
  }
  case class Connection(input: Port, output: Port) extends Structure {
    override def toString = s"[$input -> $output]"
  }

  sealed trait Control
  case class ParComp(c1: Control, c2: Control) extends Control {
    override def toString(): String = {
      def f(c: Control) = c match {
        case SeqComp(_, _) => s"($c)"
        case _ => s"$c"
      }
      s"${f(c1)} ${f(c2)}"
    }
  }
  case class SeqComp(c1: Control, c2: Control) extends Control {
    override def toString(): String = {
      def f(c: Control) = c match {
        case SeqComp(_, _) => s"$c"
        case NOP() => ""
        case _ => s"[$c]"
      }
      s"${f(c1)}\n${f(c2)}"
    }
  }
  case class Activate(ids: List[Id]) extends Control {
    def ++(a: Activate): Activate = Activate((this.ids ++ a.ids).distinct)
    override def toString(): String = ids.mkString("(!! ", " ", ")")

  }
  case class WhileLoop(condition: Port, body: Control) extends Control {
    override def toString(): String = {
      s"(while ($condition)\n($body))"
    }
  }
  case class NOP() extends Control {
    override def toString(): String = ""
  }
  case class MemPrint(id: Id) extends Control {
    override def toString(): String = {
      s"(mem-print $id)"
    }
  }

  var idx: Map[String, Int] = Map();
  def genName(base: String): Id = {
    // update idx
    idx get base match {
      case Some(n) => idx = idx + (base -> (n + 1))
      case None => idx = idx + (base -> 0)
    }
    Id(s"$base${idx(base)}")
  }

  def sortStructure(struct: List[Structure]): List[Structure] = {
    struct.sortWith((s0, s1) => (s0, s1) match {
      case (Connection(_, _), Connection(_, _)) => false
      case (NewComp(_), Connection(_, _)) => true
      case (Connection(_, _), NewComp(_)) => false
      case (NewComp(_), NewComp(_)) => true
    })
  }

  def program(name: String, struct: List[Structure], control: Control): String = {
    val langStr = "#lang racket/base"
    val imports = List("futil")
    val importStr = imports.mkString("(require ", " ", ")")

    val structStr = struct.map(x => x.toString()).mkString("\n")

    val body = s"(define/module $name () ()\n ($structStr)\n$control)"
    val execute = "(parse-cmdline (main))"
    List(langStr, importStr, body, execute).mkString("\n\n")
  }

}

private class FutilBackendHelper {

  type Store = Map[Id, Id]

  def emitDecl(d: Decl): List[Futil.Structure] = d.typ match {
    case TArray(_, dims) => {
      if (dims.length == 1) {
        val const = Futil.Constant(Id(s"${d.id}-init"), Futil.FNone())
        val constComp = Futil.Component(const.id, const)
        val mem = Futil.Component(d.id, Futil.Memory1d())
        List(
          Futil.NewComp(constComp),
          Futil.Connection(Futil.LonePort(constComp), Futil.CompPort(mem, "data-in")),
          Futil.NewComp(mem)
        )
      } else if (dims.length == 2) {
        val const = Futil.Constant(Id(s"${d.id}-init"), Futil.FNone())
        val constComp = Futil.Component(const.id, const)
        val mem = Futil.Component(d.id, Futil.Memory2d())
        List(
          Futil.Connection(Futil.LonePort(constComp), Futil.CompPort(mem, "data-in")),
          Futil.NewComp(mem)
        )
      } else {
        throw NotImplemented("Don't yet support higher dimensional arrays.")
      }
    }
    case TBool() | TFloat() | TDouble() => {
      val const = Futil.Constant(Id(s"${d.id}-init"), Futil.FNone())
      val constComp = Futil.Component(const.id, const)
      val reg = Futil.Register()
      val regComp = Futil.Component(d.id, reg)
      List(
        Futil.Connection(Futil.LonePort(constComp), Futil.CompPort(regComp, "in")),
        Futil.NewComp(regComp)
      )
    }
    case x => throw NotImplemented(s"Type $x not implemented for decls.")
  }

  def emitBinop(name: String, prim: Futil.Primitives, e1: Expr, e2: Expr)(implicit store: Store):
      (Futil.Port, List[Futil.Structure], Futil.Activate) = {
    val (e1port, e1struct, act1) = emitExpr(e1)
    val (e2port, e2struct, act2) = emitExpr(e2)
    val comp = Futil.Component(Futil.genName(name), prim)
    val lport = Futil.CompPort(comp, "left")
    val rport = Futil.CompPort(comp, "right")
    val outport = Futil.CompPort(comp, "out")
    val struct = List(
      Futil.NewComp(comp),
      Futil.Connection(e1port, lport),
      Futil.Connection(e2port, rport))
    (outport, struct ++ e1struct ++ e2struct,
      Futil.Activate(List(comp.id)) ++ act1 ++ act2)
  }

  def emitExpr(expr: Expr, lhs: Boolean = false)(implicit store: Store):
      (Futil.Port, List[Futil.Structure], Futil.Activate) =
    expr match {
      case EInt(v, _) => {
        val const = Futil.Constant(Futil.genName("const"), Futil.FInt(v))
        val comp = Futil.Component(const.id, const)
        val port = Futil.LonePort(comp)
        (port, List(), Futil.Activate(List(const.id)))
        // (Futil.Constant(Futil.genName("const"), Futil.FInt(v)), List())
      }
      case EFloat(v) => {
        val const = Futil.Constant(Futil.genName("const"), Futil.FFloat(v))
        val comp = Futil.Component(const.id, const)
        val port = Futil.LonePort(comp)
        (port, List(), Futil.Activate(List(const.id)))
      }
      case EBinop(op, e1, e2) => {
        op.op match {
          case "+" => emitBinop("add", Futil.Add(), e1, e2)
          case "-" => emitBinop("sub", Futil.Sub(), e1, e2)
          case "*" => emitBinop("mult", Futil.Mult(), e1, e2)
          case "/" => emitBinop("div", Futil.Div(), e1, e2)
          case "<" => emitBinop("lt", Futil.TruncSub(), e2, e1)
          case ">" => emitBinop("lt", Futil.TruncSub(), e1, e2)
          case x => throw NotImplemented(s"Haven't implemented binop $x yet.")
        }
      }
      case EVar(id) => id.typ match {
        case _ => {
          val portName = if (lhs) "in" else "out"
          (Futil.CustomPort(s"${store(id)}", portName), List(),
            Futil.Activate(List(store(id))))
        }
          // case Some(TIndex(_, _)) => (Futil.CustomPort(s"$id", "out"), List()) // XXX(sam) maybe should be diff
          // case Some(_) => (Futil.CustomPort(s"$id", "out"), List())
          // case N
          // case None => throw Impossible(s"This one is definitely impossible: $expr")
      }
      case EArrAccess(id, idxs) => idxs match {
        case List(e) => {
          val (port, struct, act) = emitExpr(e)
          val gadget = Futil.Component(Futil.genName("gad"), Futil.Ident())
          val gadgetIn = Futil.CompPort(gadget, "in")
          val gadgetOut = Futil.CompPort(gadget, "out")
          val addrStruct = List(
            Futil.NewComp(gadget),
            Futil.Connection(port, gadgetIn),
            Futil.Connection(gadgetOut, Futil.CustomPort(s"$id", "addr"))
          )

          if (lhs) {
            val writeEn = Futil.Component(Futil.genName(s"$id-we"), Futil.Ident())
            val writeIn = Futil.CompPort(writeEn, "in")
            val writeOut = Futil.CompPort(writeEn, "out")
            val writeStruct = List(
              Futil.NewComp(writeEn),
              Futil.Connection(writeOut, Futil.CustomPort(s"$id", "data-in")))
            (writeIn, struct ++ addrStruct ++ writeStruct,
              Futil.Activate(List(id, gadget.id, writeEn.id)) ++ act)
          } else {
            (Futil.CustomPort(s"$id", "out"), struct ++ addrStruct,
              Futil.Activate(List(id, gadget.id)) ++ act)
          }
        }
        case List(e1, e2) => {
          val (port1, struct1, act1) = emitExpr(e1)
          val (port2, struct2, act2) = emitExpr(e2)
          val gadget1 = Futil.Component(Futil.genName("gad"), Futil.Ident())
          val gadgetIn1 = Futil.CompPort(gadget1, "in")
          val gadgetOut1 = Futil.CompPort(gadget1, "out")
          val gadget2 = Futil.Component(Futil.genName("gad"), Futil.Ident())
          val gadgetIn2 = Futil.CompPort(gadget2, "in")
          val gadgetOut2 = Futil.CompPort(gadget2, "out")
          val addrStruct = List(
            Futil.NewComp(gadget1),
            Futil.NewComp(gadget2),
            Futil.Connection(port1, gadgetIn1),
            Futil.Connection(gadgetOut1, Futil.CustomPort(s"$id", "addr1")),
            Futil.Connection(port2, gadgetIn2),
            Futil.Connection(gadgetOut2, Futil.CustomPort(s"$id", "addr2")))
          val outPortName = if (lhs) "data-in" else "out"
          (Futil.CustomPort(s"$id", outPortName), struct1 ++ struct2 ++ addrStruct,
            Futil.Activate(List(id, gadget1.id, gadget2.id)) ++ act1 ++ act2)
        }
        case _ => throw NotImplemented("Haven't done larger arrays yet. Sorry :(")
      }
      case ECast(e, _) => emitExpr(e)
      case EApp(Id("sqrt"), List(e)) => {
        val (port, struct, act) = emitExpr(e)
        val sqrtComp = Futil.Component(Futil.genName("sqrt"), Futil.Sqrt())
        val sqrtIn = Futil.CompPort(sqrtComp, "in")
        val sqrtOut = Futil.CompPort(sqrtComp, "out")
        val sqrtStruct = List(
          Futil.NewComp(sqrtComp),
          Futil.Connection(port, sqrtIn))
        (sqrtOut, struct ++ sqrtStruct, Futil.Activate(List(sqrtComp.id)) ++ act)
      }
      case x => throw NotImplemented(s"No case for $x yet")
    }

  def emitCmd(c: Command)(implicit store: Store): (List[Futil.Structure], Futil.Control, Store) =
    c match {
      case CPar(c1, c2) => {
        val (struct1, con1, s1) = emitCmd(c1)
        val (struct2, con2, s2) = emitCmd(c2)(s1)
        val control = Futil.ParComp(con1, con2)
        // (con1, con2) match {
        //   case (a1@Futil.Activate(_), a2@Futil.Activate(_)) => a1 ++ a2
        //   case _ => Futil.ParComp(con1, con2)
        // }
        (struct1 ++ struct2, control, s2)
      }
      case CSeq(c1, c2) => {
        val (struct1, con1, s1) = emitCmd(c1)
        val (struct2, con2, s2) = emitCmd(c2)(s1)
        (struct1 ++ struct2, Futil.SeqComp(con1, con2), s2)
      }
      case CLet(id, _, Some(e)) => {
        val reg = Futil.Component(Futil.genName(s"$id"), Futil.Register())
        val (port, exStruct, acts) = emitExpr(e)(store)
        val struct = List(
          Futil.NewComp(reg),
          Futil.Connection(port, Futil.CompPort(reg, "in"))) ++ exStruct
        (struct, Futil.Activate(List(reg.id)) ++ acts, store + (id -> reg.id))
      }
      case CLet(id, _, None) => {
        val reg = Futil.Component(Futil.genName(s"$id"), Futil.Register())
        val struct = List(
          Futil.NewComp(reg))
        (struct, Futil.NOP(), store + (id -> reg.id))
      }
      case CUpdate(lhs, rhs) => {
        val (lPort, lexStruct, lActs) = emitExpr(lhs, true)(store)
        val (rPort, rexStruct, rActs) = emitExpr(rhs)(store)
        val struct = lexStruct ++ rexStruct ++ List(Futil.Connection(rPort, lPort))
        val control = lActs ++ rActs
        (struct, control, store)
      }
      case CFor(range, par, comb) => range match {
        case CRange(id, start, end, 1) => {
          val iter = Futil.Component(Futil.genName(s"$id"), Futil.Iterator())

          val iterStart = Futil.Constant(Id(s"${iter.id}-start"), Futil.FInt(start))
          val startPort = Futil.LonePort(Futil.Component(iterStart.id, iterStart))

          val iterIncr = Futil.Constant(Id(s"${iter.id}-incr"), Futil.FInt(1))
          val incrPort = Futil.LonePort(Futil.Component(iterIncr.id, iterIncr))

          val iterEnd = Futil.Constant(Id(s"${iter.id}-end"), Futil.FInt(end))
          val endPort = Futil.LonePort(Futil.Component(iterEnd.id, iterEnd))

          val iterEn = Futil.Constant(Id(s"${iter.id}-en"), Futil.FInt(1))
          val enPort = Futil.LonePort(Futil.Component(iterEn.id, iterEn))

          val struct = List(
            Futil.NewComp(iter),
            Futil.Connection(startPort, Futil.CompPort(iter, "start")),
            Futil.Connection(incrPort, Futil.CompPort(iter, "incr")),
            Futil.Connection(endPort, Futil.CompPort(iter, "end")),
            Futil.Connection(enPort, Futil.CompPort(iter, "en")))
          val (parStruct, parCon, s1) = emitCmd(par)(store + (id -> iter.id))
          val (combStruct, combControl, _) = emitCmd(comb)(s1)
          (struct ++ parStruct ++ combStruct,
            Futil.SeqComp(
              Futil.Activate(List(iterEn.id, iterStart.id, iterIncr.id, iterEnd.id, iter.id)),   // init iter
              Futil.WhileLoop(Futil.CompPort(iter, "stop"),           // until (iter @ stop) = 0
                Futil.SeqComp(
                  parCon,                                             // body
                  Futil.SeqComp(
                    combControl,                                      // combine
                    Futil.Activate(List(iter.id, iterEn.id))          // iter++
                  )))),
          store)
        }
        case _ => throw NotImplemented("Haven't done all the iterators yet")
      }
      case CWhile(cond, body) => {
        val (port, condStruct, acts) = emitExpr(cond)(store)
        val (bodyStruct, bodyCon, _) = emitCmd(body)(store)
        val struct = bodyStruct ++ condStruct
        val control =
          Futil.SeqComp(
            acts,
            Futil.WhileLoop(port,
              Futil.SeqComp(bodyCon, acts)))
        (struct, control, store)
      }
      case CReduce(rop, lhs, rhs) => {
        val (linPort, lStruct, lAct) = emitExpr(lhs, true)
        val (loutPort, _, _) = emitExpr(lhs)
        val (rPort, rStruct, rAct) = emitExpr(rhs)
        val (ropStruct, control) = rop.op match {
          case "+=" => {
            val adder = Futil.Component(Futil.genName("comb-add"), Futil.Add())
            val leftPort = Futil.CompPort(adder, "left")
            val rightPort = Futil.CompPort(adder, "right")
            val outPort = Futil.CompPort(adder, "out")
            (List(
               Futil.NewComp(adder),
               Futil.Connection(rPort, rightPort),
               Futil.Connection(loutPort, leftPort),
               Futil.Connection(outPort, linPort)),
             Futil.Activate(List(adder.id)))
          }
          case x => throw Impossible(s"$x is not a reduce operator!")
        }
        (lStruct ++ rStruct ++ ropStruct,
         control ++ lAct ++ rAct,
         store)
      }
      // XXX(sam): obviously a hack
      case CExpr(EApp(Id("print_vec"), List(EVar(x)))) =>
        (List(),
          Futil.MemPrint(x),
          store)
      case CExpr(EApp(Id("print_vec_2d"), List(EVar(x)))) =>
        (List(),
          Futil.MemPrint(x),
          store)
      case CExpr(EApp(Id("print_vec_3d"), List(EVar(x)))) =>
        (List(),
          Futil.MemPrint(x),
          store)
      case CEmpty => (List(), Futil.Activate(List()), store)
      case x => throw NotImplemented(s"No case for $x yet")
    }

  def emitProg(p: Prog, c: Config): String = {
    val _ = c
    val declStruct = p.decls.map(x => emitDecl(x)).foldLeft(List[Futil.Structure]())(_ ++ _)
    val store = declStruct.foldLeft(Map[Id, Id]())((store, struct) => struct match {
      case Futil.NewComp(comp) => store + (comp.id -> comp.id)
      case _ => store
    })
    val (cmdStruct, control, _) = emitCmd(p.cmd)(store)
    val struct = Futil.sortStructure(declStruct ++ cmdStruct).distinct
    Futil.program("main", struct, control)
  }
}

case object FutilBackend extends Backend {
  def emitProg(p: Prog, c: Config) = {
    (new FutilBackendHelper()).emitProg(p, c)
  }
  val canGenerateHeader = false
}