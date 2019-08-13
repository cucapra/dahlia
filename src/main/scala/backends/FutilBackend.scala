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
  case class CounterUp() extends Primitives {
    override def toString = "comp/counter-up"
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
  case class Mult() extends Primitives {
    override def toString = "comp/mult"
  }
  case class Div() extends Primitives {
    override def toString = "comp/div"
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
    override def toString(): String = s"$c1 $c2"
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
  case class WhileLoop(condition: (Component, String), body: SeqComp) extends Control {
    override def toString(): String = {
      val (comp, port) = condition
      s"(while (${comp.id} $port)\n($body))"
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
    val imports = List("\"../src/futil.rkt\"", "\"../src/visualizer.rkt\"")
    val importStr = imports.mkString("(require ", " ", ")")

    val structStr = struct.map(x => x.toString()).mkString("\n")

    val body = s"(define/module $name () ()\n ($structStr)\n$control)"
    val execute = "(void (compute (main) '() #:memory (json->memory (benchmark-data-path))))"
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
          Futil.NewComp(constComp),
          Futil.Connection(Futil.LonePort(constComp), Futil.CompPort(mem, "data-in")),
          Futil.NewComp(mem)
        )
      } else {
        throw NotImplemented("Don't yet support higher dimensional arrays.")
      }
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
          case x => throw NotImplemented(s"Haven't implemented binop $x yet")
        }
      }
      case EVar(id) => id.typ match {
        case _ =>
          (Futil.CustomPort(s"${store(id)}", "out"), List(),
            Futil.Activate(List(store(id))))
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
          val outPortName = if (lhs) "data-in" else "out"
          (Futil.CustomPort(s"$id", outPortName), struct ++ addrStruct,
            Futil.Activate(List(id, gadget.id)) ++ act)
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
      case x => throw NotImplemented(s"No case for $x yet")
    }

  def emitCmd(c: Command)(store: Store): (List[Futil.Structure], Futil.Control, Store) =
    c match {
      case CPar(c1, c2) => {
        val (struct1, con1, s1) = emitCmd(c1)(store)
        val (struct2, con2, s2) = emitCmd(c2)(s1)
        val control = (con1, con2) match {
          case (a1@Futil.Activate(_), a2@Futil.Activate(_)) => a1 ++ a2
          case _ => Futil.ParComp(con1, con2)
        }
        (struct1 ++ struct2, control, s2)
      }
      case CSeq(c1, c2) => {
        val (struct1, con1, s1) = emitCmd(c1)(store)
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
      case CFor(range, par, combine@_) => range match {
        case CRange(id, 0, end, 1) => {
          val iter = Futil.Component(Futil.genName(s"$id"), Futil.CounterUp())
          val iterEn = Futil.Constant(Id(s"${iter.id}-en"), Futil.FInt(1))
          val enPort = Futil.LonePort(Futil.Component(iterEn.id, iterEn))
          val iterData = Futil.Constant(Id(s"${iter.id}-data"), Futil.FInt(end))
          val dataPort = Futil.LonePort(Futil.Component(iterData.id, iterData))
          val struct = List(
            Futil.NewComp(iter),
            Futil.Connection(enPort, Futil.CompPort(iter, "en")),
            Futil.Connection(dataPort, Futil.CompPort(iter, "in")))
          val (parStruct, parCon, _) = emitCmd(par)(store + (id -> iter.id))
          (struct ++ parStruct,
            Futil.SeqComp(
              Futil.Activate(List(iterEn.id, iterData.id, iter.id)),  // init iter
              Futil.WhileLoop((iter, "stop"),                         // until (iter stop) = 0
                Futil.SeqComp(
                  parCon,                                             // body
                  Futil.Activate(List(iter.id, iterEn.id))))),        // iter++
          store)
        }
        case _ => throw NotImplemented("Haven't done all the iterators yet")
          // emitCmdStructure(par)
      }
      // XXX(sam): obviously a hack
      case CExpr(EApp(Id("print_float_vec"), List(EVar(x)))) =>
        (List(),
          Futil.MemPrint(x),
          store)
      case x => throw NotImplemented(s"No case for $x yet")
    }

  def emitProg(p: Prog, c: Config): String = {
    val _ = c
    val declStruct = p.decls.map(x => emitDecl(x)).foldLeft(List[Futil.Structure]())(_ ++ _)
    val (cmdStruct, control, _) = emitCmd(p.cmd)(Map())
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
