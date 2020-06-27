package fuselang.backend.futil

import fuselang.common.PrettyPrint.Doc
import fuselang.common.CompilerError._
import Doc._

object Futil {

  def emitCompStructure(structs: List[Structure]): Doc = {
    val (cells, connections) = structs.partition(st =>
      st match {
        case _: LibDecl | _: CompDecl => true
        case _ => false
      }
    )
    text("cells") <+> scope(vsep(cells.map(_.doc))) <@>
      text("wires") <+> scope(vsep(connections.map(_.doc)))
  }

  sealed trait Emitable {
    def doc(): Doc
    def emit(): String = this.doc.pretty
  }

  case class CompVar(name: String) extends Emitable with Ordered[CompVar] {
    override def doc(): Doc = text(name)
    def port(port: String): CompPort = CompPort(this, port)
    def addSuffix(suffix: String): CompVar = CompVar(s"$name$suffix")
    override def compare(that: CompVar): Int = {
      this.name.compare(that.name)
    }
  }
  case class PortDef(id: CompVar, width: Int) extends Emitable {
    override def doc(): Doc = parens(text("port") <+> id.doc <+> value(width))
  }

  /**** definition statements *****/
  case class Namespace(name: String, comps: List[NamespaceStatement])
      extends Emitable {
    override def doc(): Doc =
      vsep(comps.map(_.doc))
  }

  /** The statements that can appear in a `define/namespace` construct. */
  sealed trait NamespaceStatement extends Emitable {
    override def doc(): Doc = this match {
      case Import(filename) => text("import") <+> quote(text(filename)) <> semi
      case Component(name, inputs, outputs, structure, control) => {
        text("component") <+>
          text(name) <>
          parens(hsep(inputs.map(_.doc))) <+>
          text("->") <+>
          parens(hsep(outputs.map(_.doc))) <+>
          scope(
            emitCompStructure(structure) <@>
              text("control") <+> scope(control.doc)
          )
      }
    }
  }

  case class Import(filename: String) extends NamespaceStatement
  case class Component(
      name: String,
      inputs: List[PortDef],
      outputs: List[PortDef],
      structure: List[Structure],
      control: Control
  ) extends NamespaceStatement

  /***** structure *****/
  sealed trait Port extends Emitable with Ordered[Port] {
    override def doc(): Doc = this match {
      case CompPort(id, name) =>
        id.doc <> dot <> text(name)
      case ThisPort(id) => id.doc
    }

    override def compare(that: Port): Int = (this, that) match {
      case (ThisPort(thisId), ThisPort(thatId)) => thisId.compare(thatId)
      case (ThisPort(_), _) => 1
      case (_, ThisPort(_)) => -1
      case (CompPort(thisId, _), CompPort(thatId, _)) => thisId.compare(thatId)
    }
  }

  case class CompPort(id: CompVar, name: String) extends Port
  case class ThisPort(id: CompVar) extends Port

  sealed trait Structure extends Emitable with Ordered[Structure] {
    override def doc(): Doc = this match {
      case CompDecl(id, comp) =>
        id.doc <+> equal <+> comp.doc <> semi
      case LibDecl(id, comp) =>
        id.doc <+> equal <+> text("prim") <+> comp.doc <> semi
      case Connect(src, dest) =>
        dest.doc <+> equal <+> src.doc <> semi
      case Group(id, conns) =>
        text("group") <+> id.doc <+>
          scope(vsep(conns.map(_.doc)))
    }

    def compare(that: Structure): Int = {
      (this, that) match {
        case (LibDecl(thisId, _), LibDecl(thatId, _)) => thisId.compare(thatId)
        case (CompDecl(thisId, _), CompDecl(thatId, _)) =>
          thisId.compare(thatId)
        case (Group(thisId, _), Group(thatId, _)) => thisId.compare(thatId)
        case (Connect(thisSrc, thisDest), Connect(thatSrc, thatDest)) => {
          if (thisSrc.compare(thatSrc) == 0) {
            thisDest.compare(thatDest)
          } else {
            thisSrc.compare(thatSrc)
          }
        }
        case (LibDecl(_, _), _) => -1
        case (_, LibDecl(_, _)) => 1
        case (CompDecl(_, _), _) => -1
        case (_, CompDecl(_, _)) => 1
        case (Group(_, _), _) => -1
        case (_, Group(_, _)) => 1
      }
    }
  }
  case class LibDecl(id: CompVar, ci: CompInst) extends Structure
  case class CompDecl(id: CompVar, comp: CompVar) extends Structure
  case class Group(id: CompVar, connections: List[Connect]) extends Structure
  object Group {
    def fromStructure(
        id: CompVar,
        structure: List[Structure]
    ): (Group, List[Structure]) = {
      val (connections, st) = structure.partition(st =>
        st match {
          case _: Connect => true
          case _ => false
        }
      )
      (this(id, connections.collect({ case c:Connect => c })), st)
    }
  }
  case class Connect(src: Port, dest: Port) extends Structure

  case class CompInst(id: String, args: List[Int]) extends Emitable {
    override def doc(): Doc = {
      val strList = args.map((x: Int) => text(x.toString()))
      text(id) <> parens(hsep(strList, comma))
    }
  }

  /***** control *****/
  sealed trait Control extends Emitable {
    def seq(c: Control): Control = (this, c) match {
      case (seq0: SeqComp, seq1: SeqComp) => SeqComp(seq0.stmts ++ seq1.stmts)
      case (seq: SeqComp, _) => SeqComp(seq.stmts ++ List(c))
      case (_, seq: SeqComp) => SeqComp(this :: seq.stmts)
      case _ => SeqComp(List(this, c))
    }

    def par(c: Control): Control = (this, c) match {
      case (par0: ParComp, par1: ParComp) => ParComp(par0.stmts ++ par1.stmts)
      case (par0: ParComp, par1) => ParComp(par0.stmts ++ List(par1))
      case (par0, par1: ParComp) => ParComp(par0 :: par1.stmts)
      case _ => ParComp(List(this, c))
    }
    override def doc(): Doc = this match {
      case SeqComp(stmts) =>
        text("seq") <+> scope(vsep(stmts.map(_.doc)))
      case ParComp(stmts) =>
        text("par") <+> scope(vsep(stmts.map(_.doc)))
      case If(port, cond, trueBr, falseBr) =>
        text("if") <+> port.doc <+> text("with") <+>
          cond.doc <+>
          scope(trueBr.doc) <+>
          text("else") <+>
          scope(falseBr.doc)
      case While(port, cond, body) =>
        text("while") <+> port.doc <+> text("with") <+>
          cond.doc <+>
          scope(body.doc)
      case Print(_) =>
        throw Impossible("Futil does not support print")
      case Enable(id) => id.doc <> semi
      case Empty() => text("empty")
    }
  }
  case class SeqComp(stmts: List[Control]) extends Control
  case class ParComp(stmts: List[Control]) extends Control
  case class If(port: Port, cond: CompVar, trueBr: Control, falseBr: Control)
      extends Control
  case class While(port: Port, cond: CompVar, body: Control) extends Control
  case class Print(id: CompVar) extends Control
  case class Enable(id: CompVar) extends Control
  case class Empty() extends Control
}

/** Represents all of the primitives in Futil. */
object Stdlib {
  def register(bitwidth: Int): Futil.CompInst =
    Futil.CompInst("std_reg", List(bitwidth))

  def iterator(bitwidth: Int, start: Int, end: Int, incr: Int): Futil.CompInst =
    Futil.CompInst("std_iterator", List(bitwidth, start, end, incr))

  def constant(bitwidth: Int, v: Int): Futil.CompInst =
    Futil.CompInst("std_const", List(bitwidth, v))

  def op(op: String, bitwidth: Int): Futil.CompInst =
    Futil.CompInst(s"std_$op", List(bitwidth))

  def identity(bitwidth: Int): Futil.CompInst =
    Futil.CompInst(s"std_id", List(bitwidth))

  def memory(dims: List[Int]): Futil.CompInst =
    Futil.CompInst("std_mem", dims)

  def sqrt(): Futil.CompInst =
    Futil.CompInst("std_sqrt", List())
}
