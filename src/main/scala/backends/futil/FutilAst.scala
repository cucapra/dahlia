package fuselang.backend.futil

import fuselang.backend.PrettyPrint.Doc
import Doc._

object Futil {
  sealed trait Emitable {
    def doc(): Doc
    def emit(): String = this.doc.pretty
  }

  case class CompVar(name: String) extends Emitable {
    override def doc(): Doc = text(name)
    def port(port: String): CompPort = CompPort(this, port)
    def addSuffix(suffix: String): CompVar = CompVar(s"$name$suffix")
  }
  case class PortDef(id: CompVar, width: Int) extends Emitable {
    override def doc(): Doc = parens(text("port") <+> id.doc <+> value(width))
  }

  /**** definition statements *****/
  case class Namespace(name: String, comps: List[Component]) extends Emitable {
    override def doc(): Doc =
      parens(
        text("define/namespace") <+> text(name)
          <@> nest(vsep(comps.map(_.doc)), 2))
  }
  case class Component(
      name: String,
      inputs: List[PortDef],
      outputs: List[PortDef],
      structure: List[Structure],
      control: Control
  ) extends Emitable {
    override def doc(): Doc =
      parens(
        text("define/component")
          <+> text(name)
          <+> parens(hsep(inputs.map(_.doc)))
          <+> parens(hsep(outputs.map(_.doc)))
          <@> parens(vsep(structure.map(_.doc)))
          <@> control.doc)
  }

  /***** structure *****/
  sealed trait Port extends Emitable {
    override def doc(): Doc = this match {
      case CompPort(id, name) =>
        parens(text("@") <+> id.doc <+> text(name))
      case ThisPort(id) =>
        parens(text("@") <+> text("this") <+> id.doc)
    }
  }

  case class CompPort(id: CompVar, name: String) extends Port
  case class ThisPort(id: CompVar) extends Port

  sealed trait Structure extends Emitable {
    override def doc(): Doc = this match {
      case CompDecl(id, comp) =>
        brackets(text("new") <+> id.doc <+> comp.doc)
      case LibDecl(id, ci) =>
        brackets(text("new-std") <+> id.doc <+> ci.doc)
      case Connect(src, dest) =>
        brackets(text("->") <+> src.doc <+> dest.doc)
    }
  }
  case class CompDecl(id: CompVar, comp: CompVar) extends Structure
  case class LibDecl(id: CompVar, ci: CompInst) extends Structure
  case class Connect(src: Port, dest: Port) extends Structure

  case class CompInst(id: String, args: List[Value]) extends Emitable {
    override def doc(): Doc =
      parens(text(id) <+> hsep(args.map(_.doc)))
  }

  sealed trait Value extends Emitable {
    override def doc(): Doc = this match {
      case Num(v)  => value(v)
      case VNone() => text("#f")
    }
  }
  case class Num(v: Any) extends Value
  case class VNone() extends Value

  /***** control *****/
  sealed trait Control extends Emitable {

    def seq(c: Control): Control = (this, c) match {
      case (seq0: SeqComp, seq1: SeqComp) => SeqComp(seq0.stmts ++ seq1.stmts)
      case (seq: SeqComp, _)              => SeqComp(seq.stmts ++ List(c))
      case (_, seq: SeqComp)              => SeqComp(this :: seq.stmts)
      case _                              => SeqComp(List(this, c))
    }

    def par(c: Control): Control = (this, c) match {
      case (par0: ParComp, par1: ParComp) => ParComp(par0.stmts ++ par1.stmts)
      case (par0: ParComp, par1)          => ParComp(par0.stmts ++ List(par1))
      case (par0, par1: ParComp)          => ParComp(par0 :: par1.stmts)
      case _                              => ParComp(List(this, c))
    }
    override def doc(): Doc = this match {
      case SeqComp(stmts) =>
        nest(parens(text("seq") <@> vsep(stmts.map(_.doc))), 1)
      case ParComp(stmts) =>
        nest(parens(text("par") <@> vsep(stmts.map(_.doc))), 1)
      case If(cond, trueBr, falseBr) =>
        parens(
          text("if") <+> cond.doc
            <@> nest(trueBr.doc, 4)
            <@> nest(falseBr.doc, 4))
      case Ifen(cond, trueBr, falseBr) =>
        parens(
          text("ifen") <+> cond.doc
            <@> nest(trueBr.doc, 6)
            <@> nest(falseBr.doc, 6))
      case While(cond, body) =>
        parens(text("while") <+> cond.doc <@> nest(body.doc, 2))
      case Print(id) =>
        parens(text("print") <+> id.doc)
      case Enable(ids) =>
        parens(text("enable") <+> hsep(ids.map(_.doc)))
      case Disable(ids) =>
        parens(text("disable") <+> hsep(ids.map(_.doc)))
      case Empty() =>
        parens(text("empty"))
    }
  }
  case class SeqComp(stmts: List[Control]) extends Control
  case class ParComp(stmts: List[Control]) extends Control
  case class If(cond: Port, trueBr: Control, falseBr: Control) extends Control
  case class Ifen(cond: Port, trueBr: Control, falseBr: Control) extends Control
  case class While(cond: Port, body: Control) extends Control
  case class Print(id: CompVar) extends Control
  case class Enable(ids: List[CompVar]) extends Control {

    def ++(en: Enable): Enable =
      Enable((ids ++ en.ids).distinct)
  }
  case class Disable(ids: List[CompVar]) extends Control
  case class Empty() extends Control
}

object Stdlib {

  def constant(v: Futil.Value): Futil.CompInst =
    Futil.CompInst("const", List(v))

  def constant(v: String): Futil.CompInst =
    Futil.CompInst("const", List(Futil.Num(v)))

  def memory(dims: List[Int]): Futil.CompInst =
    Futil.CompInst("memory", dims.map(Futil.Num(_)))

  def register(): Futil.CompInst =
    Futil.CompInst("comp/reg", List())

  def op(op: String): Futil.CompInst =
    Futil.CompInst(op, List())

  def identity(): Futil.CompInst =
    Futil.CompInst("comp/id", List())

  def sqrt(): Futil.CompInst =
    Futil.CompInst("comp/sqrt", List())

  def iterator(): Futil.CompInst =
    Futil.CompInst("comp/iterator", List())

}
