package fuselang.backend.futil

import fuselang.common.PrettyPrint.Doc
import Doc._

object Futil {

  def brackets(d: Doc) =
    enclose(text("["), d, text("]"))

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
          <> nest(emptyDoc <@> vsep(comps.map(_.doc)), 2)
      )
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
          <> nest(
            emptyDoc
              <@> nest(parens(vsep(structure.map(_.doc))), 1)
              <@> control.doc,
            2
          )
      )
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

  case class CompInst(id: String, args: List[Int]) extends Emitable {
    override def doc(): Doc = {
      val strList = args.map((x: Int) => text(x.toString()))
      parens(text(id) <+> hsep(strList))
    }
  }

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
      case If(port, cond, trueBr, falseBr) =>
        parens(
          text("if") <+> port.doc <+> parens(hsep(cond.map(_.doc)))
            <> nest(emptyDoc <@> trueBr.doc, 4)
            <> nest(emptyDoc <@> falseBr.doc, 4)
        )
      case While(port, cond, body) =>
        parens(
          text("while") <+> port.doc <+> parens(hsep(cond.map(_.doc)))
            <> nest(emptyDoc <@> body.doc, 2)
        )
      case Print(id) =>
        parens(text("print") <+> id.doc)
      case Enable(ids) =>
        parens(text("enable") <+> hsep(ids.map(_.doc)))
      case Empty() =>
        parens(text("empty"))
    }
  }
  case class SeqComp(stmts: List[Control]) extends Control
  case class ParComp(stmts: List[Control]) extends Control
  case class If(
      port: Port,
      cond: List[CompVar],
      trueBr: Control,
      falseBr: Control
  ) extends Control
  case class While(port: Port, cond: List[CompVar], body: Control)
      extends Control
  case class Print(id: CompVar) extends Control
  case class Enable(ids: List[CompVar]) extends Control {

    def ++(en: Enable): Enable =
      Enable((ids ++ en.ids).distinct)
  }
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

  def add(bitwidth: Int): Futil.CompInst =
    Stdlib.op("add", bitwidth)

  def op(op: String, bitwidth: Int): Futil.CompInst =
    Futil.CompInst(s"std_$op", List(bitwidth))

  def identity(bitwidth: Int): Futil.CompInst =
    Futil.CompInst(s"std_id", List(bitwidth))

  def memory(dims: List[Int]): Futil.CompInst =
    Futil.CompInst("std_mem", dims)

  def sqrt(): Futil.CompInst =
    Futil.CompInst("std_sqrt", List())

}
